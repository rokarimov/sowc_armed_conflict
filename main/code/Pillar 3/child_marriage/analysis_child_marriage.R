# Project: SOWC 2026
# Goal: Analyze histrical child marriage patterns inside and ouside armed conflict zones
# Author: Rouslan Karimov
# Organization: UNICEF
# Created: May 1, 2026


library(here)
library(tidyverse)
library(sf)
library(haven)
library(countrycode)
library(survey)

# Load data
buffers <- readRDS(here("data_derived", "ged_country_year_buffers.rds"))


# DHS ====

# Survey metadata
ccode <- "MLI"

# Get survey cluster locations
clusters <- st_read(
  here("data", "Mali_2023-24_DHS", "MLGE8AFL - Geographic Data", "MLGE8AFL.shp"),
  quiet = TRUE
  ) |>
  mutate(iso3 = countrycode(DHSCC, origin = "iso2c", destination = "iso3c")) |>
  filter(SOURCE != "MIS", LATNUM != 0, LONGNUM != 0) |>
  st_transform(crs = 4326)

# Get DHS Individual (IR) dataset
ir <- read_dta(here("data", "Mali_2023-24_DHS", "MLIR8ADT - Individual", "MLIR8AFL.dta"))

# We look at women 18-49 y.o., to determine incidence of child marriage (CM).
# Algorithm: for each woman, figure out when she was aged 0-17. Check how many times
# out of that period her cluster was inside a (conflict buffer)/(year) combination.
# Add those up: that's her relevant conflict exposure. Age variable: v012. Marriage
# status variable: v501 (0 = never married).

# Precompute cluster x buffer-year intersection matrix
buffers_ccode <- buffers |> filter(iso3 == ccode)
hits <- st_intersects(clusters, buffers_ccode, sparse = FALSE)
buffer_years <- buffers_ccode$year

CalcConflExpDHS <- function(v001, v010, v012) {
  if (is.na(v012) || v012 < 18 || v012 >= 50) return(NA_integer_)
  if (is.na(v010)) return(NA_integer_)
  clust_row <- match(v001, clusters$DHSCLUST)
  if (is.na(clust_row)) return(NA_integer_)
  sum(hits[clust_row, ] & buffer_years %in% v010:(v010 + 17))
}

# Build conflict exposure metric for each observation in the dataset.
ir$conflict_exposure <- purrr::pmap_int(
  list(v001 = as.numeric(ir$v001),
       v010 = as.numeric(ir$v010),
       v012 = as.numeric(ir$v012)),
  CalcConflExpDHS
)

ir$exposed <- ir$conflict_exposure > 0


# Analysis
ir$wgt  <- ir$v005 / 1000000
ir <- ir |>
  mutate(
    chmarry = case_when(   # Child marriage rate. Based on https://github.com/DHSProgram/DHS-Indicators-R/blob/main/Chap04_MS/MS_MAR.R
      v511 >= 0 & v511 < 18 ~ 1,
      .default              = 0
    ),
    awfactt = awfactt / 100   # AWFACTT is the all-women correction factor; DHS recodes store it ×100.
  )

ir_design <- svydesign(
  id = ~v021,
  strata = ~v022,
  weights = ~wgt,
  data = ir,
  nest = TRUE
)


design_20_24 <- subset(ir_design, v012 >= 20 & v012 <= 24)  # 20-24 cohort matches the standard DHS published indicator MA_MBAY_W_B18
design_18_49 <- subset(ir_design, v012 >= 18 & v012 <= 49)  # 18-49 y.o. is comparison cohort for the in-zone vs out-zone test

# Calculate child marriage rates for the 20-24 years old group.
# NB: for all-women surveys (Mali 2023-24 included), awfactt == 1,
# so this collapses to a regular weighted mean.
svyratio(~chmarry, ~awfactt, design = design_20_24, na.rm = TRUE)

# Get same value published by DHS from their API, for comparison. Should match with above.
jsonlite::fromJSON(paste0(
  "https://api.dhsprogram.com/rest/dhs/data",
  "?indicatorIds=MA_MBAY_W_B18&countryIds=ML&surveyIds=ML2023DHS",
  "&breakdown=national&f=json",
  "&apiKey=", Sys.getenv("DHS_API_KEY")
))$Data$Value

svyby(~chmarry, by = ~exposed, design = design_18_49,
      FUN = svyratio, denominator = ~awfactt, na.rm = TRUE)
svyttest(chmarry ~ exposed, design = design_18_49)


# MICS ====

# Survey metadata
ccode <- "NGA"

# Get survey cluster locations
clusters <- st_read(
  here("data", "Nigeria_2021_MICS", "Geographic Data", "NigeriaMICS2021GPS.shp"),
  quiet = TRUE
) |> st_transform(crs = 4326)

# Get MICS Women (wm) dataset
wm <- read_sav(here("data", "Nigeria_2021_MICS", "SPSS Datasets", "wm.sav"))

# Calculate conflict exposure in childhood years, as for DHS above
# Precompute cluster x buffer-year intersection matrix
buffers_ccode <- buffers |> filter(iso3 == ccode)
hits <- st_intersects(clusters, buffers_ccode, sparse = FALSE)
buffer_years <- buffers_ccode$year

CalcConflExpMICS <- function(WM1, WB3Y, WB4) {
  if (is.na(WB4) || WB4 < 18 || WB4 >= 50) return(NA_integer_)
  if (is.na(WB3Y)) return(NA_integer_)
  clust_row <- match(WM1, clusters$HH1)
  if (is.na(clust_row)) return(NA_integer_)
  sum(hits[clust_row, ] & buffer_years %in% WB3Y:(WB3Y + 17))
}


# Build conflict exposure metric for each observation in the dataset.
wm$conflict_exposure <- purrr::pmap_int(
  list(WM1  = as.numeric(wm$WM1),
       WB3Y = as.numeric(wm$WB3Y),
       WB4  = as.numeric(wm$WB4)),
  CalcConflExpMICS
)

wm$exposed <- wm$conflict_exposure > 0


# Analysis

# MICS samples all women in selected households by design, so no ever-married
# sample correction is needed
wm$wgt  <- wm$wmweight
wm <- wm |>
  mutate(
    chmarry = case_when(
      WAGEM >= 0 & WAGEM < 18 ~ 1,
      .default                = 0
    )
  )

wm_design <- svydesign(
  id = ~HH2,
  strata = ~stratum,
  weights = ~wgt,
  data = wm,
  nest = TRUE
)

design_20_24 <- subset(wm_design, WB4 >= 20 & WB4 <= 24)
design_18_49 <- subset(wm_design, WB4 >= 18 & WB4 <= 49)

svymean(~chmarry, design = design_20_24, na.rm = TRUE)
svyby(~chmarry, by = ~exposed, design = design_18_49, FUN = svymean, na.rm = TRUE)
svyttest(chmarry ~ exposed, design = design_18_49)
