# Project: SOWC 2026
# Goal: Analyze histrical child marriage patterns inside and ouside armed conflict zones
# Author: Rouslan Karimov
# Organization: UNICEF
# Created: 11 May 2026

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

# Get household member data
pr <- read_dta(here("data", "Mali_2023-24_DHS", "MLPR8ADT - Household Member", "MLPR8AFL.dta"))

# We calculate stunting and wasting in children under 5.
# Algorithm: Determine the age of each child. Check how many years out of the time
# the child has been alive, her cluster was inside a (conflict buffer)/(year) combination.
# Add those up: that's the child's relevant conflict exposure. Variables: hc1 (age in months,
# is  NA for respondents 5 and older), hv007 - year of household interview.

# Precompute cluster x buffer-year intersection matrix
buffers_ccode <- buffers |> filter(iso3 == ccode)
hits <- st_intersects(clusters, buffers_ccode, sparse = FALSE)
buffer_years <- buffers_ccode$year

CalcConflExpDHS <- function(hv001, hv007, hc1) {
  age <- round(hc1 / 12, 0)
  if (is.na(hc1)) return(NA_integer_)
  if (is.na(hv007)) return(NA_integer_)
  clust_row <- match(hv001, clusters$DHSCLUST)
  if (is.na(clust_row)) return(NA_integer_)
  sum(hits[clust_row, ] & buffer_years %in% (hv007 - age):hv007)
}

# Build conflict exposure metric for each observation in the dataset.
pr$conflict_exposure <- purrr::pmap_int(
  list(hv001 = as.numeric(pr$hv001),
       hv007 = as.numeric(pr$hv007),
       hc1 = as.numeric(pr$hc1)),
  CalcConflExpDHS
)

pr$exposed <- pr$conflict_exposure > 0
pr$exposed3 <- pr$conflict_exposure > 2
pr$exposed5 <- pr$conflict_exposure > 4


# Analysis
pr$wgt <- pr$hv005 / 1000000

# Create wasting and stunting indicators. 
# Based on https://github.com/DHSProgram/DHS-Indicators-R/blob/main/Chap11_NT/NT_CH_NUT.R.
pr <- pr |>
  mutate(
    stunt = case_when(
      hv103 == 1 &  hc70 < -200                ~ 1,
      hv103 == 1 &  hc70 >= -200 & hc70 < 9996 ~ 0,
      hc70 >= 9996                             ~ NA_integer_),
    sev_stunt = case_when(
      hv103 == 1 &  hc70 < -300               ~ 1,
      hv103 == 1 &  hc70>= -300 & hc70 < 9996 ~ 0,
      hc70 >= 9996                            ~ NA_integer_),
    wast =
      case_when(
        hv103 == 1 &  hc72 < -200                ~ 1,
        hv103 == 1 &  hc72 >= -200 & hc72 < 9996 ~ 0,
        hc72 >= 9996                             ~ NA_integer_),
    sev_wast =
      case_when(
        hv103 == 1 &  hc72 < -300                ~ 1,
        hv103 == 1 &  hc72 >= -300 & hc72 < 9996 ~ 0,
        hc72 >= 9996                             ~ NA_integer_)
  )

pr_design <- svydesign(
  id = ~hv021,
  strata = ~hv022,
  weights = ~wgt,
  data = pr,
  nest = TRUE
)

design_under_5 <- subset(pr_design, hc1 >= 0 & hc1 <= 59)  # 0-59 months old cohort matches the standard DHS published indicator CN_NUTS_C_HA2


# STUNTING

# Calculate stunting prevalence for children under 5.
svymean(~stunt, design = design_under_5, na.rm = TRUE)

# Get same value published by DHS from their API, for comparison. Should match with above.
jsonlite::fromJSON(paste0(
  "https://api.dhsprogram.com/rest/dhs/data",
  "?indicatorIds=CN_NUTS_C_HA2&countryIds=ML&surveyIds=ML2023DHS",
  "&breakdown=national&f=json",
  "&apiKey=", Sys.getenv("DHS_API_KEY")
))$Data$Value

svyby(~stunt, by = ~exposed, design = design_under_5, FUN = svymean, na.rm = TRUE)
svyttest(stunt ~ exposed, design = design_under_5)

svyby(~stunt, by = ~exposed3, design = design_under_5, FUN = svymean, na.rm = TRUE)
svyttest(stunt ~ exposed3, design = design_under_5)

svyby(~stunt, by = ~exposed5, design = design_under_5, FUN = svymean, na.rm = TRUE)
svyttest(stunt ~ exposed5, design = design_under_5)


# WASTING

# Calculate wasting prevalence for children under 5.
svymean(~wast, design = design_under_5, na.rm = TRUE)

# Get same value published by DHS from their API, for comparison. Should match with above.
jsonlite::fromJSON(paste0(
  "https://api.dhsprogram.com/rest/dhs/data",
  "?indicatorIds=CN_NUTS_C_WH2&countryIds=ML&surveyIds=ML2023DHS",
  "&breakdown=national&f=json",
  "&apiKey=", Sys.getenv("DHS_API_KEY")
))$Data$Value

svyby(~wast, by = ~exposed, design = design_under_5, FUN = svymean, na.rm = TRUE)
svyttest(wast ~ exposed, design = design_under_5)

svyby(~wast, by = ~exposed3, design = design_under_5, FUN = svymean, na.rm = TRUE)
svyttest(wast ~ exposed3, design = design_under_5)

svyby(~wast, by = ~exposed5, design = design_under_5, FUN = svymean, na.rm = TRUE)
svyttest(wast ~ exposed5, design = design_under_5)


# MICS ====

# Survey metadata
ccode <- "HND"

# Get survey cluster locations
clusters <- st_read(
  here("data", "Honudras_2019_MICS6", "GPS Datasets", "HondurasMICS2019GPS.shp"),
  quiet = TRUE
) |> st_transform(crs = 4326)

# Get MICS children-under-5 (ch) dataset — anthropometry module lives here
ch <- read_sav(here("data", "Honudras_2019_MICS6", "SPSS Datasets", "ch.sav"))

# Calculate stunting and wasting in children under 5, as for DHS above.

# Precompute cluster x buffer-year intersection matrix
buffers_ccode <- buffers |> filter(iso3 == ccode)
hits <- st_intersects(clusters, buffers_ccode, sparse = FALSE)
buffer_years <- buffers_ccode$year

# Key variables: HH1 - cluster number, UF7Y - year of interview,
# CAGE - age in months (MICS6 ch.sav equivalent of DHS hc1).
CalcConflExpMICS <- function(HH1, UF7Y, CAGE) {
  age <- round(CAGE / 12, 0)
  if (is.na(CAGE)) return(NA_integer_)
  if (is.na(UF7Y)) return(NA_integer_)
  clust_row <- match(HH1, clusters$HH1)
  if (is.na(clust_row)) return(NA_integer_)
  sum(hits[clust_row, ] & buffer_years %in% (UF7Y - age):UF7Y)
}

# Build conflict exposure metric for each observation in the dataset.
ch$conflict_exposure <- purrr::pmap_int(
  list(HH1  = as.numeric(ch$HH1),
       UF7Y = as.numeric(ch$UF7Y),
       CAGE = as.numeric(ch$CAGE)),
  CalcConflExpMICS
)

ch$exposed  <- ch$conflict_exposure > 0
ch$exposed3 <- ch$conflict_exposure > 2
ch$exposed5 <- ch$conflict_exposure > 4


# Analysis
ch$wgt <- ch$chweight

# Create wasting and stunting indicators.
# MICS6 derives z-scores into HAZ2 / WAZ2 / WHZ2 (true z-scores, not ×100 like
# DHS hc70). HAZFLAG / WHZFLAG == 1 marks biologically implausible values that
# should be dropped (NA flag = no measurement attempted).
ch <- ch |>
  mutate(
    stunt = case_when(
      HAZFLAG == 0 & HAZ2 < -2  ~ 1,
      HAZFLAG == 0 & HAZ2 >= -2 ~ 0),
    sev_stunt = case_when(
      HAZFLAG == 0 & HAZ2 < -3  ~ 1,
      HAZFLAG == 0 & HAZ2 >= -3 ~ 0),
    wast = case_when(
      WHZFLAG == 0 & WHZ2 < -2  ~ 1,
      WHZFLAG == 0 & WHZ2 >= -2 ~ 0),
    sev_wast = case_when(
      WHZFLAG == 0 & WHZ2 < -3  ~ 1,
      WHZFLAG == 0 & WHZ2 >= -3 ~ 0)
  )

ch_design <- svydesign(
  id = ~PSU,
  strata = ~stratum,
  weights = ~wgt,
  data = ch,
  nest = TRUE
)

design_under_5 <- subset(ch_design, CAGE >= 0 & CAGE <= 59)


# STUNTING

# Calculate stunting prevalence for children under 5.
# Compare against the value in the MICS Statistical Snapshot / SDG indicator
# 2.2.1 reported on data.unicef.org for this country/round.
svymean(~stunt, design = design_under_5, na.rm = TRUE)

svyby(~stunt, by = ~exposed, design = design_under_5, FUN = svymean, na.rm = TRUE)
svyttest(stunt ~ exposed, design = design_under_5)

svyby(~stunt, by = ~exposed3, design = design_under_5, FUN = svymean, na.rm = TRUE)
svyttest(stunt ~ exposed3, design = design_under_5)

svyby(~stunt, by = ~exposed5, design = design_under_5, FUN = svymean, na.rm = TRUE)
svyttest(stunt ~ exposed5, design = design_under_5)


# WASTING

# Calculate wasting prevalence for children under 5.
svymean(~wast, design = design_under_5, na.rm = TRUE)

svyby(~wast, by = ~exposed, design = design_under_5, FUN = svymean, na.rm = TRUE)
svyttest(wast ~ exposed, design = design_under_5)

svyby(~wast, by = ~exposed3, design = design_under_5, FUN = svymean, na.rm = TRUE)
svyttest(wast ~ exposed3, design = design_under_5)

svyby(~wast, by = ~exposed5, design = design_under_5, FUN = svymean, na.rm = TRUE)
svyttest(wast ~ exposed5, design = design_under_5)







