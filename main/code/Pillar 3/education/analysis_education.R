# Project: SOWC 2026
# Goal: Single-survey methodology test for net and gross school attendance
#       ratios (NAR / GAR) for primary and secondary education inside and
#       outside conflict zones. Run on raw DHS (Mali 2023-24) and raw MICS
#       (Nigeria 2021) files to validate the pipeline's NAR/GAR formulae
#       against DHS-published values and the MICS report — without IPUMS
#       harmonization in between.
# Author: Rouslan Karimov
# Organization: UNICEF
# Created: 26 May 2026


library(here)
library(tidyverse)
library(sf)
library(haven)
library(readxl)
library(countrycode)
library(survey)

options(survey.lonely.psu = "adjust")


# Load data
buffers <- readRDS(here("data_derived", "ged_country_year_buffers.rds"))

unesco <- read_excel(here("data", "UNESCO Info",
                          "school_age_calendar_info.xlsx"),
                     sheet = "Data") |>
  suppressWarnings() |>
  filter(indicatorId %in% c("299905", "999975", "999977", "403")) |>
  select(iso3 = geoUnit, year, indicatorId, value) |>
  pivot_wider(names_from = indicatorId, values_from = value,
              values_fn = mean) |>
  transmute(
    iso3,
    year            = as.integer(year),
    age_prim_min    = as.integer(`299905`),
    age_prim_max    = as.integer(pmax(`299905`, `999975` - 1)),
    age_sec_min     = as.integer(`999975`),
    age_sec_max     = 18L,
    school_start_mo = if_else(`403` == 0, NA_real_, `403`)
  )

LookupSchoolAges <- function(this_iso3, this_year) {
  d <- unesco[unesco$iso3 == this_iso3 & !is.na(unesco$school_start_mo), ]
  d[which.min(abs(d$year - this_year)), ]
}


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

# Get household member data (PR file). Universe for NAR/GAR is de facto
# household members (hv103 == 1) of school age. hv121 = attendance in
# current school year (0 = no, 1 = curr yr, 2 = prev yr only); hv122 =
# level attended current year (0/missing = none/pre-primary, 1 = primary,
# 2 = secondary, 3 = higher).
pr <- read_dta(here("data", "Mali_2023-24_DHS",
                    "MLPR8ADT - Household Member", "MLPR8AFL.dta"))

# Algorithm: For each de facto household member aged 5-24, determine the
# academic year start, compute school_age, and flag school-age cohorts.
# Currently-attending-primary = hv121 == 1 & hv122 == 1.
# Currently-attending-secondary = hv121 == 1 & hv122 == 2.
# Conflict exposure: years between birth and interview that intersect the
# child's cluster's GED buffer.

# Precompute cluster x buffer-year intersection matrix
buffers_ccode <- buffers |> filter(iso3 == ccode)
hits <- st_intersects(clusters, buffers_ccode, sparse = FALSE)
buffer_years <- buffers_ccode$year

CalcConflExpDHS <- function(hv001, hv007, hv105) {
  if (is.na(hv105) || hv105 < 5 || hv105 > 24) return(NA_integer_)
  if (is.na(hv007)) return(NA_integer_)
  clust_row <- match(hv001, clusters$DHSCLUST)
  if (is.na(clust_row)) return(NA_integer_)
  sum(hits[clust_row, ] & buffer_years %in% (hv007 - hv105):hv007)
}

pr$conflict_exposure <- purrr::pmap_int(
  list(hv001 = as.numeric(pr$hv001),
       hv007 = as.numeric(pr$hv007),
       hv105 = as.numeric(pr$hv105)),
  CalcConflExpDHS
)

pr$exposed  <- pr$conflict_exposure > 0
pr$exposed3 <- pr$conflict_exposure > 2
pr$exposed5 <- pr$conflict_exposure > 4


# Build school_age (years old at the start of the current academic year).
# Synthetic birth CMC: interview CMC minus age*12, offset 6 months for mid-
# year anchor. PR file lacks a precise child DOB CMC for non-IR members.
ages <- LookupSchoolAges("MLI", 2023)

start_year <- ifelse(pr$hv006 >= ages$school_start_mo,
                     pr$hv007, pr$hv007 - 1)
acad_start_cmc <- (start_year - 1900) * 12 + ages$school_start_mo
birth_cmc <- pr$hv008 - pr$hv105 * 12 - 6
pr$school_age <- as.integer(floor((acad_start_cmc - birth_cmc) / 12))
pr$school_age <- if_else(pr$school_age >= 0 & pr$school_age <= 30,
                         pr$school_age, NA_integer_)

pr$prim_age <- as.integer(pr$school_age >= ages$age_prim_min &
                          pr$school_age <= ages$age_prim_max)
pr$sec_age  <- as.integer(pr$school_age >= ages$age_sec_min &
                          pr$school_age <= ages$age_sec_max)


# Attendance flags. Restrict to de facto household members; non-de-facto
# rows become NA so they drop out of both NAR and GAR.
pr <- pr |>
  mutate(
    wgt = hv005 / 1e6,
    in_prim = case_when(
      hv103 != 1                ~ NA_integer_,
      hv121 == 1 & hv122 == 1   ~ 1L,
      !is.na(hv121)             ~ 0L,
      .default = NA_integer_),
    in_sec = case_when(
      hv103 != 1                ~ NA_integer_,
      hv121 == 1 & hv122 == 2   ~ 1L,
      !is.na(hv121)             ~ 0L,
      .default = NA_integer_)
  )

pr_design <- svydesign(
  id = ~hv021,
  strata = ~hv022,
  weights = ~wgt,
  data = pr,
  nest = TRUE
)

design_prim <- subset(pr_design, prim_age == 1L)
design_sec  <- subset(pr_design, sec_age  == 1L)


# PRIMARY

# NAR primary: weighted proportion of primary-age children attending primary.
svymean(~in_prim, design = design_prim, na.rm = TRUE)

# Get same value published by DHS from their API, for comparison.
jsonlite::fromJSON(paste0(
  "https://api.dhsprogram.com/rest/dhs/data",
  "?indicatorIds=ED_NARP_B_BTH&countryIds=ML&surveyIds=ML2023DHS",
  "&breakdown=national&f=json",
  "&apiKey=", Sys.getenv("DHS_API_KEY")
))$Data$Value

# GAR primary: number of primary attenders (any age) over primary-age
# universe. Can exceed 1 when many over-/under-age children attend primary.
svyratio(~in_prim, ~prim_age, design = pr_design, na.rm = TRUE)

jsonlite::fromJSON(paste0(
  "https://api.dhsprogram.com/rest/dhs/data",
  "?indicatorIds=ED_GARP_B_BTH&countryIds=ML&surveyIds=ML2023DHS",
  "&breakdown=national&f=json",
  "&apiKey=", Sys.getenv("DHS_API_KEY")
))$Data$Value

svyby(~in_prim, by = ~exposed, design = design_prim,
      FUN = svymean, na.rm = TRUE)
svyttest(in_prim ~ exposed, design = design_prim)


# SECONDARY

# NAR secondary.
svymean(~in_sec, design = design_sec, na.rm = TRUE)

jsonlite::fromJSON(paste0(
  "https://api.dhsprogram.com/rest/dhs/data",
  "?indicatorIds=ED_NARS_B_BTH&countryIds=ML&surveyIds=ML2023DHS",
  "&breakdown=national&f=json",
  "&apiKey=", Sys.getenv("DHS_API_KEY")
))$Data$Value

# GAR secondary.
svyratio(~in_sec, ~sec_age, design = pr_design, na.rm = TRUE)

jsonlite::fromJSON(paste0(
  "https://api.dhsprogram.com/rest/dhs/data",
  "?indicatorIds=ED_GARS_B_BTH&countryIds=ML&surveyIds=ML2023DHS",
  "&breakdown=national&f=json",
  "&apiKey=", Sys.getenv("DHS_API_KEY")
))$Data$Value

svyby(~in_sec, by = ~exposed, design = design_sec,
      FUN = svymean, na.rm = TRUE)
svyttest(in_sec ~ exposed, design = design_sec)


# MICS ====

# Survey metadata
ccode <- "NGA"

# Get survey cluster locations
clusters <- st_read(
  here("data", "Nigeria_2021_MICS", "Geographic Data", "NigeriaMICS2021GPS.shp"),
  quiet = TRUE
) |> st_transform(crs = 4326)

# Get MICS Household Listing (hl) dataset — the school-attendance module
# lives here. Key variables (MICS6 standard):
#   HH1     = cluster                HH2     = household
#   HL6     = age in years           hhweight= household weight
#   stratum = stratum                PSU     = primary sampling unit
#   ED4     = ever attended school   (1 yes / 2 no)
#   ED5A    = currently attending    (1 yes / 2 no)
#   ED5B    = level current year     (1 primary / 2 lower-sec / 3 upper-sec /
#                                     4 higher / 0 pre-primary / ...)
hl <- read_sav(here("data", "Nigeria_2021_MICS", "SPSS Datasets", "hl.sav"))

# Conflict exposure (childhood years 0..age), as for DHS above.
buffers_ccode <- buffers |> filter(iso3 == ccode)
hits <- st_intersects(clusters, buffers_ccode, sparse = FALSE)
buffer_years <- buffers_ccode$year

CalcConflExpMICS <- function(HH1, HH5Y, HL6) {
  if (is.na(HL6) || HL6 < 5 || HL6 > 24) return(NA_integer_)
  if (is.na(HH5Y)) return(NA_integer_)
  clust_row <- match(HH1, clusters$HH1)
  if (is.na(clust_row)) return(NA_integer_)
  sum(hits[clust_row, ] & buffer_years %in% (HH5Y - HL6):HH5Y)
}

hl$conflict_exposure <- purrr::pmap_int(
  list(HH1  = as.numeric(hl$HH1),
       HH5Y = as.numeric(hl$HH5Y),
       HL6  = as.numeric(hl$HL6)),
  CalcConflExpMICS
)

hl$exposed  <- hl$conflict_exposure > 0
hl$exposed3 <- hl$conflict_exposure > 2
hl$exposed5 <- hl$conflict_exposure > 4


# Build school_age (years old at the start of the current academic year).
ages <- LookupSchoolAges("NGA", 2021)

start_year <- ifelse(hl$HH5M >= ages$school_start_mo,
                     hl$HH5Y, hl$HH5Y - 1)
acad_start_cmc <- (start_year - 1900) * 12 + ages$school_start_mo
# MICS HL records age only (HL6); synthetic birth CMC anchored at mid-year.
interview_cmc <- (hl$HH5Y - 1900) * 12 + hl$HH5M
birth_cmc     <- interview_cmc - hl$HL6 * 12 - 6
hl$school_age <- as.integer(floor((acad_start_cmc - birth_cmc) / 12))
hl$school_age <- if_else(hl$school_age >= 0 & hl$school_age <= 30,
                         hl$school_age, NA_integer_)

hl$prim_age <- as.integer(hl$school_age >= ages$age_prim_min &
                          hl$school_age <= ages$age_prim_max)
hl$sec_age  <- as.integer(hl$school_age >= ages$age_sec_min &
                          hl$school_age <= ages$age_sec_max)


# Attendance flags. ED5A == 1 currently attending, ED5B carries the level.
# MICS6 ED5B uses ISCED-style codes; Nigeria 2021 codes: 1 = primary,
# 2 = JSS (lower-sec), 3 = SSS (upper-sec). Secondary = ED5B %in% c(2, 3).
hl <- hl |>
  mutate(
    wgt = hhweight,
    in_prim = case_when(
      ED5A == 1 & ED5B == 1                    ~ 1L,
      ED5A == 2                                ~ 0L,
      ED5A == 1 & ED5B %in% c(0, 2, 3, 4, 5)   ~ 0L,
      .default = NA_integer_),
    in_sec = case_when(
      ED5A == 1 & ED5B %in% c(2, 3)            ~ 1L,
      ED5A == 2                                ~ 0L,
      ED5A == 1 & ED5B %in% c(0, 1, 4, 5)      ~ 0L,
      .default = NA_integer_)
  )

hl_design <- svydesign(
  id = ~PSU,
  strata = ~stratum,
  weights = ~wgt,
  data = hl,
  nest = TRUE
)

design_prim <- subset(hl_design, prim_age == 1L)
design_sec  <- subset(hl_design, sec_age  == 1L)


# PRIMARY

# Compare against the Nigeria MICS 2021 Survey Findings Report (UNICEF /
# NBS), Education chapter — Net Attendance Ratio (adjusted) for primary.
svymean(~in_prim, design = design_prim, na.rm = TRUE)
svyratio(~in_prim, ~prim_age, design = hl_design, na.rm = TRUE)

svyby(~in_prim, by = ~exposed, design = design_prim,
      FUN = svymean, na.rm = TRUE)
svyttest(in_prim ~ exposed, design = design_prim)


# SECONDARY

svymean(~in_sec, design = design_sec, na.rm = TRUE)
svyratio(~in_sec, ~sec_age, design = hl_design, na.rm = TRUE)

svyby(~in_sec, by = ~exposed, design = design_sec,
      FUN = svymean, na.rm = TRUE)
svyttest(in_sec ~ exposed, design = design_sec)



# Fin
