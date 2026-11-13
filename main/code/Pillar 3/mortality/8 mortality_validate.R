# Project: SOWC 2026
# Goal: Validate the mortality master dataset before any modelling — coverage,
#       agreement with DHS-published rates, and agreement with Wagner's Table 2
# Author: Rouslan Karimov
# Organization: UNICEF
# Created: 31 July 2026


library(here)
library(tidyverse)
library(countrycode)
library(jsonlite)


# Load data ==================================================================

master <- readRDS(here("data_derived", "mortality_master.rds"))


# 1. Coverage ================================================================

# The failure mode this pipeline is most exposed to is a silent GPS or strata
# join miss that drops a whole survey without erroring. A per-survey table makes
# that obvious: a survey with zero clusters or 0% exposure is a bug, not a
# finding.
coverage <- master |>
  summarise(
    n_births    = n(),
    n_clusters  = n_distinct(cluster_uid),
    yr_min      = min(birth_year),
    yr_max      = max(birth_year),
    pct_exposed = round(100 * mean(conf_50_y1, na.rm = TRUE), 1),
    pct_u1_obs  = round(100 * mean(!is.na(died_u1)), 1),
    pct_u5_obs  = round(100 * mean(!is.na(died_u5)), 1),
    imr         = round(1000 * mean(died_u1, na.rm = TRUE), 1),
    u5mr        = round(1000 * mean(died_u5, na.rm = TRUE), 1),
    .by = c(source, sample_id, iso3, survey_year)
  ) |>
  arrange(iso3, survey_year)

print(coverage, n = Inf)

suspect <- coverage |> filter(n_clusters < 20 | pct_u1_obs < 50 | imr == 0)
if (nrow(suspect) > 0) {
  warning("Surveys that look wrong — inspect before modelling:\n",
          paste(capture.output(print(suspect)), collapse = "\n"))
}


# 2. Against DHS-published rates =============================================

# CM_ECMR_C_IMR / _U5M / _NNR are the published infant, under-five and neonatal
# mortality rates. Same fetch helper as the education pipeline; the API key
# lives in .Renviron as DHS_API_KEY (note: no .Renviron exists in the project
# yet, so this section will run unauthenticated and may be rate-limited).
FetchDhsData <- function(indicator_ids, country_codes) {
  base <- "https://api.dhsprogram.com/rest/dhs/data"
  api_key <- Sys.getenv("DHS_API_KEY")
  qs <- paste0(
    "?indicatorIds=", paste(indicator_ids, collapse = ","),
    "&countryIds=",   paste(country_codes,  collapse = ","),
    "&breakdown=all&perpage=1000&f=json",
    if (nzchar(api_key)) paste0("&apiKey=", api_key) else ""
  )
  pages <- list(); page <- 1
  repeat {
    resp <- fromJSON(paste0(base, qs, "&page=", page), simplifyDataFrame = TRUE)
    pages[[page]] <- resp$Data
    if (page >= resp$TotalPages) break
    page <- page + 1
  }
  bind_rows(pages)
}

# DHS publishes mortality for the five years preceding the survey, so our
# all-cohort rate is not the comparable quantity. Restrict to births 12-60
# months before interview: the lower bound keeps the under-1 window complete,
# the upper bound matches the published reference period.
#
# This remains an approximation. DHS uses a synthetic-cohort life table, not a
# simple proportion of deaths to births, so expect agreement to within a few
# points rather than exactly. Section 3 is the exact check.
recent <- master |>
  filter(source == "DHS", months_observed >= 12, months_observed <= 60) |>
  summarise(imr_ours  = round(1000 * mean(died_u1, na.rm = TRUE), 1),
            .by = c(iso3, survey_year)) |>
  mutate(dhs_cc = countrycode(iso3, "iso3c", "dhs"))

dhs_published <- FetchDhsData("CM_ECMR_C_IMR", na.omit(unique(recent$dhs_cc))) |>
  filter(IsPreferred == 1, SurveyType == "DHS",
         CharacteristicCategory == "Total") |>
  mutate(dhs_cc      = DHS_CountryCode,
         survey_year = SurveyYear,
         imr_pub     = Value,
         .keep = "none")

comparison <- recent |>
  left_join(dhs_published, by = c("dhs_cc", "survey_year")) |>
  mutate(diff = imr_ours - imr_pub) |>
  arrange(desc(abs(diff)))

message("\nIMR vs DHS-published (largest gaps first):")
print(head(comparison, 25))
message("Median absolute difference: ",
        round(median(abs(comparison$diff), na.rm = TRUE), 1), " per 1,000")


# 3. Against DHS.rates::chmort ===============================================

# The strongest single check that the birth-history variables were decoded
# correctly. DHS.rates::chmort wants raw DHS variable names (v005, v008, v021,
# v022, b3, b5, b7), which the IPUMS harmonised names do not match, so this is
# run manually on a few surveys rather than wired into the pipeline:
#
#   br <- read_dta("<survey>/XXBR<phase>FL.dta")
#   chmort(br)                       # see code/Pillar 3/OLD_analyze_surveys.R
#
# Compare the IMR and U5MR rows against `recent` above for the same survey. A
# gap of more than a few points per 1,000 means either the censoring rule or the
# KIDALIVE / KIDAGEDIEDIMP sentinel handling in script 2 is wrong.


# 4. Against Wagner's Table 2 ================================================

# Restricting to his frame — Africa, births 1995-2015 — should land us in the
# same neighbourhood. It will not match exactly: we use GED v25.1 rather than
# the 2015 vintage, and more surveys. A large gap in the *exposed share* points
# at the spatial join in script 5; a large gap in the *mortality rate* points at
# the censoring rule in script 2.
wagner_reference <- tribble(
  ~group,        ~births,   ~deaths, ~rate_per_1000,
  "total",       1985109,   133361,  67.18,
  "conflict",     252027,    18795,  74.58,
  "no conflict", 1733082,   114566,  66.11,
  "1-4",           69683,     4297,  61.66,
  "5-18",          57050,     3948,  69.20,
  "19-74",         62952,     4776,  75.87,
  "75-1000",       50663,     4540,  89.61,
  ">1000",         11679,     1234, 105.66
)

africa <- master |>
  filter(countrycode(iso3, "iso3c", "continent") == "Africa",
         birth_year >= 1995, birth_year <= 2015,
         !is.na(died_u1))

ours <- bind_rows(
  africa |>
    summarise(group = "total", births = n(), deaths = sum(died_u1)),
  africa |>
    summarise(births = n(), deaths = sum(died_u1), .by = conf_50_y1) |>
    mutate(group = if_else(conf_50_y1, "conflict", "no conflict"), .keep = "unused"),
  africa |>
    filter(intensity_y1 != "none") |>
    summarise(births = n(), deaths = sum(died_u1), .by = intensity_y1) |>
    mutate(group = as.character(intensity_y1), .keep = "unused")
) |>
  mutate(rate_per_1000 = round(1000 * deaths / births, 2))

wagner_check <- wagner_reference |>
  left_join(ours, by = "group", suffix = c("_wagner", "_ours")) |>
  mutate(rate_diff = rate_per_1000_ours - rate_per_1000_wagner)

message("\nAgainst Wagner Table 2 (Africa, 1995-2015):")
print(wagner_check)

message("\nExposed share — Wagner 12.7%, ours ",
        round(100 * mean(africa$conf_50_y1, na.rm = TRUE), 1), "%")


# 5. Spatial spot-check ======================================================

# Nigeria 2014-2015 and DRC 2013 are years with unambiguous, well-located
# conflict. If the 50 km flags are right, exposure in those cluster-years should
# be both high and concentrated.
message("\nSpot-check — exposure by year in high-conflict settings:")
master |>
  filter(iso3 %in% c("NGA", "COD"), birth_year >= 2010, birth_year <= 2016) |>
  summarise(births      = n(),
            pct_exposed = round(100 * mean(conf_50_y1, na.rm = TRUE), 1),
            mean_deaths = round(mean(deaths_50_y1, na.rm = TRUE)),
            .by = c(iso3, birth_year)) |>
  arrange(iso3, birth_year) |>
  print(n = Inf)


# Save =======================================================================

write_csv(coverage, here("data_derived", "mortality_validation.csv"))

# Fin
