# Project: SOWC 2026
# Goal: Build the child-level DHS birth table for the mortality pipeline —
#       one row per birth, with survival outcomes and mother/child covariates
# Author: Rouslan Karimov
# Organization: UNICEF
# Created: 31 July 2026
#
# INPUT NOT YET ON DISK. This script reads a new IPUMS-DHS extract at the
# BIRTHS unit of analysis. The existing data/IPUMS extracts/DHS/births.dta was
# slimmed in Stata to four columns (sample, idhshid, kiddobcmc, linenokid) with
# no GPS and no survival variables, so it cannot be used here. Request the new
# extract for every survey in get_from_ipums_dhs.csv with the variables listed
# in the col_select below, then confirm the two sentinel conventions flagged
# with CONFIRM before trusting the output.


library(here)
library(tidyverse)
library(haven)
library(countrycode)


# Load data ==================================================================

births <- read_dta(
  here("data", "IPUMS extracts", "DHS", "births_mortality.dta"),
  col_select = c(sample, country, year,
                 idhspid, idhshid, idhspsu, idhsstrata, clusternoall,
                 perweight,
                 gpslat, gpslong, urban,
                 intdatecmc,
                 bidx, kiddobcmc, kidsex, kidalive, kidagediedimp, kidbord,
                 kidtwin,
                 age, educlvl, yrslived)) |>
  zap_labels()


# Prepare survey rows ========================================================

# Universe: births with usable cluster coordinates and a valid date of birth,
# from 1995 onwards. 1995 is Wagner's start year; it also leaves six years of
# GED history (geocoding starts 1989) behind the earliest birth, so the
# shorter lag windows are complete from the outset.
births <- births |>
  filter(gpslat != 0, gpslong != 0,
         !is.na(gpslat), !is.na(gpslong),
         !is.na(kiddobcmc), !is.na(intdatecmc)) |>
  mutate(
    iso3       = countrycode(country, origin = "iso3n", destination = "iso3c"),
    cluster_uid = paste(sample, clusternoall, sep = "_"),

    # CMC (century month code) = (year - 1900) * 12 + month, so month 1 is
    # January 1900. Invert it for the birth-year and birth-month fixed effects.
    birth_year  = 1900L + as.integer((kiddobcmc - 1) %/% 12),
    birth_month = as.integer(kiddobcmc - 12 * (birth_year - 1900L)),

    # Months of observation available for this child: how long we could have
    # seen a death before the mother was interviewed.
    months_observed = intdatecmc - kiddobcmc,

    # CONFIRM once the extract lands: IPUMS-DHS KIDALIVE is expected to be
    # 0 = dead / 1 = alive, with 8-9 reserved for missing. KIDAGEDIEDIMP is
    # DHS b7 (age at death in months, imputed) and is expected to carry 997+
    # as its missing sentinels. Both guards below are deliberately loose.
    alive           = if_else(kidalive %in% c(0, 1), as.integer(kidalive), NA_integer_),
    age_died_months = if_else(kidagediedimp < 900, kidagediedimp, NA_real_)
  ) |>
  filter(birth_year >= 1995, months_observed >= 0)


# Outcomes ===================================================================

# Right-censoring is the whole ballgame here. A child born four months before
# the interview cannot contribute to infant mortality: they have not had the
# chance to die before age one. Coding them 0 rather than NA would drag the
# denominator up with no matching numerator and bias every mortality rate
# downward. So each outcome is defined only over children with a full exposure
# window, and is NA otherwise.
births <- births |>
  mutate(
    died_neo = case_when(
      months_observed < 1  ~ NA_integer_,
      alive == 1           ~ 0L,
      age_died_months < 1  ~ 1L,
      !is.na(age_died_months) ~ 0L,
      .default = NA_integer_),

    died_u1 = case_when(
      months_observed < 12 ~ NA_integer_,
      alive == 1           ~ 0L,
      age_died_months < 12 ~ 1L,
      !is.na(age_died_months) ~ 0L,
      .default = NA_integer_),

    died_u5 = case_when(
      months_observed < 60 ~ NA_integer_,
      alive == 1           ~ 0L,
      age_died_months < 60 ~ 1L,
      !is.na(age_died_months) ~ 0L,
      .default = NA_integer_),

    # Conditional-survival flags for the under-5 stage. Wagner's under-5
    # specification is Pr(y | y_{a-1} = 0): when he estimates the effect of
    # conflict in the a-th year of life he restricts to children who reached
    # age a, so that conflict cannot appear to predict a death that already
    # happened. These four flags are derivable from age_died_months, but the
    # stage-3 sample restrictions are much easier to read as a filter.
    survived_1y = case_when(alive == 1 ~ TRUE,
                            age_died_months >= 12 ~ TRUE,
                            !is.na(age_died_months) ~ FALSE,
                            .default = NA),
    survived_2y = case_when(alive == 1 ~ TRUE,
                            age_died_months >= 24 ~ TRUE,
                            !is.na(age_died_months) ~ FALSE,
                            .default = NA),
    survived_3y = case_when(alive == 1 ~ TRUE,
                            age_died_months >= 36 ~ TRUE,
                            !is.na(age_died_months) ~ FALSE,
                            .default = NA),
    survived_4y = case_when(alive == 1 ~ TRUE,
                            age_died_months >= 48 ~ TRUE,
                            !is.na(age_died_months) ~ FALSE,
                            .default = NA)
  )


# Covariates =================================================================

# Mother's age at interview minus the child's age gives her age when she gave
# birth — Wagner controls on age of mother, and a mother's age at interview
# would be the wrong quantity for a birth twenty years earlier.
births <- births |>
  mutate(
    source              = "DHS",
    sample_id           = sample,
    survey_year         = year,
    psu                 = idhspsu,
    strata              = idhsstrata,
    weight              = perweight,
    mother_age_at_birth = age - months_observed / 12,
    mother_educ         = if_else(educlvl < 8, educlvl, NA_real_),
    mother_yrs_resident = if_else(yrslived < 90, yrslived, NA_real_),
    child_sex           = kidsex,
    birth_order         = kidbord,
    twin                = kidtwin,
    .keep = "all"
  ) |>
  select(source, sample_id, iso3, country, survey_year,
         cluster_uid, psu, strata, weight,
         gpslat, gpslong, urban,
         birth_cmc = kiddobcmc, birth_year, birth_month,
         int_cmc = intdatecmc, months_observed,
         child_sex, birth_order, twin,
         alive, age_died_months, died_neo, died_u1, died_u5,
         survived_1y, survived_2y, survived_3y, survived_4y,
         mother_age_at_birth, mother_educ, mother_yrs_resident)


# Report =====================================================================

message("DHS births retained: ", format(nrow(births), big.mark = ","))
message("Surveys: ", n_distinct(births$sample_id),
        " in ", n_distinct(births$iso3), " countries")
message("Clusters: ", format(n_distinct(births$cluster_uid), big.mark = ","))
message("Birth years: ", min(births$birth_year), "-", max(births$birth_year))
message("Infant mortality (unweighted, per 1,000): ",
        round(1000 * mean(births$died_u1, na.rm = TRUE), 1))
message("Births censored out of died_u1: ",
        format(sum(is.na(births$died_u1)), big.mark = ","))


# Save =======================================================================

saveRDS(births, here("data_derived", "mortality_dhs_births.rds"))

# Fin
