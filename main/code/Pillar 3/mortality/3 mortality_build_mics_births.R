# Project: SOWC 2026
# Goal: Build the child-level MICS birth table for the mortality pipeline,
#       harmonised to the same schema as the DHS side (script 2)
# Author: Rouslan Karimov
# Organization: UNICEF
# Created: 31 July 2026
#
# INPUT NOT YET ON DISK. This needs a new IPUMS-MICS extract at the Births unit
# of analysis. IPUMS offers a Births unit for 109 MICS samples, but only 15 of
# those also have GPS, and those 15 are the whole of our MICS universe:
#
#   COM 2022, SWZ 2021, FJI 2021, GMB 2018, GHA 2017, HND 2019, LAO 2017,
#   LSO 2018, MDG 2018, MWI 2019, NGA 2016, NGA 2021, PSE 2019, SLE 2017,
#   ZWE 2019
#
# GEO 2018, THA 2022, LAO 2023 and XKX 2013/2019 have GPS folders on disk but
# no Births sample in IPUMS, so they cannot enter this analysis at all.
#
# The household extract also needs re-pulling to add FJI 2021, MWI 2019 and
# PSE 2019 — it currently covers 13 samples and overlaps our 15 on only 12,
# and it is the only source of imicsstratum.
#
# This script folds together what the other Pillar 3 domains split across
# "2 global_ipums_mics_add_gps.R" and "3 *_ipums_mics_1_add_strata.R": there is
# only one topical file here, so a single pass is clearer than two.


library(here)
library(tidyverse)
library(haven)
library(sf)
library(countrycode)


# Load data ==================================================================

# CONFIRM the birth-history variable names against the IPUMS-MICS codebook once
# the extract lands, and adjust this rename map rather than the code below.
# Everything downstream is written against the right-hand names, which mirror
# the DHS variables used in script 2.
mics <- read_dta(here("data_derived", "ipums_mics_extract_births.dta")) |>
  zap_labels() |>
  rename(any_of(c(
    kiddobcmc     = "birthdatecmc",
    kidalive      = "childalive",
    kidagediedimp = "agediedmo",
    kidsex        = "sexbirth",
    kidbord       = "birthorder",
    intdatecmc    = "interviewcmc"
  )))

# Strip byte-encoding (e.g., "b'NGA'") from country_iso3 — same quirk the other
# MICS scripts handle.
mics <- mics |>
  mutate(iso3 = str_remove_all(as.character(country_iso3), "^b'|'$")) |>
  select(-country_iso3)


# Attach cluster GPS =========================================================

# Convention: <ISO3>_<YEAR>_MICS_v01_M, cluster shapefile under
# "GPS Datasets/*.shp". MICS HH1 is the cluster number. The join is keyed on
# sample + cluster because cluster IDs are recycled across surveys.
gps_root <- here("data", "IPUMS extracts", "MICS", "Geospatial")

sample_map <- mics |>
  distinct(sample = as.numeric(sample), iso3, year = as.integer(year))

folder_index <- tibble(folder = list.dirs(gps_root, recursive = FALSE)) |>
  mutate(iso3 = str_extract(basename(folder), "^[A-Z]{3}"),
         year = as.integer(str_extract(basename(folder), "(?<=_)[0-9]{4}"))) |>
  filter(!is.na(iso3), !is.na(year)) |>
  inner_join(sample_map, by = c("iso3", "year"))

ReadGPS <- function(folder, sample, ...) {
  shp_path <- list.files(file.path(folder, "GPS Datasets"),
                         pattern = "\\.shp$", full.names = TRUE)
  if (length(shp_path) == 0) return(NULL)
  st_read(shp_path[1], quiet = TRUE) |>
    st_drop_geometry() |>
    mutate(sample    = sample,
           cluster   = as.integer(HH1),
           longitude = as.numeric(LONGITUDE),
           latitude  = as.numeric(LATITUDE),
           .keep = "none")
}

gps <- pmap(folder_index, ReadGPS) |>
  list_rbind() |>
  distinct(sample, cluster, .keep_all = TRUE)

mics <- mics |>
  mutate(sample = as.numeric(sample), cluster = as.integer(cluster)) |>
  left_join(gps, by = c("sample", "cluster"))

# Surface samples that found no GPS folder before anything downstream silently
# drops them.
missing_gps <- setdiff(sample_map$sample, folder_index$sample)
if (length(missing_gps) > 0) {
  warning("No GPS folder for samples: ", paste(missing_gps, collapse = ", "))
}


# Attach sampling strata =====================================================

# The IPUMS-MICS topical files carry psu / imicspsu and the topical weight, but
# NOT stratum / imicsstratum — only the household extract has those. Households
# link to the topical records many-to-one on SAMPLE + CLUSTER + HHNO, per the
# IPUMS MICS linking guide (https://mics.ipums.org/mics/linking.shtml).
hh <- read_dta(here("data_derived",
                    "ipums_mics_extract_hh_characteristics_gps.dta")) |>
  zap_labels() |>
  select(sample, cluster, hhno, stratum, imicsstratum,
         psu_hh = psu, imicspsu_hh = imicspsu) |>
  distinct(sample, cluster, hhno, .keep_all = TRUE)

mics <- mics |>
  left_join(hh, by = c("sample", "cluster", "hhno")) |>
  mutate(psu      = coalesce(psu, psu_hh),
         imicspsu = coalesce(imicspsu, imicspsu_hh)) |>
  select(-psu_hh, -imicspsu_hh)

no_strata <- mics |> filter(is.na(imicsstratum)) |> distinct(sample) |> pull(sample)
if (length(no_strata) > 0) {
  warning("No strata matched for samples (re-pull the HH extract?): ",
          paste(no_strata, collapse = ", "))
}


# Prepare survey rows ========================================================

# Same universe and same outcome construction as the DHS side — see script 2
# for the reasoning on right-censoring, which is identical here.
mics <- mics |>
  filter(latitude != 0, longitude != 0,
         !is.na(latitude), !is.na(longitude),
         !is.na(kiddobcmc), !is.na(intdatecmc)) |>
  mutate(
    cluster_uid = paste(sample, cluster, sep = "_"),
    birth_year  = 1900L + as.integer((kiddobcmc - 1) %/% 12),
    birth_month = as.integer(kiddobcmc - 12 * (birth_year - 1900L)),
    months_observed = intdatecmc - kiddobcmc,
    alive           = if_else(kidalive %in% c(0, 1), as.integer(kidalive), NA_integer_),
    age_died_months = if_else(kidagediedimp < 900, kidagediedimp, NA_real_)
  ) |>
  filter(birth_year >= 1995, months_observed >= 0) |>
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
    survived_1y = case_when(alive == 1 ~ TRUE, age_died_months >= 12 ~ TRUE,
                            !is.na(age_died_months) ~ FALSE, .default = NA),
    survived_2y = case_when(alive == 1 ~ TRUE, age_died_months >= 24 ~ TRUE,
                            !is.na(age_died_months) ~ FALSE, .default = NA),
    survived_3y = case_when(alive == 1 ~ TRUE, age_died_months >= 36 ~ TRUE,
                            !is.na(age_died_months) ~ FALSE, .default = NA),
    survived_4y = case_when(alive == 1 ~ TRUE, age_died_months >= 48 ~ TRUE,
                            !is.na(age_died_months) ~ FALSE, .default = NA)
  )


# Harmonise to the DHS schema ================================================

births <- mics |>
  mutate(
    source      = "MICS",
    sample_id   = sample,
    country     = countrycode(iso3, origin = "iso3c", destination = "iso3n"),
    survey_year = year,
    psu         = imicspsu,
    strata      = imicsstratum,
    weight      = weightwm,
    child_sex   = kidsex,
    birth_order = kidbord,
    twin        = NA_real_,          # not collected in the MICS birth history
    mother_age_at_birth = agewm - months_observed / 12,
    mother_educ         = NA_real_,  # CONFIRM: MICS education coding differs
    mother_yrs_resident = NA_real_,  # MICS has no V104 equivalent
    .keep = "all"
  ) |>
  select(source, sample_id, iso3, country, survey_year,
         cluster_uid, psu, strata, weight,
         gpslat = latitude, gpslong = longitude, urban,
         birth_cmc = kiddobcmc, birth_year, birth_month,
         int_cmc = intdatecmc, months_observed,
         child_sex, birth_order, twin,
         alive, age_died_months, died_neo, died_u1, died_u5,
         survived_1y, survived_2y, survived_3y, survived_4y,
         mother_age_at_birth, mother_educ, mother_yrs_resident)


# Report =====================================================================

message("MICS births retained: ", format(nrow(births), big.mark = ","))
message("Surveys: ", n_distinct(births$sample_id),
        " in ", n_distinct(births$iso3), " countries")
message("Clusters: ", format(n_distinct(births$cluster_uid), big.mark = ","))
message("Infant mortality (unweighted, per 1,000): ",
        round(1000 * mean(births$died_u1, na.rm = TRUE), 1))

# Per-sample GPS match rate — the failure mode this pipeline is most exposed to
# is a silent GPS join miss, which would quietly drop a whole survey.
births |>
  count(sample_id, iso3, survey_year, name = "n_births") |>
  arrange(iso3, survey_year) |>
  print(n = Inf)


# Save =======================================================================

saveRDS(births, here("data_derived", "mortality_mics_births.rds"))

# Fin
