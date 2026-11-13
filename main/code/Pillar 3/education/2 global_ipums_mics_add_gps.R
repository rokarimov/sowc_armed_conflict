# Project: SOWC 2026
# Goal: Attach MICS cluster GPS coordinates to the IPUMS MICS .dta extracts
#       (child-marriage, household-characteristics, nutrition, and education).
# Author: Rouslan Karimov
# Organization: UNICEF

library(here)
library(tidyverse)
library(haven)
library(sf)


# Build a (iso3, year) -> IPUMS sample map from the .dta files. The join below
# is keyed on `sample + cluster` so it stays correct even if two IPUMS samples
# ever share a calendar year for one country (cluster IDs are recycled across
# surveys, so country-year scoping alone is not future-proof).

cm_path  <- here("data_derived", "ipums_mics_extract_child_marriage.dta")
hh_path  <- here("data_derived", "ipums_mics_extract_hh_characteristics.dta")
nut_path <- here("data_derived", "ipums_mics_extract_nutrition.dta")
ed_path  <- here("data_derived", "ipums_mics_extract_education.dta")

# Strip byte-encoding (e.g., "b'NGA'") from country_iso3
CleanISO <- function(x) str_remove_all(as.character(x), "^b'|'$")

# Optional extracts are skipped silently when not yet downloaded, so this
# script stays runnable as topical extracts are added over time.
ReadSampleKey <- function(path) {
  if (!file.exists(path)) return(NULL)
  read_dta(path, col_select = c(sample, country_iso3, year))
}

sample_map <- bind_rows(
    ReadSampleKey(cm_path),
    ReadSampleKey(hh_path),
    ReadSampleKey(nut_path),
    ReadSampleKey(ed_path)
  ) |>
  transmute(
    sample = as.numeric(sample),
    iso3   = CleanISO(country_iso3),
    year   = as.integer(year)
  ) |>
  distinct()

ambiguous <- sample_map |> count(iso3, year) |> filter(n > 1)
if (nrow(ambiguous) > 0) {
  warning("Multiple IPUMS samples share (iso3, year) — folder lookup is ",
          "ambiguous and needs a manual override:\n",
          paste(capture.output(print(ambiguous)), collapse = "\n"))
}


# Index GPS folders ------------------------------------------------------------
# Convention: <ISO3>_<YEAR>_MICS_v01_M, with the cluster shapefile under
# "GPS Datasets/*.shp". A sibling "mics_boundaries_nr shp" subdir holds survey
# boundaries (not clusters) — non-recursive list.files skips it.

gps_root <- here("data", "IPUMS extracts", "MICS", "Geospatial")

folder_index <- tibble(folder = list.dirs(gps_root, recursive = FALSE)) |>
  mutate(
    iso3 = str_extract(basename(folder), "^[A-Z]{3}"),
    year = as.integer(str_extract(basename(folder), "(?<=_)[0-9]{4}"))
  ) |>
  filter(!is.na(iso3), !is.na(year)) |>
  inner_join(sample_map, by = c("iso3", "year"))


# Read each cluster shapefile and stack ----------------------------------------
ReadGPS <- function(folder, sample, ...) {
  shp_path <- list.files(file.path(folder, "GPS Datasets"),
                         pattern = "\\.shp$", full.names = TRUE)
  if (length(shp_path) == 0) return(NULL)

  st_read(shp_path[1], quiet = TRUE) |>
    st_drop_geometry() |>
    transmute(
      sample    = sample,
      cluster   = as.integer(HH1),
      longitude = as.numeric(LONGITUDE),
      latitude  = as.numeric(LATITUDE)
    )
}

gps <- pmap(folder_index, ReadGPS) |>
  list_rbind() |>
  distinct(sample, cluster, .keep_all = TRUE)


# Attach to the two .dta extracts ----------------------------------------------
AttachGPS <- function(dta_in, dta_out) {
  d <- read_dta(dta_in) |>
    mutate(iso3 = CleanISO(country_iso3)) |>
    select(-country_iso3)

  key <- tibble(
    sample  = as.numeric(d$sample),
    cluster = as.integer(d$cluster)
  )
  joined <- left_join(key, gps, by = c("sample", "cluster"))

  d$latitude  <- joined$latitude
  d$longitude <- joined$longitude

  # Save GPS-enriched dataset to disk
  write_dta(d, dta_out)

  # Output summary: For each sample, what % of observations did we 
  # match with their GPS coordinates?
  tibble(sample = key$sample, latitude = d$latitude) |>
    group_by(sample) |>
    summarise(
      n       = n(),
      matched = sum(!is.na(latitude)),
      pct     = round(100 * matched / n, 1),
      .groups = "drop"
    ) |>
    left_join(distinct(sample_map, sample, iso3, year), by = "sample") |>
    relocate(iso3, year, sample)
}

cm_summary <- AttachGPS(
  cm_path,
  here("data_derived", "ipums_mics_extract_child_marriage_gps.dta")
)
hh_summary <- AttachGPS(
  hh_path,
  here("data_derived", "ipums_mics_extract_hh_characteristics_gps.dta")
)
if (file.exists(nut_path)) {
  nut_summary <- AttachGPS(
    nut_path,
    here("data_derived", "ipums_mics_extract_nutrition_gps.dta")
  )
}
if (file.exists(ed_path)) {
  ed_summary <- AttachGPS(
    ed_path,
    here("data_derived", "ipums_mics_extract_education_gps.dta")
  )
}

# Surface samples in the .dta with no matching GPS folder.
setdiff(sample_map$sample, folder_index$sample)

