# Project: SOWC 2026
# Goal: Build the cluster x year covariate panel — annual rainfall, temperature,
#       night lights, degree of urbanisation and a food price index
# Author: Rouslan Karimov
# Organization: UNICEF
# Created: 31 July 2026
#
# Wagner controls on cluster-level temperature, rainfall, night light luminosity
# and a food price index, but never documents where any of them came from — the
# paper cites three studies to justify why the variables matter, the appendix
# gives sources only for the burden extrapolation, and there is no public
# replication archive. So these are built from public annual products instead,
# which are finer in time than the 5-year snapshots in the DHS/MICS geospatial
# covariate files and finer than anything Wagner is likely to have had.
#
# INPUTS TO DOWNLOAD (see the header of each section for the URL pattern):
#   data/CHIRPS/chirps-v2.0.<YYYY>.tif          annual precipitation
#   data/TerraClimate/TerraClimate_tmax_<YYYY>.nc and _tmin_<YYYY>.nc
#   data_derived/mortality_nightlights_gee.csv  exported by 6a_nightlights.js
#   data/DHS GeoCovariates/*.csv                one per DHS survey
#   data/WFP/wfp_food_prices_global.csv         from HDX
#
# Each section is guarded on its input existing, so the script stays runnable
# as the downloads arrive one at a time.


library(here)
library(tidyverse)
library(sf)
library(terra)
library(readxl)


# Load data ==================================================================

clusters <- readRDS(here("data_derived", "mortality_clusters.rds"))
births_years <- 1995:2024      # birth cohorts we need covariates for

covariates <- expand_grid(cluster_uid = clusters$cluster_uid,
                          year        = births_years)

# terra wants its own vector class, and extraction is by row order.
pts <- vect(clusters)


# Rainfall — CHIRPS v2.0 =====================================================
# Global annual precipitation, 0.05 degrees, 1981-present. One GeoTIFF per year:
# https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_annual/tifs/chirps-v2.0.<YYYY>.tif
# Free, no account needed.

chirps_dir <- here("data", "CHIRPS")

if (dir.exists(chirps_dir)) {
  rain <- map(births_years, function(y) {
    f <- file.path(chirps_dir, paste0("chirps-v2.0.", y, ".tif"))
    if (!file.exists(f)) return(NULL)
    tibble(cluster_uid = clusters$cluster_uid,
           year        = y,
           rain_mm     = terra::extract(rast(f), pts)[, 2])
  }) |>
    list_rbind() |>
    # CHIRPS marks water and no-data cells as -9999.
    mutate(rain_mm = if_else(rain_mm < 0, NA_real_, rain_mm))

  covariates <- left_join(covariates, rain, by = c("cluster_uid", "year"))
  message("Rainfall attached for ", n_distinct(rain$year), " years")
} else {
  message("SKIPPED rainfall — ", chirps_dir, " not found")
  covariates$rain_mm <- NA_real_
}


# Temperature — TerraClimate =================================================
# ~4 km monthly, 1958-present. Chosen over ERA5-Land because it needs no
# Copernicus account. Monthly max and min are averaged to an annual mean:
# http://thredds.northwestknowledge.net/thredds/fileServer/TERRACLIMATE_ALL/data/
#   TerraClimate_tmax_<YYYY>.nc  and  TerraClimate_tmin_<YYYY>.nc

tc_dir <- here("data", "TerraClimate")

if (dir.exists(tc_dir)) {
  temp <- map(births_years, function(y) {
    f_max <- file.path(tc_dir, paste0("TerraClimate_tmax_", y, ".nc"))
    f_min <- file.path(tc_dir, paste0("TerraClimate_tmin_", y, ".nc"))
    if (!file.exists(f_max) || !file.exists(f_min)) return(NULL)

    # Each file holds twelve monthly layers; rowMeans across them gives the
    # annual mean of the monthly maxima (and minima), and the midpoint of the
    # two is the conventional annual mean temperature.
    tmax <- rowMeans(terra::extract(rast(f_max), pts)[, -1], na.rm = TRUE)
    tmin <- rowMeans(terra::extract(rast(f_min), pts)[, -1], na.rm = TRUE)

    tibble(cluster_uid = clusters$cluster_uid,
           year        = y,
           temp_c      = (tmax + tmin) / 2)
  }) |>
    list_rbind()

  covariates <- left_join(covariates, temp, by = c("cluster_uid", "year"))
  message("Temperature attached for ", n_distinct(temp$year), " years")
} else {
  message("SKIPPED temperature — ", tc_dir, " not found")
  covariates$temp_c <- NA_real_
}


# Night lights — harmonized DMSP-VIIRS =======================================
# Annual, ~1 km, 1992-2023 (Li et al. 2020 Scientific Data, extended 2024).
# Extracted server-side in Google Earth Engine by "6a_nightlights.js" in this
# folder, which exports one row per cluster-year. Doing it in GEE avoids
# downloading tens of gigabytes of global 30 arc-second rasters.

ntl_path <- here("data_derived", "mortality_nightlights_gee.csv")

if (file.exists(ntl_path)) {
  ntl <- read_csv(ntl_path, show_col_types = FALSE) |>
    select(cluster_uid, year, ntl)
  covariates <- left_join(covariates, ntl, by = c("cluster_uid", "year"))
  message("Night lights attached for ", n_distinct(ntl$year), " years")
} else {
  message("SKIPPED night lights — run 6a_nightlights.js in GEE first")
  covariates$ntl <- NA_real_
}


# Degree of urbanisation =====================================================
# The one thing worth keeping from the survey geospatial covariate files. It is
# a 3-class GHSL classification (1 = city, 2 = town / semi-dense, 3 = rural),
# which is more informative than the urban/rural binary Wagner splits on.
#
# It is close to time-invariant in practice — on the Nigeria MICS 2021 clusters
# only 232 of 2,079 change class across the 2000-2025 snapshots — so treat it as
# a stratifier rather than a time-varying control. Snapshots are carried forward
# to the nearest available year rather than interpolated, because interpolating
# a categorical variable is meaningless.
#
# MICS files are already on disk; the DHS ones must be downloaded one per survey
# from spatialdata.dhsprogram.com/covariates/. CONFIRM the DHS column names on
# first download — the cluster key is expected to be DHSCLUST.

mics_gc <- list.dirs(here("data", "IPUMS extracts", "MICS", "Geospatial"),
                     recursive = FALSE) |>
  map(function(folder) {
    f <- list.files(file.path(folder, "Geospatial Covariates"),
                    pattern = "GeoCov[.]xlsx$", full.names = TRUE)
    if (length(f) == 0) return(NULL)
    d <- read_excel(f[1], sheet = "Data - Cluster") |> suppressWarnings()
    keep <- c("cluster", grep("^DEGURBA_[0-9]{4}$", names(d), value = TRUE))
    d |>
      select(all_of(keep)) |>
      mutate(iso3 = str_extract(basename(folder), "^[A-Z]{3}"),
             svy_year = as.integer(str_extract(basename(folder), "(?<=_)[0-9]{4}")))
  }) |>
  list_rbind()

if (!is.null(mics_gc) && nrow(mics_gc) > 0) {
  message("MICS DEGURBA read for ", n_distinct(mics_gc$iso3), " surveys")
} else {
  message("SKIPPED MICS DEGURBA — no GeoCov files found")
}

# The join back to cluster_uid needs the sample_id that script 4 built the uid
# from, so it is deferred to script 7 where the survey identifiers are present.
# Write the long-format snapshot table and let the master do the carry-forward.
if (!is.null(mics_gc) && nrow(mics_gc) > 0) {
  degurba <- mics_gc |>
    pivot_longer(starts_with("DEGURBA_"),
                 names_to = "snapshot_year", values_to = "degurba") |>
    mutate(snapshot_year = as.integer(str_remove(snapshot_year, "DEGURBA_"))) |>
    filter(!is.na(degurba))
  saveRDS(degurba, here("data_derived", "mortality_degurba_snapshots.rds"))
}


# Food price index — WFP VAM =================================================
# From HDX: https://data.humdata.org/dataset/global-wfp-food-prices
# ~98 countries, ~3,000 markets, monthly. Coverage is thin before about 2005,
# so early birth cohorts will carry NA here — that is a real limitation, not a
# bug, and it is why food prices are the weakest of the four controls.
#
# Prices are not comparable across commodities or units, so an index is built
# rather than a price: each observation is expressed relative to the median
# price of that same commodity-unit-pricetype within the same country, then
# averaged within market-year. A value of 1 means "typical for this country".

wfp_path <- here("data", "WFP", "wfp_food_prices_global.csv")

if (file.exists(wfp_path)) {
  wfp <- read_csv(wfp_path, show_col_types = FALSE) |>
    filter(!is.na(usdprice), usdprice > 0,
           !is.na(latitude), !is.na(longitude)) |>
    mutate(year = as.integer(str_sub(date, 1, 4))) |>
    filter(year %in% births_years)

  market_year <- wfp |>
    mutate(rel = usdprice / median(usdprice),
           .by = c(countryiso3, commodity, unit, pricetype)) |>
    summarise(price_idx = mean(rel),
              latitude  = first(latitude),
              longitude = first(longitude),
              .by = c(countryiso3, market, year))

  # Nearest market to each cluster, capped at 200 km — beyond that the local
  # price signal is not credibly local. st_nearest_feature is computed once on
  # the distinct market locations, not per year.
  markets_sf <- market_year |>
    distinct(countryiso3, market, latitude, longitude) |>
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

  nearest <- st_nearest_feature(clusters, markets_sf)
  dist_m  <- st_distance(clusters, markets_sf[nearest, ], by_element = TRUE)

  cluster_market <- tibble(
    cluster_uid = clusters$cluster_uid,
    market      = markets_sf$market[nearest],
    countryiso3 = markets_sf$countryiso3[nearest],
    dist_km     = as.numeric(dist_m) / 1000) |>
    filter(dist_km <= 200)

  prices <- cluster_market |>
    left_join(market_year, by = c("countryiso3", "market"),
              relationship = "many-to-many") |>
    select(cluster_uid, year, food_price_idx = price_idx)

  covariates <- left_join(covariates, prices, by = c("cluster_uid", "year"))
  message("Food prices attached for ",
          format(n_distinct(cluster_market$cluster_uid), big.mark = ","),
          " clusters within 200 km of a market")
} else {
  message("SKIPPED food prices — ", wfp_path, " not found")
  covariates$food_price_idx <- NA_real_
}


# Report =====================================================================

message("\nCovariate panel: ", format(nrow(covariates), big.mark = ","), " rows")
covariates |>
  summarise(across(c(rain_mm, temp_c, ntl, food_price_idx),
                   ~ round(100 * mean(!is.na(.x)), 1))) |>
  print()


# Save =======================================================================

saveRDS(covariates, here("data_derived", "mortality_cluster_covariates.rds"))

# Fin
