# Project: SOWC 2026
# Goal: Build unioned 50 km conflict-zone buffers for every country-year in the
#       GED dataset from 1996 onwards
# Author: Rouslan Karimov
# Organization: UNICEF
# Created: 1 May 2026


library(here)
library(tidyverse)
library(sf)
library(countrycode)

# Load data
ged <- readRDS(here("data", "Uppsala GEDEvent_v25_1.rds")) |>
  mutate(iso3 = countrycode(country_id, origin = "gwn", destination = "iso3c",
                             custom_match = c("345" = "SRB",
                                              "678" = "YMN")
                             )
  )


# Country-year buffers ====

# We stay in WGS84 and let s2 buffer on the sphere — `dist` is in meters
# globally, so we avoid picking a planar CRS per country (wrong for most,
# ambiguous for countries spanning several UTM zones).
ged_buffers <- ged |>
  filter(year >= 1996) |>
  filter(!is.na(longitude), !is.na(latitude)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  group_by(iso3, country, year) |>
  summarise(.groups = "drop") |>
  st_buffer(dist = 50000)

saveRDS(ged_buffers, here("data_derived", "ged_country_year_buffers.rds"))

# Fin
