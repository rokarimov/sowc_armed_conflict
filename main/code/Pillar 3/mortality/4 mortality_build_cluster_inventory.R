# Project: SOWC 2026
# Goal: Build the union of DHS and MICS survey clusters — the spatial unit that
#       conflict exposure and cluster covariates are computed against
# Author: Rouslan Karimov
# Organization: UNICEF
# Created: 31 July 2026


library(here)
library(tidyverse)
library(sf)


# Load data ==================================================================

dhs  <- readRDS(here("data_derived", "mortality_dhs_births.rds"))
mics <- readRDS(here("data_derived", "mortality_mics_births.rds"))


# Build the inventory ========================================================

# The other Pillar 3 domains compute exposure inside a per-survey
# AnalyzeSurvey(), because each survey is analysed on its own. Here the
# regression is pooled across every survey at once, so exposure is computed
# once against the union of all clusters and joined back by cluster_uid. That
# also means the expensive spatial work happens once rather than ~157 times.
clusters <- bind_rows(
    dhs  |> select(source, sample_id, iso3, cluster_uid, gpslat, gpslong),
    mics |> select(source, sample_id, iso3, cluster_uid, gpslat, gpslong)
  ) |>
  distinct(cluster_uid, .keep_all = TRUE)


# A cluster_uid must resolve to exactly one point. If a survey ever reports two
# coordinate pairs under one cluster number the distinct() above silently keeps
# the first, so check for it rather than assume.
inconsistent <- bind_rows(
    dhs  |> select(cluster_uid, gpslat, gpslong),
    mics |> select(cluster_uid, gpslat, gpslong)
  ) |>
  distinct() |>
  count(cluster_uid) |>
  filter(n > 1)

if (nrow(inconsistent) > 0) {
  warning(nrow(inconsistent), " cluster_uid values carry more than one ",
          "coordinate pair — the first was kept:\n",
          paste(head(inconsistent$cluster_uid, 10), collapse = ", "))
}


clusters_sf <- clusters |>
  st_as_sf(coords = c("gpslong", "gpslat"), crs = 4326, remove = FALSE)


# Report =====================================================================

message("Clusters: ", format(nrow(clusters_sf), big.mark = ","))
print(count(st_drop_geometry(clusters_sf), source, name = "n_clusters"))
message("Countries: ", n_distinct(clusters_sf$iso3))


# Save =======================================================================

saveRDS(clusters_sf, here("data_derived", "mortality_clusters.rds"))

# Fin
