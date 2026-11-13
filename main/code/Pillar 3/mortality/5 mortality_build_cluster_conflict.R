# Project: SOWC 2026
# Goal: Build the cluster x year conflict panel — events and battle deaths
#       within 50 km of each survey cluster, for every year of GED coverage
# Author: Rouslan Karimov
# Organization: UNICEF
# Created: 31 July 2026


library(here)
library(tidyverse)
library(sf)

# furrr is optional here. The vectorised aggregation below made the sequential
# path fast enough to be tolerable for a one-off build (roughly twelve minutes
# for ~90,000 clusters), and furrr is currently not installed under R 4.6, so
# the script falls back rather than failing. Install furrr to cut it to a few
# minutes.
use_furrr <- requireNamespace("furrr", quietly = TRUE)


# Load data ==================================================================

clusters <- readRDS(here("data_derived", "mortality_clusters.rds"))
events   <- readRDS(here("data_derived", "mortality_ged_events.rds"))

events_sf <- events |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)


# Cluster-year conflict ======================================================

# Distance is measured point-to-point rather than by intersecting pre-built
# buffers. The Pillar 3 buffer approach (ged_country_year_buffers.rds) dissolves
# events into one polygon per country-year, which throws away the two things
# this analysis needs most: how many events were nearby, and how many people
# they killed. Wagner's intensity bands and chronicity measure are both built
# from those counts.
#
# We stay in WGS84 and let s2 measure on the sphere, so `dist` is in metres
# globally and we avoid picking a planar CRS per country — the same reasoning
# as the comment in the existing "1 global_make_conflict_zone_buffers.R".
#
# Events are NOT restricted to the cluster's own country. Proximity is
# geometric: an event 30 km across a national border is still 30 km away, and
# Wagner draws his 50 km radius without regard to borders.

ConflictNearClusters <- function(cl_chunk) {
  hits <- st_is_within_distance(cl_chunk, events_sf, dist = 50000)

  # st_is_within_distance returns a sparse list: one vector of event row indices
  # per cluster. Flattening it into a single long tibble and aggregating once is
  # about twelve times faster than building a tibble per cluster, and was
  # checked to give identical output on the Nigeria 2021 clusters.
  idx <- unlist(hits, use.names = FALSE)
  if (length(idx) == 0) return(NULL)

  tibble(cluster_uid = rep(cl_chunk$cluster_uid, lengths(hits)),
         year        = events$year[idx],
         best        = events$best[idx]) |>
    summarise(n_events = n(),
              deaths   = sum(best),
              .by      = c(cluster_uid, year))
}

# Chunked so the sparse hit lists never materialise all at once. multisession
# (not multicore) because macOS plus sf has known trouble with R's fork-based
# parallelism — same choice the DHS education script makes.
chunks <- clusters |>
  mutate(chunk = (row_number() - 1) %/% 2000) |>
  group_split(chunk)

if (use_furrr) {
  furrr::plan(future::multisession, workers = 6)
  conflict_sparse <- chunks |>
    furrr::future_map(ConflictNearClusters, .progress = TRUE,
                      .options = furrr::furrr_options(packages = "sf",
                                                      seed = TRUE)) |>
    list_rbind()
  furrr::plan(future::sequential)   # release worker processes
} else {
  message("furrr not installed — running sequentially, expect ~12 minutes")
  conflict_sparse <- chunks |>
    map(ConflictNearClusters, .progress = TRUE) |>
    list_rbind()
}


# Densify ====================================================================

# The sparse result holds only cluster-years that saw conflict. Chronicity —
# Wagner's count of *consecutive* conflict years leading up to birth — needs the
# zero years to be present as rows, otherwise a gap in the series is
# indistinguishable from a missing observation. Roughly 90,000 clusters x 36
# years is about 3.2M rows, which is small enough to carry densely.
cluster_conflict <- expand_grid(
    cluster_uid = clusters$cluster_uid,
    year        = min(events$year):max(events$year)
  ) |>
  left_join(conflict_sparse, by = c("cluster_uid", "year")) |>
  mutate(n_events = replace_na(n_events, 0L),
         deaths   = replace_na(deaths, 0),
         conflict = n_events > 0)


# Report =====================================================================

message("Panel rows: ", format(nrow(cluster_conflict), big.mark = ","))
message("Cluster-years with conflict: ",
        format(sum(cluster_conflict$conflict), big.mark = ","),
        " (", round(100 * mean(cluster_conflict$conflict), 1), "%)")
message("Clusters ever exposed: ",
        format(n_distinct(conflict_sparse$cluster_uid), big.mark = ","),
        " of ", format(nrow(clusters), big.mark = ","))


# Save =======================================================================

saveRDS(cluster_conflict, here("data_derived", "mortality_cluster_conflict.rds"))

# Fin
