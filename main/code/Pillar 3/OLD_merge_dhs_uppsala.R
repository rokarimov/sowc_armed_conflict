# Project: SOWC 2026
# Goal: Use GED data (Uppsala) to tag DHS and MICS observations as near-conflict or not
# Author: Rouslan Karimov
# Organization: UNICEF
# Created: 29 March 2026


library(here)
library(tidyverse)
library(sf)
library(haven)


# Load data
ged <- readRDS(here("data", "Uppsala GEDEvent_v25_1.rds"))
buffers <- readRDS(here("data_derived", "ged_country_year_buffers.rds"))

# Take a peek at conflicts, their spans and total victim counts
ged |>
#  filter(year > 1995) |>
  filter(active_year == TRUE) |>
  summarise(
    from    = min(year),
    to      = max(year),
    ndeaths = sum(best),
    .by     = c(conflict_new_id, country)
  ) |>
  arrange(desc(ndeaths))


# DHS cluster locations ====

# DHSCLUST matches v001 in all DHS recode files;
# SOURCE == "MIS" rows hold (0, 0) coords and are dropped.
clusters <- st_read(
  here("data", "Mali_2023-24_DHS", "MLGE8AFL - Geographic Data", "MLGE8AFL.shp"),
  quiet = TRUE
) |>
  filter(SOURCE != "MIS", LATNUM != 0, LONGNUM != 0) |>
  st_transform(crs = 4326)



# DHS Individual (IR) dataset ====

# Load data
ir <- read_dta(here("data", "Mali_2023-24_DHS", "MLIR8ADT - Individual", "MLIR8AFL.dta"))

# We look at women 18-49 y.o., to determine incidence of child marriage (CM).
# Algorithm: for each woman, figure out when she was aged 0-17. Check how many times
# out of that period her cluster was inside a conflict buffer. Add those up: that's
# her relevant conflict exposure.




near_conflict <- buffers |>
  filter(country == "Mali", year %in% 1974:2023) |>
  st_intersects(clusters, sparse = FALSE)

colSums(near_conflict)

clusters$near_conflict <- near_conflict


cluster_flags <- clusters |>
  st_drop_geometry() |>
  select(DHSCLUST, near_conflict)











# DHS Children (KR) dataset ====

kr <- read_dta(
  here("data", "Mali_2023-24_DHS", "MLKR8ADT - Children", "MLKR8AFL.dta")
) |>
  left_join(cluster_flags, by = c("v001" = "DHSCLUST"))


# DHS Births (BR) dataset ====

br <- read_dta(
  here("data", "Mali_2023-24_DHS", "MLBR8ADT - Births", "MLBR8AFL.dta")
) |>
  left_join(cluster_flags, by = c("v001" = "DHSCLUST"))

# Save DHS data enriched with conflict data to file
save(ir, kr, br, file = here("data_derived", "dhs_conflict_data.RData"))

# Fin
