# Project: SOWC 2026
# Goal: Prepare the UCDP GED event points that drive conflict exposure in the
#       mortality pipeline (Wagner et al. 2018 replication, stage 1)
# Author: Rouslan Karimov
# Organization: UNICEF
# Created: 31 July 2026


library(here)
library(tidyverse)


# Load data ==================================================================

# GED_cleaned.RDS comes from code/Pillar 1/0_ged_prep.R, which already applies
# the filters Wagner's design needs: best > 0 (his definition of an armed
# conflict event is one with at least one direct death), iso3 harmonisation,
# and the PRIO non-conflict-country exclusions.
ged <- readRDS(here("data_derived", "GED_cleaned.RDS"))


# Deduplicate ================================================================

# Pillar 1 duplicates events across successor and co-belligerent states so that
# a single event can be charged to more than one country's population (Sudan /
# South Sudan, Israel / Palestine, the Yugoslav successors, and so on). That is
# the right call for a country-level population-at-risk count, but it is wrong
# here: our exposure is purely geometric — a cluster is within 50 km of an event
# or it is not — so a duplicated event would be counted twice in the same
# neighbourhood and inflate both the event count and the death total.
#
# 374,192 rows collapse to 355,726 distinct events.
ged_events <- ged |>
  distinct(id, .keep_all = TRUE) |>
  filter(year >= 1989) |>                             # GED geocoding starts 1989
  filter(!is.na(longitude), !is.na(latitude)) |>
  mutate(
    id, year, date_start, best, where_prec,
    longitude, latitude, country, iso3,
    .keep = "none"
  )


# Report =====================================================================

message("Events retained: ", nrow(ged_events),
        " (", min(ged_events$year), "-", max(ged_events$year), ")")
message("Total battle deaths (best): ", format(sum(ged_events$best), big.mark = ","))

# where_prec is kept for the geocoding-precision robustness check (Wagner
# appendix S1.5): levels 1-2 are exact or near-exact points, 3-4 are ADM2/ADM1
# centroids, 5-7 are coarser still. His check drops > 4, and alternatively
# weights each birth by the inverse of the precision code.
message("Precision distribution:")
print(table(ged_events$where_prec, useNA = "ifany"))


# Save =======================================================================

saveRDS(ged_events, here("data_derived", "mortality_ged_events.rds"))

# Fin
