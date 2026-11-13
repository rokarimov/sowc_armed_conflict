# Project: SOWC 2026
# Goal: Assemble the master analytic dataset — one row per birth, with survival
#       outcomes, conflict exposure and cluster covariates
# Author: Rouslan Karimov
# Organization: UNICEF
# Created: 31 July 2026


library(here)
library(tidyverse)


# Load data ==================================================================

dhs      <- readRDS(here("data_derived", "mortality_dhs_births.rds"))
mics     <- readRDS(here("data_derived", "mortality_mics_births.rds"))
conflict <- readRDS(here("data_derived", "mortality_cluster_conflict.rds"))
covars   <- readRDS(here("data_derived", "mortality_cluster_covariates.rds"))

births <- bind_rows(dhs, mics)


# Chronicity =================================================================

# Wagner's chronicity measure is the number of *consecutive* years of conflict
# in the cluster leading up to the first year of life, censored at 5. It has to
# be computed on the full dense panel before any subsetting, because a gap year
# resets the count and a missing row would be indistinguishable from a gap.
#
# sequence(rle(x)$lengths) numbers the positions within each run of equal
# values (1,2,3,1,2,...); multiplying by the 0/1 conflict flag zeroes out the
# peaceful runs and leaves the running streak length on the conflict ones.
conflict <- conflict |>
  arrange(cluster_uid, year) |>
  mutate(streak = sequence(rle(conflict)$lengths) * as.integer(conflict),
         .by = cluster_uid)


# Attach conflict exposure ===================================================

# Year of life 1 is the birth year, matching Wagner's D_lct. Year of life 2 is
# birth_year + 1, which is the only other year his under-5 analysis finds to
# matter (Table S7: 5.875 and 1.855 per 1,000 for years 1 and 2, then nothing).
# Years 3-5 stay in the panel for stage 3 to join if wanted.
master <- births |>
  left_join(
    conflict |>
      select(cluster_uid, year,
             events_50_y1 = n_events, deaths_50_y1 = deaths,
             conf_50_y1 = conflict, streak),
    by = c("cluster_uid", "birth_year" = "year")) |>
  mutate(year_plus1 = birth_year + 1L) |>
  left_join(
    conflict |>
      select(cluster_uid, year,
             events_50_y2 = n_events, deaths_50_y2 = deaths,
             conf_50_y2 = conflict),
    by = c("cluster_uid", "year_plus1" = "year")) |>
  select(-year_plus1)


# Exposure derivations =======================================================

master <- master |>
  mutate(
    # Wagner's fixed death bands, kept verbatim so coefficients are directly
    # comparable to his Figure 2 and Table 2. Recomputing quartiles on our own
    # (global, longer) data is a sensitivity for stage 2, not the default.
    intensity_y1 = cut(deaths_50_y1,
                       breaks = c(-Inf, 0, 4, 18, 74, 1000, Inf),
                       labels = c("none", "1-4", "5-18", "19-74",
                                  "75-1000", ">1000")),

    # Censored at 5: "5 or more consecutive years" is the top category.
    chronicity_y1 = pmin(streak, 5L),

    # GED geocoding starts in 1989, so a birth in 1995 has only six years of
    # conflict history behind it. The ten-year lag structure stage 4 needs is
    # complete only from the 1999 cohort onwards. Wagner has the same gap and
    # says so in his Figure 3 note.
    lag10_complete = birth_year >= 1999,

    # Fixed-effect keys. country_birthmonth is Wagner's seasonality control —
    # "January in Kenya" — and is distinct from a plain month effect.
    country_birthmonth = paste(iso3, birth_month, sep = "_")
  ) |>
  select(-streak)


# Attach cluster covariates ==================================================

master <- master |>
  left_join(covars, by = c("cluster_uid", "birth_year" = "year"))


# Degree of urbanisation =====================================================

# Snapshots are carried to the nearest available year rather than interpolated:
# DEGURBA is categorical, so an interpolated value between "town" and "rural"
# would mean nothing. Currently MICS-only — the DHS geospatial covariate files
# still need downloading, and until they do, DHS rows carry NA here and the
# urban/rural binary remains the fallback stratifier.
degurba_path <- here("data_derived", "mortality_degurba_snapshots.rds")

if (file.exists(degurba_path)) {
  degurba <- readRDS(degurba_path)

  # Resolve the snapshot on the distinct cluster-year combinations rather than
  # on the master itself. Joining ~5M birth rows against six snapshots each and
  # then slicing would need a per-row key, and no combination of the birth
  # columns is reliably unique — two mothers in the same cluster can easily have
  # a child of the same birth_cmc and birth order, which would collapse them.
  # Reducing first sidesteps that and keeps the intermediate ~30x smaller.
  lookup <- master |>
    distinct(iso3, survey_year, cluster_uid, birth_year) |>
    mutate(cluster_no = as.integer(str_remove(cluster_uid, "^[^_]+_"))) |>
    left_join(degurba |> rename(cluster_no = cluster),
              by = c("iso3", "survey_year" = "svy_year", "cluster_no"),
              relationship = "many-to-many") |>
    slice_min(abs(snapshot_year - birth_year), n = 1, with_ties = FALSE,
              by = c("cluster_uid", "birth_year")) |>
    select(cluster_uid, birth_year, degurba)

  master <- left_join(master, lookup, by = c("cluster_uid", "birth_year"))
} else {
  message("SKIPPED DEGURBA — ", degurba_path, " not found")
  master$degurba <- NA_real_
}


# Report =====================================================================

message("Master rows (births): ", format(nrow(master), big.mark = ","))
message("Surveys: ", n_distinct(master$sample_id),
        " in ", n_distinct(master$iso3), " countries")
message("Clusters: ", format(n_distinct(master$cluster_uid), big.mark = ","))
message("Birth years: ", min(master$birth_year), "-", max(master$birth_year))

message("\nOutcome availability after censoring:")
master |>
  summarise(n              = n(),
            n_died_neo     = sum(!is.na(died_neo)),
            n_died_u1      = sum(!is.na(died_u1)),
            n_died_u5      = sum(!is.na(died_u5)),
            imr_per_1000   = round(1000 * mean(died_u1, na.rm = TRUE), 1),
            u5mr_per_1000  = round(1000 * mean(died_u5, na.rm = TRUE), 1),
            nnmr_per_1000  = round(1000 * mean(died_neo, na.rm = TRUE), 1)) |>
  print()

message("\nExposure in first year of life:")
master |>
  count(intensity_y1) |>
  mutate(pct = round(100 * n / sum(n), 2)) |>
  print()

message("\nChronicity (consecutive conflict years to first year of life):")
master |> count(chronicity_y1) |> print()


# Save =======================================================================

saveRDS(master, here("data_derived", "mortality_master.rds"))

# Fin
