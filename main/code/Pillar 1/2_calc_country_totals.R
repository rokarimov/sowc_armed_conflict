# Project: SOWC 2026
# Goal: Replicate PRIO calculation of # children in conflict zones, 1996-2025.
#       Use PRIO's method of duplicating events in pre-cession countries and
#       assigning them also to the post-cession new state to account for
#       new state formation, cross-border spillover and contested territories.
# Author: Rouslan Karimov
# Organization: UNICEF
# Created: 4 June 2026

# Load libraries
library(here)
library(terra)
library(wpp2024)
library(tidyverse)
library(scales)
library(sf)
library(countrycode)
library(ggrepel)
library(furrr)
library(showtext)  # For NotoSans
font_add_google("Noto Sans")
showtext_auto()

# Parameters ====
years      <- 1996:2025

# Select which type of rasters to load. Options: "globpop", "linterp" or "nearest".
pop_method <- "globpop"

# UNICEF color palette
unicefcol <- c('#00AEEF', '#9ADBE8', '#0047BB', '#004C45', '#FFB500', '#FF8200', '#961A49', '#E2231A')


# Load data ====

# Per-event 50 km buffers, clipped to present-day borders (built in
# 0_ged_prep.R). One row per clipped buffer, in EPSG:4326,
# carrying country, iso3, year and the event's battle-death count (best).
conflict_buffers_clipped <- readRDS(
  here("data_derived", "conflict_buffers_clipped.RDS"))

# UNICEF-approved geographic names
area <- readxl::read_xlsx(here("data", "areas.xlsx"))

# Paths to pre-built population rasters
pop_dir   <- here("data_derived", "popcounts_interpolated",
                  paste0("scaled_", pop_method))
pop_files <- list.files(pop_dir, pattern = "\\.tif$", full.names = TRUE)
names(pop_files) <- as.character(years)

# Per-year urbanicity (DUG L1) rasters, named by year. Years with no match
# (pre-2015) fall back to a single undisaggregated total.
urb_dir   <- here("data_derived", "degurba")
urb_files <- list.files(urb_dir, pattern = "\\.tif$", full.names = TRUE)
names(urb_files) <- str_match(basename(urb_files), "_(\\d{4})\\.tif$")[, 2]


# Age-specific counts for age distributions
data(popAge1dt)

# Population estimates only go to 2023; for 2024, we need to incorporate projections
data(popprojAge1dt)
popAge1dt <- bind_rows(popAge1dt, 
                       popprojAge1dt |> filter(year == 2024) |> mutate(year = as.integer(year))) |>
  select(any_of(names(popAge1dt)))


# Per-year buffer slices, so each worker receives only its year's geometry instead
# of the full (large) buffer set broadcast as a global.
bufs_by_year <- split(conflict_buffers_clipped, conflict_buffers_clipped$year)


# Function to process single year ====
ProcessYear <- function(y, bufs_y) {
  message("Processing year: ", y)

  # Pre-built annual raster.
  pop <- rast(pop_files[[as.character(y)]])

  # Year-matched urbanicity raster, or NULL when none exists for this year
  urb <- if (as.character(y) %in% names(urb_files)) {
    rast(urb_files[[as.character(y)]])
  } else NULL

  # Country-specific under-18 shares from WPP for this year
  wpp_shares <- popAge1dt |>
    filter(year == y - 1, country_code < 900) |>
    summarise(
      total_pop   = sum(pop, na.rm = TRUE),
      childpop    = sum(pop[age < 18], na.rm = TRUE),
      share_child = childpop / total_pop,
      childpop_0_4    = sum(pop[age < 5], na.rm = TRUE),
      share_child_0_4 = childpop_0_4 / total_pop,
      childpop_5_9    = sum(pop[age > 4 & age < 10], na.rm = TRUE),
      share_child_5_9 = childpop_5_9 / total_pop,
      childpop_10_14    = sum(pop[age > 9 & age < 15], na.rm = TRUE),
      share_child_10_14 = childpop_10_14 / total_pop,
      childpop_15_17    = sum(pop[age > 14 & age < 18], na.rm = TRUE),
      share_child_15_17 = childpop_15_17 / total_pop,
      .by = country_code
    ) |>
    mutate(iso3 = countrycode(country_code, "un", "iso3c", warn = FALSE,
                              custom_match = c("412" = "KOS"))) |>
    filter(!is.na(iso3)) |>
    select(-country_code)

  if (is.null(bufs_y) || nrow(bufs_y) == 0) return(NULL)

  # Per country: use the same clipped buffers for both the population count and the
  # mapping geometry. For countries that split, pre-split events were copied onto their
  # successors in 0_ged_prep.R and coded to the successor's modern ISO3, so each was
  # clipped to its own modern polygon and the population splits geographically.
  # Grouped by iso3 alone, so originals and copies for one country form a single
  # group and overlapping population is counted once.
  ProcessCountry <- function(sub) {
    bufs <- sub[!st_is_empty(sub), ]
    if (nrow(bufs) == 0) return(NULL)

    # Dissolve buffers into one polygon, used for the mapping output and as the
    # boundary for the urbanicity area split. Keep only the polygonal part:
    # dissolving overlapping discs can leave a stray zero-width LINESTRING that
    # would double-count the country.
    geom <- st_union(st_geometry(bufs))
    geom <- st_combine(st_collection_extract(geom, "POLYGON"))

    conflictpop <- 0
    urb_tbl     <- NULL
    pop_c <- crop(pop, ext(vect(bufs)))
    if (ncell(pop_c) > 0) {
      score    <- rasterize(vect(bufs), pop_c, field = "numdeaths", fun = "sum", background = 0)
      cover    <- classify(score, cbind(1, Inf, 1), right = FALSE, others = NA)
      pop_conf <- mask(pop_c, cover)
      s        <- global(pop_conf, "sum", na.rm = TRUE)[[1]]
      if (length(s) == 1 && !is.na(s)) conflictpop <- s

      # Split by urbanicity: population from the raster zonal; area from a vector
      # intersection of the buffer with class polygons (areas sum to buffer area).
      if (!is.null(urb)) {
        urb_c <- urb |>
          crop(project(ext(pop_c), from = crs(pop_c), to = crs(urb))) |>
          project(pop_c, method = "near")

        zs <- zonal(pop_conf, urb_c, fun = "sum", na.rm = TRUE)
        names(zs) <- c("urbanicity", "conflictpop")
        zs <- zs[!is.na(zs$urbanicity) & zs$conflictpop > 0, , drop = FALSE]
        if (nrow(zs) > 0) {
          # Mask to the conflict zone before polygonizing: polygonizing the full
          # bounding box (e.g. all of India) is very slow and is clipped away
          # below anyway. touches = TRUE keeps edge cells, so areas are unchanged.
          urb_poly <- mask(urb_c, vect(geom), touches = TRUE) |>
            as.polygons(dissolve = TRUE) |>
            st_as_sf() |>
            st_transform("+proj=moll") |>
            st_make_valid()
          names(urb_poly)[1]  <- "urbanicity"
          urb_poly$urbanicity <- as.character(urb_poly$urbanicity)

          # Intersect in projected CRS (GEOS) so the overlay never reaches s2.
          geom_moll <- st_make_valid(st_transform(st_sf(geometry = geom), "+proj=moll"))
          urb_poly  <- st_intersection(urb_poly, geom_moll)

          # Calculate area for each urbanicity category
          urb_poly$area <- st_area(urb_poly)

          urb_tbl <- urb_poly |>
            st_drop_geometry() |>
            inner_join(tibble(urbanicity  = as.character(zs$urbanicity),
                              conflictpop = zs$conflictpop),
                       by = "urbanicity") |>
            mutate(country    = sub$country[1],
                   iso3       = sub$iso3[1],
                   year       = y,
                   urbanicity = factor(urbanicity,
                     levels = c("Rural", "Urban cluster/town", "Urban centre/city")))
        }
      }
    }

    total <- st_sf(
      country     = sub$country[1],
      iso3        = sub$iso3[1],
      year        = y,
      conflictpop = conflictpop,
      geometry    = geom
    )

    list(total = total, urb = urb_tbl)
  }

  out <- bufs_y |>
    group_split(iso3) |>
    map(ProcessCountry) |>
    compact()
  if (length(out) == 0) return(NULL)

  # Country totals (one row per country, with dissolved conflict-zone geometry).
  totals <- do.call(rbind, map(out, "total")) |>
    left_join(wpp_shares, by = "iso3") |>
    mutate(children_in_conflict = conflictpop * share_child,
           share_at_risk = children_in_conflict / (childpop * 1000),
           children_in_conflict_0_4   = conflictpop * share_child_0_4,
           children_in_conflict_5_9   = conflictpop * share_child_5_9,
           children_in_conflict_10_14 = conflictpop * share_child_10_14,
           children_in_conflict_15_17 = conflictpop * share_child_15_17)

  # Urbanicity breakdown (one row per country-class with conflict-zone area;
  # geometry dropped in-loop; NULL pre-2015).
  urb_list <- map(out, "urb") |> compact()
  urb <- if (length(urb_list) > 0) {
    list_rbind(urb_list) |>
      left_join(wpp_shares, by = "iso3") |>
      mutate(children_in_conflict = conflictpop * share_child,
             share_at_risk = children_in_conflict / (childpop * 1000))
  } else NULL

  list(totals = totals, urb = urb)
}

# Run for all years ====

# Counts by country-year (parallelized for speed)
plan(multisession, workers = 6)   # raise/lower to taste; each worker holds a raster

system.time(
results <- future_map2(
  years, bufs_by_year[as.character(years)], ProcessYear,
  .options = furrr_options(
    packages = c("terra", "sf", "dplyr", "purrr", "countrycode"),
    seed = TRUE,
    # One future per year, so the heavy recent years (2021-2025) are spread
    # across workers rather than chunked onto the last one
    scheduling = Inf
  )
) |>
  Filter(Negate(is.null), x = _)
)

plan(sequential)   # release the workers

confl_num_countries <- map(results, "totals") |> do.call(rbind, args = _)

# Per-class conflict zones (one row per country-year-urbanicity, with area)
urb_list      <- map(results, "urb") |> compact()
confl_num_urb <- if (length(urb_list) > 0) list_rbind(urb_list) else NULL


# Global totals by year and age group (age shares exist for every year, 1996-2025)
confl_num_age_global <- confl_num_countries |>
  st_drop_geometry() |>
  summarize(across(c(children_in_conflict_0_4, children_in_conflict_5_9, 
                     children_in_conflict_10_14, children_in_conflict_15_17,),
                   \(x) sum(x, na.rm = TRUE)),
            .by = year) |>
  pivot_longer(-year, names_to = "age_group", values_to = "children_confl",
               names_prefix = "children_in_conflict_") |>
  mutate(age_group = factor(age_group,
                            levels = c("0_4", "5_9", "10_14", "15_17"),
                            labels = c("0–4 years", "5–9 years", "10–14 years", "15–17 years")))


# Headline global total = sum across the four age groups (= the under-18 total)
childpop_glob <- popAge1dt |>
  filter(country_code < 900) |>
  summarise(
    childpop    = sum(pop[age < 18], na.rm = TRUE),
    .by = year
  ) |>
  mutate(year = year + 1)

confl_num_global <- confl_num_countries |>
  st_drop_geometry() |>
  summarize(
    children_confl = sum(children_in_conflict, na.rm = TRUE),
    .by = year
    ) |>
  left_join(childpop_glob, by = "year") |>
  mutate(share_at_risk = children_confl/childpop/1000)

# Global child population

# Global totals by year and urbanicity (years with DEGURBA coverage only)
confl_num_urb_global <- confl_num_urb |>
  summarize(
    children_confl_urb = sum(children_in_conflict, na.rm = TRUE),
    .by = c(year, urbanicity)) |>
  left_join(select(confl_num_global, year, children_confl), by = "year") |>
  mutate(share_degurb = children_confl_urb / children_confl)


# Save to disk
save(
  confl_num_countries, confl_num_global, confl_num_age_global,
  confl_num_urb, confl_num_urb_global,
  file = here("data_derived", paste0("num_children_in_confl_", pop_method, ".RData"))
  )


# Load from disk ====
load(here("data_derived", paste0("num_children_in_confl_", pop_method, ".RData")))
# UNICEF-approved geographic names
area <- readxl::read_xlsx(here("data", "areas.xlsx"))


# Visualize ====

# Global number of children at risk
ggplot(confl_num_global) +
  geom_line(aes(x = year, y = children_confl), colour = "blue", linewidth = 1) +
  geom_text_repel(
    aes(x = year,
        y = case_when(year %in% c(2008, 2013, 2017, 2020, 2025) ~ children_confl,
                      .default = NA),
        label = round(children_confl / 1e+6, 0),
        vjust = -1,
        family = "Noto Sans"
        )) +
  geom_text_repel(
    aes(x = year,
        y = case_when(year %in% c(1996, 2010, 2014) ~ children_confl,
                      .default = NA),
        label = round(children_confl / 1e+6, 0),
        vjust = 2,
        family = "Noto Sans"
    )) +
  labs(y = "Number of children (millions)") +
  scale_y_continuous(
    limits = c(0, 600000000),
    breaks = seq(0, 600000000, by = 100000000),
    labels = seq(0, 600, by = 100)) +
  scale_x_continuous(
    breaks = seq(1996, 2026, 2),
    labels = seq(1996, 2026, 2)) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        text = element_text(size = 14))

# Global share of children at risk
# For ggplot charts, define function that adds a percentage sign after a number in axis labels
percentify <- function (x) {paste0(x, '%')}

ggplot(confl_num_global) +
  geom_line(aes(x = year, y = share_at_risk), colour = "red", linewidth = 1) +
  geom_text_repel(
    aes(x = year,
        y = case_when(year %in% c(2008, 2013, 2017, 2020, 2025) ~ share_at_risk,
                      .default = NA),
        label = label_percent(accuracy = 0.1)(share_at_risk),
        vjust = -1,
        family = "Noto Sans"
    )) +
  geom_text_repel(
    aes(x = year,
        y = case_when(year %in% c(1996, 2010, 2014) ~ share_at_risk,
                      .default = NA),
        label = label_percent(accuracy = 0.1)(share_at_risk),
        vjust = 2,
        family = "Noto Sans"
    )) +
  labs(y = "Percent children at risk") +
  scale_y_continuous(
    limits = c(0, 0.25),
    labels = label_percent()) +
  scale_x_continuous(
    breaks = seq(1996, 2026, 2),
    labels = seq(1996, 2026, 2)) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        text = element_text(size = 14))


# Children at risk by age group
confl_num_age_global |>
  mutate(cumnum = cumsum(children_confl), .by = year) |>
  ggplot(aes(x = year, y = children_confl, fill = fct_rev(age_group))) +
  geom_area() +
  geom_text_repel(
    aes(x = year,
        y = case_when(year == 1996 ~ cumnum,
                      .default = NA),
        hjust = 0,
        label = round(cumnum / 1e+6, 0),
        family = "Noto Sans"
    )) +
  geom_text_repel(
    aes(x = year,
        y = case_when(year == 2025 ~ cumnum,
                      .default = NA),
        hjust = 1,
        label = round(cumnum / 1e+6, 0),
        family = "Noto Sans"
    )) +
  labs(y = "Number of children (millions)") +
  scale_y_continuous(labels = \(x) x / 1e+6) +
  scale_x_continuous(breaks = seq(1996, 2026, 2)) +
  scale_fill_manual("Age group", values = unicefcol[c(1, 4, 2, 5, 6, 8, 7, 3)],
                    breaks = c("0–4 years", "5–9 years", "10–14 years", "15–17 years")) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        text = element_text(family = "Noto Sans", size = 14))


# Children at risk by SDG region
df_reg <- confl_num_countries |>
  st_drop_geometry() |>
  left_join(select(area, iso3, sdgregcode), by = "iso3") |>
  summarise(children_in_conflict = as.numeric(sum(children_in_conflict, na.rm = TRUE)),
            .by = c(sdgregcode, year)) |>
  complete(sdgregcode, year, fill = list(children_in_conflict = 0))  # Provide missing years for Oceania, with 0 kids exposed


df_reg |>
  mutate(label = ifelse(year == max(year), sdgregcode, NA_character_)) |>
  ggplot(aes(x = year, y = children_in_conflict, colour = sdgregcode)) +
  geom_line(linewidth = 1) +
  scale_colour_manual("Region", values = unicefcol[c(1, 4, 2, 5, 6, 8, 7, 3)], guide = "none") +
  geom_text_repel(
    aes(x = year,
        y = case_when(year == 1996 ~ children_in_conflict,
                      .default = NA),
        hjust = 0,
        label = round(children_in_conflict / 1e+6, 0),
        family = "Noto Sans"
    )) +
  geom_text_repel(
    aes(x = year,
        y = case_when(year == 2025 ~ children_in_conflict,
                      .default = NA),
        hjust = 1,
        label = round(children_in_conflict / 1e+6, 0),
        family = "Noto Sans"
    )) +
  geom_text_repel(aes(label = label), 
                  min.segment.length = Inf,
                  hjust = 0, nudge_x = 1.5, family = "Noto Sans", segment.size = 0) +
  scale_y_continuous(labels = \(x) x / 1e+6) +
  scale_x_continuous(
    limits = c(1996, 2032),
    breaks = seq(1996, 2032, 2),
    labels = seq(1996, 2032, 2)) +
  labs(y = "Number of children (millions)") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        text = element_text(family = "Noto Sans", size = 14))


# Children in conflict by urbanicity (DEGURBA years) ====
confl_num_urb_global |>
  mutate(cumnum = cumsum(children_confl_urb), .by = year) |>
  ggplot(aes(x = year, y = children_confl_urb, fill = fct_rev(urbanicity))) +
  geom_area() +
  geom_text_repel(
    aes(x = year,
        y = case_when(year == 2015 ~ cumnum,
                      .default = NA),
        hjust = 0,
        label = round(cumnum / 1e+6, 0),
        family = "Noto Sans"
    )) +
  geom_text_repel(
    aes(x = year,
        y = case_when(year == 2025 ~ cumnum,
                      .default = NA),
        hjust = 1,
        label = round(cumnum / 1e+6, 0),
        family = "Noto Sans"
    )) +
  labs(y = "Number of children (millions)") +
  scale_y_continuous(labels = \(x) x / 1e+6) +
  scale_x_continuous(
    breaks = seq(2015, 2027, 1)) +
  scale_fill_manual("Urbanicity", values = unicefcol[c(1, 5, 7)]) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        text = element_text(family = "Noto Sans", size = 14))

ggsave(here("outputs", "urban.eps"), width = 3000, height = 1500, units = "px")

