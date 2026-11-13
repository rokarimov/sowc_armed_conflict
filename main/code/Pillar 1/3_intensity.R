# Project: SOWC 2026
# Goal: Replicate PRIO's "children at risk" intensity breakdown, 1996-2024.
# Author: Rouslan Karimov
# Organization: UNICEF
# Created: 8 June 2026

# Load libraries
library(here)
library(terra)
library(wpp2024)
library(tidyverse)
library(sf)
library(countrycode)
library(rnaturalearth)
library(extrafont)
library(ggrepel)
library(furrr)
library(packcircles)
library(showtext)  # For NotoSans
font_add_google("Noto Sans")
showtext_auto()


# Parameters ====
years      <- 1996:2025
pop_method <- "globpop"   # If using CIESIN-based data, "linterp" or "nearest"
                          # selects which set of pre-built annual rasters to load.
                          # GlobPOP doesn't require these methods b/c no interpolation.

# UNICEF color palette
unicefcol <- c('#00AEEF', '#9ADBE8', '#0047BB', '#004C45', '#FFB500', '#FF8200', '#961A49', '#E2231A')


# Intensity categories: cumulative battle-deaths within 50 km of a pixel over the year
int_cat_breaks <- c(1, 25, 100, 1000, Inf)
int_cat_labels <- c("Low (1–24)", "Medium (25–99)",
                 "High (100–999)", "Extreme (1000+)")

# DUG L1 urbanicity codes -> labels
urb_labels <- c("1" = "Rural", "2" = "Urban cluster/town", "3" = "Urban centre/city")


# Load data ====

load(here("data_derived", paste0("num_children_in_confl_", pop_method, ".RData")))

# UNICEF-approved geographic names
area <- readxl::read_xlsx(here("data", "areas.xlsx"))

# Per-event 50 km buffers in EPSG:4326, clipped to present-day borders (built in
# replicate_prio_0_ged_prep.R).
conflict_buffers_clipped <- readRDS(
  here("data_derived", "conflict_buffers_clipped.RDS"))


# Paths to pre-built population rasters
pop_dir   <- here("data_derived", "popcounts_interpolated",
                  paste0("scaled_", pop_method))
pop_files <- list.files(pop_dir, pattern = "\\.tif$", full.names = TRUE)
names(pop_files) <- as.character(years)

# Per-year urbanicity (DUG L1) rasters, named by year. Years with no match
# (pre-2015) skip the urbanicity split.
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


# Country boundaries, from Natural Earth
countries <- ne_countries(scale = "medium", returnclass = "sf") |>
  # NE codes these -99; fold them into the UN-recognised country (Kosovo kept
  # separate, as in areas.xlsx).
  mutate(iso_a3_eh = case_when(adm0_a3 == "KOS" ~ "KOS",
                               adm0_a3 == "SOL" ~ "SOM",   # Somaliland
                               .default = iso_a3_eh)) |>
  filter(iso_a3_eh != "-99") |>
  rename(iso3 = iso_a3_eh) |>
  group_by(iso3) |>
  summarise(.groups = "drop") |>
  st_make_valid()

# Project to equal-area (Eckert VI)
countries_eck <- countries |>
  st_transform("+proj=eck6") |>
  st_make_valid()

# Function to process single year ====
ProcessYear <- function(y, buffered, wpp_shares) {
  message("Processing year: ", y)

  # Pre-built annual raster.
  pop <- rast(pop_files[[as.character(y)]])

  # Year-matched urbanicity raster, or NULL when none exists for this year.
  urb <- if (as.character(y) %in% names(urb_files)) {
    rast(urb_files[[as.character(y)]])
  } else NULL

  # buffered: pre-clipped buffers for this year (built in script 0; already in
  # EPSG:4326). NOT unioned: overlapping buffers must stay separate so a pixel
  # can accumulate the deaths of every event within 50 km of it.
  if (nrow(buffered) == 0) return(NULL)

  # Count children per intensity category for one country.
  CountByCountry <- function(sub) {
    iso3_code <- sub$iso3[1]
    cntry     <- sub$country[1]

    bufs <- sub[!st_is_empty(sub), ]
    if (nrow(bufs) == 0) return(NULL)

    v <- vect(bufs)

    # Crop pop to the buffers' extent; buffers are already clipped to the
    # country's borders in 0_ged_prep.R.
    pop_c <- crop(pop, ext(v))
    if (ncell(pop_c) == 0) return(NULL)

    # Per-pixel score = SUM of numdeaths over every buffer covering the pixel =
    # total battle-deaths within 50 km
    score <- rasterize(v, pop_c, field = "numdeaths", fun = "sum", background = 0)

    # Bucket each pixel into exactly one int_cat by its score. Pixels with score 0
    # (< the lowest break) become NA and drop out.
    rcl  <- cbind(int_cat_breaks[-length(int_cat_breaks)],
                  int_cat_breaks[-1],
                  seq_along(int_cat_labels))
    int_cat <- classify(score, rcl, right = FALSE, others = NA)

    # No urbanicity for this year: count by intensity only.
    if (is.null(urb)) {
      zs <- zonal(pop_c, int_cat, fun = "sum", na.rm = TRUE)
      names(zs) <- c("int_cat_id", "conflictpop")
      zs <- zs[!is.na(zs$int_cat_id) & zs$conflictpop > 0, , drop = FALSE]
      if (nrow(zs) == 0) return(NULL)
      return(tibble(
        country     = cntry,
        iso3        = iso3_code,
        year        = y,
        int_cat     = int_cat_labels[zs$int_cat_id],
        urbanicity  = NA_character_,
        conflictpop = zs$conflictpop
      ))
    }

    # Cross intensity with urbanicity via a composite zone (int_cat*10 + urb code).
    # The DEGURBA crop is reprojected onto the pop_c grid (categorical -> nearest).
    urb_c <- project(crop(urb, project(ext(pop_c), from = crs(pop_c), to = crs(urb))),
                     pop_c, method = "near")
    zone  <- int_cat * 10 + urb_c
    zs <- zonal(pop_c, zone, fun = "sum", na.rm = TRUE)
    names(zs) <- c("zone_id", "conflictpop")
    zs <- zs[!is.na(zs$zone_id) & zs$conflictpop > 0, , drop = FALSE]
    if (nrow(zs) == 0) return(NULL)

    tibble(
      country     = cntry,
      iso3        = iso3_code,
      year        = y,
      int_cat     = int_cat_labels[zs$zone_id %/% 10],
      urbanicity  = unname(urb_labels[as.character(zs$zone_id %% 10)]),
      conflictpop = zs$conflictpop
    )
  }

  out <- buffered |>
    group_split(iso3) |>
    map(CountByCountry) |>
    compact() |>
    list_rbind()
  if (nrow(out) == 0) return(NULL)

  out |>
    left_join(wpp_shares, by = "iso3") |>
    mutate(children_in_conflict = conflictpop * share_child,
           share_at_risk = children_in_conflict / (childpop * 1000))
}


# Run for all years ====

# Slice the heavy buffers and WPP shares by year up front, so each worker gets
# only its own year rather than the whole dataset.
buffers_by_year <- map(years, \(y) filter(conflict_buffers_clipped, year == y))

# Country-specific under-18 shares from WPP, one tibble per year.
wpp_by_year <- map(years, \(y) {
  popAge1dt |>
    filter(year == y - 1, country_code < 900) |>
    summarise(
      total_pop   = sum(pop, na.rm = TRUE),
      childpop    = sum(pop[age < 18], na.rm = TRUE),
      share_child = childpop / total_pop,
      .by = country_code
    ) |>
    mutate(iso3 = countrycode(country_code, "un", "iso3c", warn = FALSE,
                              custom_match = c("412" = "KOS"))) |>
    filter(!is.na(iso3)) |>
    select(-country_code)
})

# Counts by country-year-intensity (parallelized for speed)
plan(multisession, workers = 6)   # raise/lower to taste; each worker holds a raster

system.time(
confl_intensity_countries <- future_pmap(
  list(years, buffers_by_year, wpp_by_year), ProcessYear,
  .options = furrr_options(
    packages = c("terra", "sf", "dplyr", "tidyr", "purrr", "countrycode"),
    seed = TRUE
  )
) |>
  compact() |>
  list_rbind()
)

plan(sequential)   # release the workers

confl_intensity_countries <- confl_intensity_countries |>
  mutate(int_cat = factor(int_cat, levels = int_cat_labels))

# For Myanmar in 2017, there is a zone in pop raster that is water and thus has no population.
# This same zone (zone 43) is marked as city in urban raster. This produces some NA values
# that we need to filter out. It does not affect the accuracy of counts.
confl_intensity_countries <- confl_intensity_countries |>
  filter(!is.na(int_cat))

# Global totals by year and intensity (summed over urbanicity)
confl_intensity_global <- confl_intensity_countries |>
  summarize(children_confl = sum(children_in_conflict, na.rm = TRUE),
            .by = c(year, int_cat))

# Global totals by year, intensity and urbanicity (DEGURBA years carry the split)
confl_intensity_urb_global <- confl_intensity_countries |>
  summarize(children_confl = sum(children_in_conflict, na.rm = TRUE),
            .by = c(year, int_cat, urbanicity))


# Conflict intensity as experienced by average in-zone child ====
pop     <- rast(pop_files[[as.character(2025)]])

# Per country: population-weighted mean per-pixel score over conflict pixels.
AvgIntensityByCountry <- function(country_buf) {
  v     <- vect(country_buf)
  pop_c <- crop(pop, ext(v))
  if (ncell(pop_c) == 0) return(NULL)
  
  # Same per-pixel score as CountByCountry: total battle-deaths within 50 km.
  score <- rasterize(v, pop_c, field = "numdeaths", fun = "sum", background = 0)
  
  # Restrict to conflict pixels (score >= 1) and take the population-weighted mean
  # score = intensity experienced by the average conflict-exposed child.
  conflict <- classify(score, cbind(1, Inf, 1), right = FALSE, others = NA)
  pop_w    <- mask(pop_c, conflict)
  num      <- global(score * pop_w, "sum", na.rm = TRUE)[[1]]
  den      <- global(pop_w, "sum", na.rm = TRUE)[[1]]
  if (is.na(den) || den == 0) return(NULL)
  
  tibble(iso3 = country_buf$iso3[1], avg_intensity = num / den)
}


avg_intensity <- conflict_buffers_clipped |>
  filter(year == 2025) |>
  group_split(iso3) |>
  map(AvgIntensityByCountry) |>
  compact() |>
  list_rbind() |>
  mutate(
    int_cat = factor(int_cat_labels[findInterval(avg_intensity, int_cat_breaks)],
                     levels = int_cat_labels)
  ) |>
  left_join(select(area, iso3, country), by = "iso3")


avg_intensity <- confl_num_countries |>
  st_drop_geometry() |>
  filter(year == 2025) |>
  select(iso3, children_in_conflict) |>
  right_join(avg_intensity, by = "iso3")


# Save to disk
save(
  confl_intensity_countries, confl_intensity_global, confl_intensity_urb_global, avg_intensity,
  file = here("data_derived", paste0("num_children_in_confl_intensity_", pop_method, ".RData"))
)

# Load from disk
load(here("data_derived", paste0("num_children_in_confl_intensity_", pop_method, ".RData")))


# Visualize ====

intensity_palette <- setNames(unicefcol[c(1, 5, 8, 7)], int_cat_labels)

# Stacked area of children in conflict by intensity over time.
confl_intensity_global |>
  mutate(cumnum = cumsum(children_confl), .by = year) |>
  ggplot(aes(x = year, y = children_confl, fill = fct_rev(int_cat))) +
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
  scale_fill_manual("Conflict intensity\n(# deaths)",
                    values = rev(unicefcol[c(1, 4, 2, 5)])) +
  labs(y = "Number of children (millions)") +
  scale_y_continuous(
    limits = c(0, 600000000),
    breaks = seq(0, 600000000, by = 100000000),
    labels = seq(0, 600, by = 100)
  ) +
  scale_x_continuous(
    breaks = seq(1996, 2026, 2),
    labels = seq(1996, 2026, 2)) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        text = element_text(family = "Noto Sans"))


# Visualize average intensity ====
# Packed circles viz of average intensity and number of children under conflict
pack <- circleProgressiveLayout(avg_intensity$children_in_conflict, sizetype = "area")
dat <- cbind(avg_intensity, pack)              # adds x, y, radius
dat <- dat |> mutate(country = case_when(country == "Democratic Republic of the Congo (the)" ~ "DR Congo", .default = country))
verts <- circleLayoutVertices(pack, npoints = 50)
verts$int_cat <- dat$int_cat[verts$id]

ggplot() +
  geom_polygon(data = verts,
               aes(x, y, group = id, fill = int_cat),
               colour = "white", alpha = 0.9) +
  geom_text(data = dat |> slice_max(children_in_conflict, n = 10),
            aes(x, y, label = country), size = 4) +
  geom_text_repel(data = dat |> filter(iso3 %in% c("UKR", "PSE")),
                  aes(x, y, label = country),
                  hjust = 1.9, vjust = -1) +
  scale_fill_manual(values = intensity_palette) +
  labs(fill = "Intensity") +
  coord_equal() +
  theme_void() +
  theme(text = element_text(family = "Noto Sans"))

# Same in a bar chart
avg_intensity |>
  filter(avg_intensity > 99) |>  # Pick top 15 countries by intensity
  ggplot(aes(x = children_in_conflict, y = reorder(country, children_in_conflict), fill = int_cat)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(children_in_conflict /  1e6, 1)), hjust = 0) +
  scale_fill_discrete("Conflict intensity\n(# deaths)", palette = intensity_palette[2:3]) +
  scale_x_continuous(
    limits = c(0, 27e6),
    breaks = seq(0, 27e6, by = 5e6),
    labels = seq(0, 27, by = 5)
  ) +
  labs(x = "Children at risk (millions)") +
  theme_bw() +
  theme(text = element_text(family = "Noto Sans", size = 14),
        axis.title.y = element_blank())
