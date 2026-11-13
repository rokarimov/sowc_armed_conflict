# Project: SOWC 2026
# Goal: Build annual population rasters 2000-2024
# Author: Rouslan Karimov
# Organization: UNICEF
# Created: 30 May 2026

library(here)
library(terra)
library(sf)
library(tidyverse)
library(wpp2024)
library(exactextractr)
library(countrycode)
library(rnaturalearth)


# Use all cores for terra arithmetic and GDAL I/O; exactextractr is already
# multi-threaded via OpenMP.
terraOptions(threads = TRUE)


# Parameters ====
years          <- 2000:2025
snapshot_years <- c(2000, 2005, 2010, 2015, 2020)

snapshot_paths <- here(
  "data", "Earthdata", "popcounts_wpp_v11",
  paste0("gpw_v4_population_count_rev11_", snapshot_years, "_30_sec.tif")
)

outdirs <- c(
  nearest = here("data_derived", "popcounts_interpolated", "scaled_nearest"),
  linterp = here("data_derived", "popcounts_interpolated", "scaled_linterp")
)
for (d in outdirs) if (!dir.exists(d)) dir.create(d, recursive = TRUE)


# Load inputs ====
snaps <- rast(snapshot_paths)
names(snaps) <- as.character(snapshot_years)

# Get country geographies
countries <- ne_countries(scale = "medium", returnclass = "sf") |>
  # Natural Earth leaves iso_a3 == "-99" for France and Norway; their code lives
  # only in iso_a3_eh. Filtering/renaming on iso_a3_eh keeps them in (so they get
  # rescaled to WPP) instead of being silently dropped.
  filter(iso_a3_eh != "-99") |>
  rename(iso3 = iso_a3_eh) |>
  group_by(iso3) |>
  summarise(.groups = "drop") |>
  st_make_valid()

# WPP total population per country per year, in persons (popAge1dt is in
# thousands). Year-y raster is scaled to year-(y-1) WPP totals because wpp2024 package
# years are one year behind published WPP years, per documentation.
data(popAge1dt)

wpp_totals <- popAge1dt |>
  filter(year %in% (years - 1), country_code < 900) |>
  summarise(wpp_tot_pop = sum(pop, na.rm = TRUE),
            .by = c(country_code, year)) |>
  mutate(iso3 = countrycode(country_code, "un", "iso3c", warn = FALSE),
         target_year = year + 1) |>
  filter(!is.na(iso3)) |>
  select(iso3, target_year, wpp_tot_pop)


# Per-year change grids for the linterp base ====
gap_lens <- diff(snapshot_years)

change_grids <- rast(lapply(
  seq_len(nlyr(snaps) - 1),
  \(i) (snaps[[i + 1]] - snaps[[i]]) / gap_lens[i]
))


# Base-raster builders ====
BaseRasterNearest <- function(y) {
  anchor_year <- min(floor(y / 5) * 5, max(snapshot_years))
  snaps[[as.character(anchor_year)]]
}

BaseRasterLinterp <- function(y) {
  anchor_ix <- pmin(findInterval(y, snapshot_years), nlyr(snaps))
  chg_ix    <- pmin(anchor_ix, nlyr(snaps) - 1)
  snaps[[anchor_ix]] + change_grids[[chg_ix]] * (y - snapshot_years[anchor_ix])
}


# Per-country WPP rescaling ====
# Parallelism here comes from (1) exactextractr's OpenMP-threaded extraction
# over polygons, and (2) terra's multi-threaded arithmetic and GDAL writes
# enabled by terraOptions(threads = TRUE) above and the NUM_THREADS GDAL
# option in writeRaster. Across-year parallelism is not used: each year
# already saturates threads and would otherwise compete for memory and I/O.
ScaleToWPP <- function(base, y) {
  grid_tot <- exact_extract(base, countries, fun = "sum", progress = FALSE)
  scale_vec <- tibble(iso3 = countries$iso3, grid_tot_pop = grid_tot) |>
    left_join(wpp_totals |> filter(target_year == y), by = "iso3") |>
    mutate(scale = (1000 * wpp_tot_pop) / grid_tot_pop,
           scale = if_else(is.finite(scale), scale, 1)) |>
    pull(scale)
  scale_rast <- rasterize(
    vect(countries |> mutate(scale = scale_vec)),
    base, field = "scale", background = 1
  )
  base * scale_rast
}


# Driver loop ====
# Prompt: skip or overwrite existing outputs
existing <- unlist(lapply(outdirs, list.files, pattern = "\\.tif$"))

skip_existing <- TRUE

if (length(existing) > 0) {
  ans <- utils::menu(
    c("Skip existing files (resume build)",
      "Overwrite existing files"),
    title = sprintf("Found %d existing .tif file(s) in output folders. What do you want to do?",
                    length(existing))
  )
  skip_existing <- (ans != 2)
}


methods <- list(nearest = BaseRasterNearest, linterp = BaseRasterLinterp)
for (m in names(methods)) {
  for (y in years) {
    out_path <- file.path(outdirs[[m]], sprintf("pop_%d.tif", y))
    if (file.exists(out_path) && skip_existing) {
      message("Skip (exists): ", basename(out_path), " [", m, "]")
      next
    }
    message("Building ", basename(out_path), " [", m, "]")
    base <- methods[[m]](y)
    out  <- ScaleToWPP(base, y)
    writeRaster(out, out_path, overwrite = TRUE,
                gdal = c("COMPRESS=LZW", "TILED=YES", "NUM_THREADS=ALL_CPUS"))
  }
}
