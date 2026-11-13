# Project: SOWC 2026
# Goal: Scale and prepare annual population rasters 2000-2025
# Author: Rouslan Karimov
# Organization: UNICEF
# Created: 2 June 2026

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
years        <- 1996:2022   # years with an observed GlobPOP raster
extrap_years <- 2023:2025   # no GlobPOP raster; extrapolated from the latest observed year
all_years    <- c(years, extrap_years)

paths <- here(
  "data", "Globpop",
  paste0("GlobPOP_Count_30arc_", years, "_I32.tiff")
)

outdir <- here("data_derived", "popcounts_interpolated", "scaled_globpop")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)


# Load inputs ====
rasters <- rast(paths)
names(rasters) <- as.character(years)

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

# Population estimates only go to 2023; for 2024 (the 2025 raster's target), we need
# to incorporate projections
data(popprojAge1dt)
popAge1dt <- bind_rows(popAge1dt,
                       popprojAge1dt |> filter(year == 2024) |> mutate(year = as.integer(year))) |>
  select(any_of(names(popAge1dt)))

wpp_totals <- popAge1dt |>
  filter(year %in% (all_years - 1), country_code < 900) |>
  summarise(wpp_tot_pop = sum(pop, na.rm = TRUE),
            .by = c(country_code, year)) |>
  mutate(iso3 = countrycode(country_code, "un", "iso3c", warn = FALSE),
         target_year = year + 1) |>
  filter(!is.na(iso3)) |>
  select(iso3, target_year, wpp_tot_pop)


# Base-raster selector ====
# Observed years use their own GlobPOP raster; extrapolated years (2023-25) reuse
# the latest observed spatial distribution and rely on ScaleToWPP to grow national
# totals to the WPP target year.
BaseRaster <- function(y) {
  src_year <- min(y, max(years))
  rasters[[as.character(src_year)]]
}


# Per-country WPP rescaling ====
# Parallelism here comes from (1) exactextractr's OpenMP-threaded extraction
# over polygons, and (2) terra's multi-threaded arithmetic and GDAL writes
# enabled by terraOptions(threads = TRUE) above and the NUM_THREADS GDAL
# option in writeRaster. Across-year parallelism is not used: each year
# already saturates threads and would otherwise compete for memory and I/O.
ScaleToWPP <- function(base, y) {
  # The scale-of-1 fallback below is meant for territories without WPP data; a year
  # with no WPP totals at all would otherwise pass through unscaled without notice.
  if (!any(wpp_totals$target_year == y)) {
    stop("No WPP totals for target year ", y, " (WPP year ", y - 1, ")")
  }
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
for (y in all_years) {
  base <- BaseRaster(y)
  out_path <- file.path(outdir, sprintf("pop_%d.tif", y))

  if (file.exists(out_path)) {
    # Ask once how to handle pre-existing outputs; reuse that choice thereafter.
    if (!exists("skip_existing")) {
      ans <- utils::menu(
        c("Skip existing files (resume build)",
          "Overwrite existing files"),
        title = "Found existing .tif file(s) in output folder. What do you want to do?"
      )
      skip_existing <- (ans != 2)
    }
    if (skip_existing) {
      message("Skip (exists): ", basename(out_path))
      next
    }
  }
  message("Building ", basename(out_path))
  
  out <- ScaleToWPP(base, y)
  writeRaster(out, out_path, overwrite = TRUE,
              gdal = c("COMPRESS=LZW", "TILED=YES", "NUM_THREADS=ALL_CPUS"))
}


