# Project: SOWC 2026
# Goal: Merge per-country WorldPop Degree of Urbanisation (DUG L1) tiles into one
#       global raster per available year, for use as a per-pixel urban/rural zone
#       when disaggregating children-in-conflict counts by urbanicity.
# Author: Rouslan Karimov
# Organization: UNICEF
# Created: 15 June 2026


# Load libraries ====
library(here)
library(terra)
library(tidyverse)

terraOptions(threads = TRUE)


# Inputs ====
# Per-country DUG L1 tiles, native World Mollweide, 1 km, classes 1/2/3 (NoData 0).
urb_dir   <- here("data", "Worldpop", "degree_urban")
urb_files <- list.files(urb_dir, pattern = "\\.tif$", full.names = TRUE)

# DUG L1 class labels, attached so downstream zonal output is a readable factor.
dug_levels <- data.frame(
  value = 1:3,
  urbanicity = c("Rural", "Urban cluster/town", "Urban centre/city")
)

# Year is the token after "DUG_" in each filename.
file_years <- str_match(basename(urb_files), "_DUG_(\\d{4})_")[, 2]
years      <- sort(unique(as.integer(file_years)))


# Output ====
out_dir <- here("data_derived", "degurba")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)


# Prompt: skip or overwrite existing per-year mosaics ====
existing <- list.files(out_dir, pattern = "\\.tif$")

skip_existing <- TRUE

if (length(existing) > 0) {
  ans <- utils::menu(
    c("Skip existing files (resume build)",
      "Overwrite existing files"),
    title = sprintf("Found %d existing .tif file(s) in %s. What do you want to do?",
                    length(existing), out_dir)
  )
  skip_existing <- (ans != 2)
}


# Build one merged raster per year ====
# Native Mollweide is kept; reprojection to the population grid happens per-crop
# in the analysis scripts.
for (y in years) {
  out_path <- file.path(out_dir, sprintf("worldpop_degurba_l1_%d.tif", y))
  if (file.exists(out_path) && skip_existing) {
    message("Skip (exists): ", basename(out_path))
    next
  }

  message("Building ", basename(out_path))
  yr_files <- urb_files[file_years == as.character(y)]

  urb <- map(yr_files, rast) |>
    sprc() |>     # Assemble tiles into a collection to pass on to merge()
    merge()

  levels(urb) <- dug_levels

  writeRaster(urb, out_path, overwrite = TRUE,
              gdal = c("COMPRESS=LZW", "TILED=YES", "NUM_THREADS=ALL_CPUS"))
}
