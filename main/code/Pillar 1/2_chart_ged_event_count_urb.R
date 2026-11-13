# Project: SOWC 2026
# Goal: Map conflict events count over time
# Author: Rouslan Karimov
# Organization: UNICEF
# Created: 1 June 2026

# Load libraries
library(here)
library(terra)
library(sf)
library(tidyverse)
library(scales)
library(ggrepel)
library(viridis)
library(showtext)
font_add_google("Noto Sans", "Noto Sans")
showtext_auto()


# Define UNICEF color palette
unicefcol <- c('#00AEEF', '#9ADBE8', '#0047BB', '#004C45', '#FFB500', '#FF8200', '#961A49', '#E2231A')

# Load data
# UNICEF-approved geographic names
area <- readxl::read_xlsx(here("data", "areas.xlsx"))

ged <- readRDS(here("data_derived", "GED_cleaned.RDS")) |>
  filter(year %in% 1996:2025) |>   # baseline year is 1996; GED_cleaned now retains pre-1996 events
  left_join(area |> select(iso3, sdgregcode), by =  "iso3") |> 
  st_drop_geometry()

# Per-year urbanicity (DUG L1) rasters, named by year (2015-2025)
urb_dir   <- here("data_derived", "degurba")
urb_files <- list.files(urb_dir, pattern = "\\.tif$", full.names = TRUE)
names(urb_files) <- str_match(basename(urb_files), "_(\\d{4})\\.tif$")[, 2]


# Classify events ====

ged$urb_class <- NA_character_  # Initialize

for (y in (2015:2025)) {

  # First, get raster of urban/rural zones on the planet for a given year
  # These data are from WorldPop (DUG L1)
  r <- rast(urb_files[[as.character(y)]])
  
  # Convert all violent events from given year to a SpatVector
  sv <- vect(ged |> filter(year == y), geom = c("longitude", "latitude"), crs = "EPSG:4326")
  sv <- project(sv, crs(r))  # Project to the same spatial reference system (Mollweide) as our raster
  
  # Now extract urbanicity classifications from raster for each vector point.
  # Some points, especially in coastal areas, end up on water or NoData cells.
  # To prevent them from being unassigned an urban class, we assign them to the nearest
  # classified cell within a 2-cell radius (which is 2000 meters; each cell is 1000 x 1000 m).
  vals <- terra::extract(r, sv, ID = FALSE, search_radius = 2000)

  ged$urb_class[ged$year == y] <- as.character(vals$urbanicity)
  
}

ged$urb_class <- factor(ged$urb_class, levels = c("Rural", "Urban cluster/town", "Urban centre/city"))

# Visual check that our projection was accurate
plot(r, col = unicefcol[c(1, 4, 2, 5, 6, 8, 7, 3)]); points(sv, col = "yellow")


# Visualize ====
df <- ged |>
  filter(year > 2014, !is.na(urb_class)) |>  # a small # of points still lack urban class, so we filter them out
  summarise(numevents = n(), avg_deaths = mean(best), .by = c(year, urb_class))

ggplot(df, aes(x = year, y = numevents, fill = avg_deaths)) +
  geom_col() +
  geom_text(aes(label = label_comma()(numevents)), vjust = -0.5) +
  labs(y = "Number of conflict events") +
  scale_x_continuous(
    breaks = seq(1996, 2025, 1),
    labels = seq(1996, 2025, 1)) +
  scale_y_continuous(limits = c(0, 17500),
                     labels = label_comma()) +
  scale_fill_gradientn(
    name = "Average deaths\nper conflict event",
    colours = unicefcol[c(1, 4, 5, 6, 8)],
    breaks  = scales::breaks_pretty(4),   # or e.g. c(1, 5, 10, 20)
    guide   = guide_colourbar(title.position = "top")
  ) +
  facet_wrap(vars(urb_class), ncol = 1) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        text = element_text(family = "Noto Sans",
                            size = 14))






