# Project: SOWC 2026
# Goal: Scratchpad check — how much of Gaza's territory is urban in the most
#       recent DEGURBA year (2024)? Splits Gaza's land area by DUG L1 class from
#       WorldPop's Palestine (PSE) urbanicity file, and maps it.
#       Not part of the pipeline; the only output is the map PNG.
# Author: Rouslan Karimov
# Organization: UNICEF
# Created: 3 October 2026


# Load libraries ====
library(here)
library(terra)
library(sf)
library(rnaturalearth)
library(tidyverse)

terraOptions(progress = 0)


# Parameters ====
# 2024 is the latest year; 2015, 2020 and 2021 bracket the DEGURBA series breaks
# (2015/16 and 2020/21), so changes across them are mostly reclassification.
years <- c(2015, 2020, 2021, 2024)

urb_labels <- c("0" = "Unclassified", "1" = "Rural",
                "2" = "Urban cluster/town", "3" = "Urban centre/city")


# Gaza boundary ====
# Natural Earth has one polygon for Palestine (West Bank + Gaza), the same layer
# used to rescale the population rasters. Gaza is the part west of 34.6E; the
# West Bank begins around 34.88E, so the crop separates the two cleanly.
sf_use_s2(FALSE)   # planar crop on a small lon/lat box

gaza <- ne_countries(scale = "medium", returnclass = "sf") |>
  filter(iso_a3_eh == "PSE") |>
  st_make_valid() |>
  st_crop(xmin = 34, xmax = 34.6, ymin = 31, ymax = 32)

gaza_area_km2 <- as.numeric(st_area(st_transform(gaza, 32636))) / 1e6   # UTM 36N
message("Gaza area (Natural Earth, medium scale): ", round(gaza_area_km2, 1), " km2")

gaza_v <- vect(gaza)


# Land area by urbanicity ====
# Measured on the native DEGURBA grid (World Mollweide is equal-area, so each cell
# is ~1 km2), weighting each cell by the fraction of it inside the Gaza polygon.
# Cells with no DEGURBA class (where the coarse Natural Earth outline runs past
# the edge of the PSE file) count as Unclassified; the shares of classified land
# leave them out.
gaza_area_by_urb <- years |>
  map(\(year) {
    urb <- rast(here("data", "Worldpop", "degree_urban",
                     sprintf("PSE_DUG_%d_GRID_L1_R2025A_v1.tif", year)))
    levels(urb) <- NULL

    # terra:: because tidyverse masks extract()
    cells <- terra::extract(c(urb, cellSize(urb, unit = "km")),
                            project(gaza_v, crs(urb)), exact = TRUE)
    names(cells)[2:3] <- c("urb_code", "cell_km2")

    cells |>
      as_tibble() |>
      mutate(urb_code = replace_na(urb_code, 0)) |>
      summarise(km2 = sum(fraction * cell_km2), .by = urb_code) |>
      mutate(year = year)
  }) |>
  list_rbind() |>
  mutate(urbanicity = factor(unname(urb_labels[as.character(urb_code)]),
                             levels = unname(urb_labels)),
         share_all        = 100 * km2 / sum(km2),
         share_classified = if_else(urb_code == 0, NA,
                                    100 * km2 / sum(km2[urb_code != 0])),
         .by = year)

# Area (km2) and shares (%) by class.
gaza_area_by_urb |>
  select(year, urbanicity, km2, share_all, share_classified) |>
  mutate(across(c(km2, share_all, share_classified), \(x) round(x, 1))) |>
  arrange(year, urbanicity) |>
  print(n = Inf)

# Urban (town + city) share of classified land, by year.
gaza_area_by_urb |>
  summarise(urban_share_classified = sum(share_classified[urb_code %in% 2:3]),
            .by = year) |>
  print()


# Map: Gaza by urbanicity, 2024 ====
# Native 1 km DEGURBA cells from the PSE file, clipped to the Gaza outline. Area
# inside Gaza with no class is shown grey. Ordered classes take one hue, light
# (rural) to dark (city).
urb_colours <- c("Rural" = "#86b6ef", "Urban cluster/town" = "#2a78d6",
                 "Urban centre/city" = "#0d366b", "Unclassified" = "#d9d8d4")

urb_2024 <- rast(here("data", "Worldpop", "degree_urban",
                      "PSE_DUG_2024_GRID_L1_R2025A_v1.tif"))
levels(urb_2024) <- NULL

gaza_moll    <- project(gaza_v, crs(urb_2024))
gaza_moll_sf <- st_transform(gaza, crs(urb_2024))

cells_2024 <- urb_2024 |>
  crop(gaza_moll, snap = "out") |>
  as.polygons(dissolve = FALSE) |>
  st_as_sf() |>
  st_set_crs(st_crs(gaza_moll_sf))
names(cells_2024)[1] <- "urb_code"

classified_2024 <- cells_2024 |>
  filter(urb_code %in% 1:3) |>
  st_intersection(st_geometry(gaza_moll_sf)) |>
  group_by(urb_code) |>
  summarise(.groups = "drop")

map_cells <- bind_rows(
  classified_2024,
  st_sf(urb_code = 0,
        geometry = st_difference(st_geometry(gaza_moll_sf),
                                 st_union(classified_2024)))
) |>
  mutate(urbanicity = factor(unname(urb_labels[as.character(urb_code)]),
                             levels = names(urb_colours)))

# Shares of classified land, 2024, for the subtitle.
shares_2024 <- gaza_area_by_urb |>
  filter(year == 2024)
urban_2024 <- sum(shares_2024$share_classified[shares_2024$urb_code %in% 2:3])
rural_2024 <- shares_2024$share_classified[shares_2024$urb_code == 1]

gaza_map <- ggplot(map_cells) +
  geom_sf(aes(fill = urbanicity), colour = NA) +
  geom_sf(data = gaza_moll_sf, fill = NA, colour = "#3a3a38", linewidth = 0.3) +
  scale_fill_manual(values = urb_colours, name = NULL, drop = FALSE) +
  coord_sf(crs = st_crs(4326), datum = NA) +
  guides(fill = guide_legend(ncol = 1)) +
  labs(
    title = "Gaza by degree of urbanisation, 2024",
    subtitle = sprintf("Share of classified land: %.1f%% urban, %.1f%% rural",
                       urban_2024, rural_2024),
    caption = paste("Source: WorldPop DEGURBA L1 (R2025A), Palestine file,",
                    "1 km cells;\nGaza outline from Natural Earth (medium scale).")
  ) +
  theme_void(base_size = 11) +
  theme(plot.background  = element_rect(fill = "#fcfcfb", colour = NA),
        plot.title       = element_text(face = "bold", colour = "#1f1f1e"),
        plot.subtitle    = element_text(colour = "#55554f", margin = margin(b = 8)),
        plot.caption     = element_text(colour = "#77776f", size = 8, hjust = 0),
        legend.position  = "right",
        legend.text      = element_text(colour = "#3a3a38"),
        plot.margin      = margin(12, 12, 12, 12))

ggsave(here("code", "Pillar 1", "scratchpad_gaza_urbanicity_map.png"),
       gaza_map, width = 6, height = 6, dpi = 200)
