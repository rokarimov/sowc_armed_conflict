# Project: SOWC 2026
# Goal: Analysis of conflict zones and their area
# Author: Rouslan Karimov
# Organization: UNICEF
# Created: 1 June 2026

# Load libraries
library(here)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggrepel)
library(showtext)  # For NotoSans
font_add_google("Noto Sans")
showtext_auto()

# Define UNICEF color palette
unicefcol <- c('#00AEEF', '#9ADBE8', '#0047BB', '#004C45', '#FFB500', '#FF8200', '#961A49', '#E2231A')


# Load data
load(here("data_derived", "num_children_in_confl_globpop.RData"))

areas <- readxl::read_xlsx(here("data", "areas.xlsx")) |>
  select(iso3, region = sdgregcode)

confl_num_countries <- confl_num_countries |>
  left_join(areas, by = "iso3")


# Analyze areas of conflict over time ====
confl_num_countries <- st_make_valid(confl_num_countries)
confl_num_countries$area <- st_area(confl_num_countries$geometry)


# Plot total conflict areas - global
df <- confl_num_countries |>
  st_drop_geometry() |>
  summarise(conflict_area = as.numeric(sum(area)) / 1000000, .by = year)


ggplot(df, aes(x = year, y = conflict_area)) +
  geom_line(colour = "blue", linewidth = 1) +
  geom_text_repel(
    aes(x = year, 
        y = case_when(year %in% c(1996, 2000, 2025) ~ conflict_area,
                      .default = NA),
        label = round(conflict_area / 1e+6, 1),
        vjust = -1,
        family = "Noto Sans"
    )) +
  labs(y = "Areas within 50 km of conflict,\n world total (million km²)") +
  scale_y_continuous(
    limits = c(0, 9400000),
    breaks = seq(0, 9100000, by = 1e+6),
    labels = seq(0, 9.1, by = 1)
  ) +
  scale_x_continuous(
    breaks = seq(1996, 2026, 2),
    labels = seq(1996, 2026, 2)) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        text = element_text(family = "Noto Sans"))


# Plot total conflict areas - by region
df_reg <- confl_num_countries |>
  st_drop_geometry() |>
  summarise(conflict_area = as.numeric(sum(area)) / 1000000,
            .by = c(region, year))
  

df_reg |>
  mutate(label = ifelse(year == max(year), region, NA_character_)) |>
  ggplot(aes(x = year, y = conflict_area, colour = (region))) +
  geom_line(linewidth = 1) +
  geom_text_repel(
    aes(x = year,
        y = case_when(year == 1996 ~ conflict_area,
                      .default = NA),
        label = round(conflict_area / 1e+6, 1),
        hjust = 0, nudge_x = -0.001,
        family = "Noto Sans"
      )) +
  geom_text_repel(
    aes(x = year,
        y = case_when(year == 2025 ~ conflict_area,
                      .default = NA),
        label = round(conflict_area / 1e+6, 1),
        hjust = 1,
        family = "Noto Sans"
    )) +
  geom_text_repel(
    aes(label = label),
    min.segment.length = Inf,
    hjust = 0, nudge_x = 1, family = "Noto Sans", segment.size = 0) +
  scale_colour_manual("Region", values = unicefcol[c(1, 4, 2, 5, 6, 8, 7, 3)], guide = "none") +
  scale_x_continuous(
    limits = c(1996, 2032),
    breaks = seq(1996, 2032, 2),
    labels = seq(1996, 2032, 2)) +
  scale_y_continuous(
    limits = c(0, 3800000),
    breaks = seq(0, 3800000, by = 1e+6),
    labels = seq(0, 3.8, by = 1)
  ) +
  labs(y = "Areas within 50 km of conflict,\n world total (million km²)") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        text = element_text(family = "Noto Sans"))


# Plot total conflict area by urbanicity
df_urb <- confl_num_urb |>
  summarise(conflict_area = as.numeric(sum(area)) / 1000000, .by = c(year, urbanicity))

df_urb |>
  ggplot(aes(x = year, y = conflict_area)) +
  geom_line(aes(colour = urbanicity), linewidth = 1) +
  geom_text_repel(
    aes(x = year, 
        y = case_when(year == 2025 ~ conflict_area,
                      .default = NA),
        label = round(conflict_area / 1e+6, 1),
        hjust = 1,
        family = "Noto Sans"
    )) +
  geom_text_repel(
    aes(x = year, 
        y = case_when(year == 2015 ~ conflict_area,
                      .default = NA),
        label = round(conflict_area / 1e+6, 1),
        hjust = 0,
        family = "Noto Sans"
    )) +
  scale_colour_manual("Type of location", values = unicefcol[c(1, 5, 7)]) +
  scale_y_continuous(
    limits = c(0, 9100000),
    breaks = seq(0, 9100000, by = 1e+6),
    labels = seq(0, 9.1, by = 1)
  ) +
  scale_x_continuous(
    breaks = seq(2015, 2025, 1),
    labels = seq(2015, 2025, 1)) +
  labs(y = "Conflict zone area (million km²)") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        text = element_text(family = "Noto Sans", size = 16))


# Mapping conflict over time ====
# Get world map
world <- ne_countries(scale = "medium", returnclass = "sf") |> filter(name != "Antarctica")

ggplot() +
  geom_sf(data = world, fill = "beige") +
  geom_sf(data = confl_num_countries |> filter(year == 2025), fill = "brown") +
  theme_void()

# Calculate total land area covered by conflict in a given year
confl_num_countries |> filter(year == 2025) |> pull(area) |> sum()
