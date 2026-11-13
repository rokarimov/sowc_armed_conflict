# Project: SOWC 2026
# Goal: Map conflict events count over time
# Author: Rouslan Karimov
# Organization: UNICEF
# Created: 1 June 2026

# Load libraries
library(here)
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
  filter(year %in% 1996:2025) |>
  select(-country) |> 
  left_join(area |> select(iso3, country, sdgregcode), by =  "iso3")

df <- ged |> summarise(numevents = n(), avg_deaths = mean(best), .by = c(year))

ggplot(df, aes(x = year, y = numevents, fill = avg_deaths)) +
  geom_col() +
  geom_text(aes(label = label_comma()(numevents)), vjust = -0.5) +
  labs(y = "Number of conflict events") +
  scale_x_continuous(
    limits = c(1995.5, 2025.5),
    breaks = seq(1996, 2025, 2),
    labels = seq(1996, 2025, 2)) +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_gradientn(
    name = "Average deaths\nper conflict event",
    colours = unicefcol[c(1, 4, 5, 6, 8)],
    breaks  = scales::breaks_pretty(4),   # or e.g. c(1, 5, 10, 20)
    guide   = guide_colourbar(title.position = "top")
    ) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        text = element_text(family = "Noto Sans",
                            size = 14))

df_reg <- ged |> summarise(numevents = n(), avg_deaths = mean(best), .by = c(year, sdgregcode))

# Listing top ten countries by total # deaths ====

ged |> filter(year == 2025) |> 
  summarise(totaldeaths = sum(best, na.rm = TRUE), .by = c("country")) |> 
  arrange(desc(totaldeaths))

# Fin