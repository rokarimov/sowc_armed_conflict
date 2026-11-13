# Project: SOWC 2026
# Goal: Clean Uppsala GED data
# Author: Rouslan Karimov
# Organization: UNICEF
# Created: 4 June 2026


# Load libraries
library(here)
library(tidyverse)
library(sf)
library(countrycode)
library(rnaturalearth)
library(furrr)


# Load, clean and transform GED ====
# ged <- readRDS(here("data", "Uppsala GEDEvent_v25_1.rds")) |>
#   filter(best > 0)
ged <- read_csv(here("data", "GEDEvent_v26_1.csv")) |>
  filter(best > 0)

ged$iso3 <- countrycode(ged$country_id, "gwn", "iso3c", warn = FALSE)
ged$iso3[ged$country == "Yemen (North Yemen)"] <- "YEM"
ged$iso3[ged$country == "Serbia (Yugoslavia)"] <- "SRB"
ged$month <- month(ged$date_start)

# Drop countries / events that PRIO classifies as non-conflict (one-off terror
# incidents or otherwise not constituting conflict that puts the population at
# risk). Dropping Australia, Belgium, Canada, France, Netherlands, United States, Austria,
# Germany, United Arab Emirates, Sweden, Benin, Bhutan, Botswana and Switzerland
nc_codes <- c(900, 211, 20, 220, 210, 2, 305, 260, 696, 380, 434, 760, 571, 225)
ged <- ged |>
  filter(!(country_id %in% nc_codes)) |>
  filter(!(country_id == 230 & year > 2016)) |>   # Spain: keep Basque rebels
  filter(!(country_id == 200 & year > 1999)) |>   # UK: keep the Troubles
  filter(!(country_id == 616 & year == 2002))     # Tunisia: drop 2002 one-off


# Some countries split off within the GED timeframe and need pre-split events to be duplicated so they can be assigned
# as affecting both countries prior to the split. Other interstate conflicts need to have events duplicated so that
# events will affect both countries' populations regardless of which country the event took place in. This is because
# normally events are not allowed to 'spill out' of a country's borders unless there is a justification for allowing this

# Sudan -> South Sudan: events through July 2011 also count against SSD
ged_ssd <- ged |>
  filter(country == "Sudan", year < 2012) |>
  filter(!(year == 2011 & month > 7)) |>
  mutate(iso3 = "SSD")

# Indonesia -> Timor-Leste: events through May 2002 also count against TLS
ged_tls <- ged |>
  filter(country == "Indonesia", year < 2003) |>
  filter(!(year == 2002 & month > 5)) |>
  mutate(iso3 = "TLS")

# Balkans pre-breakup of Yugoslavia (gw = 345) -> successor states
ged_yugo <- ged |> filter(country_id == 345)
ged_balkans <- map(
  c("HRV", "SVN", "SRB", "MNE", "MKD", "BIH", "KOS"),
  \(code) mutate(ged_yugo, iso3 = code)
) |> list_rbind()

# Eritrea pre-split from Ethiopia (gw = 530), through Dec 2000
ged_eritrea <- ged |>
  filter(country_id == 530, year < 2001) |>
  mutate(iso3 = "ERI")

# South Africa (gw = 560) -> Namibia: pre-independence events through March 1990
# (Namibia became independent 21 March 1990) also count against NAM
ged_namibia <- ged |>
  filter(country_id == 560, year < 1991) |>
  filter(!(year == 1990 & month > 3)) |>
  mutate(iso3 = "NAM")

# Soviet Union (gw = 365) -> Azerbaijan/Armenia: late-Soviet Nagorno-Karabakh
# events (1989-1991) count against both successor states
ged_soviet     <- ged |> filter(country_id == 365, year %in% 1989:1991)
ged_soviet_aze <- ged_soviet |> mutate(iso3 = "AZE")
ged_soviet_arm <- ged_soviet |> mutate(iso3 = "ARM")

# Azerbaijan/Armenia war: each side's events also count against the other's
# population
ged_azerbaijan <- filter(ged, iso3 == "ARM")
ged_armenia    <- filter(ged, iso3 == "AZE")

# Israel/Palestine (gw = 666): assign each event to the territory it occurred in,
# using GED adm_1. No spillover across the two. East Jerusalem (GED "Jerusalem
# district") and events with missing adm_1 stay with Israel.
pse_event <- ged$country_id == 666 & ged$adm_1 %in% c("Gaza Strip", "West Bank")
ged$iso3[pse_event]    <- "PSE"
ged$country[pse_event] <- "Palestine"

# Western Sahara (gw = 600)
ged_wsh <- ged |> filter(country_id == 600) |> mutate(iso3 = "ESH")

ged <- bind_rows(
  ged, ged_ssd, ged_tls, ged_balkans, ged_eritrea, ged_namibia,
  ged_soviet_aze, ged_soviet_arm,
  ged_azerbaijan, ged_armenia, ged_wsh
) |>
  filter(!is.na(iso3))

# Save to disk
saveRDS(ged, file = here("data_derived", "GED_cleaned.RDS"))


# Pre-build clipped conflict buffers ====
# Each event's 50 km buffer clipped to present-day national borders.

# Country boundaries, from Natural Earth
countries <- ne_countries(scale = "medium", returnclass = "sf") |>
  filter(iso_a3_eh != "-99") |>
  rename(iso3 = iso_a3_eh) |>
  group_by(iso3) |>
  summarise(.groups = "drop") |>
  st_make_valid()

# Project to equal-area (Eckert VI)
countries_eck <- countries |>
  st_transform("+proj=eck6") |>
  st_make_valid()


# Clip a country's per-event 50 km buffers to its borders
ClipBuffersToCountry <- function(buffers, iso3_code, countries) {
  if (is.na(iso3_code)) return(buffers)
  cntry <- st_geometry(countries)[countries$iso3 == iso3_code]
  if (length(cntry) == 0) return(buffers)
  clipped <- suppressWarnings(
    st_intersection(st_make_valid(buffers), st_union(cntry))
  ) |> st_make_valid()
  if (nrow(clipped) == 0 || all(st_is_empty(clipped))) return(buffers)   # fallback
  if (any(st_geometry_type(clipped) == "GEOMETRYCOLLECTION")) {
    clipped <- st_collection_extract(clipped, "POLYGON")
  }
  clipped[!st_is_empty(clipped), ]
}


# Per-event 50 km buffers, built in equal-area Eckert VI; keep best (battle-deaths).
buffers <- ged |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform("+proj=eck6") |>
  st_buffer(dist = 50000) |>
  select(country, iso3, year, best)

# Clip each event's buffer to its country's modern borders (split-event duplicates
# will clip to their successor's polygon). Buffers stay separate, not unioned, so a
# pixel can later accumulate every nearby event's deaths. Clip per country and loop
# years inside: this keeps ClipBuffersToCountry's per-country-year fallback while
# binding a few large sf pieces rather than thousands of small ones (much faster).
# Parallelised: multicore (fork) when headless, multisession in the RStudio GUI.
if (interactive()) plan(multisession, workers = 4) else plan(multicore, workers = 4)

conflict_buffers_clipped <- buffers |>
  group_split(country, iso3) |>
  future_map(\(cnt) {
    # clip this country's buffers one year at a time (per-year fallback), then
    # bind the years into a single sf for this country
    buffers_by_cntry_yr <- group_split(cnt, year) |>
      map(\(df) ClipBuffersToCountry(df, df$iso3[1], countries_eck))
    do.call(rbind, buffers_by_cntry_yr)
  }, .options = furrr_options(packages = "sf", seed = TRUE)) |>
  do.call(rbind, args = _) |>     # bind the ~200 per-country results
  st_transform(4326)

plan(sequential)   # release the workers

conflict_buffers_clipped <- conflict_buffers_clipped |> 
  rename(numdeaths = best)

saveRDS(conflict_buffers_clipped, here("data_derived", "conflict_buffers_clipped.RDS"))

