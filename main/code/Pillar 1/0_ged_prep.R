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
ged <- read_csv(here("data", "GEDEvent_v26_1.csv")) |>
  filter(best > 0)

ged$iso3 <- countrycode(ged$country_id, "gwn", "iso3c", warn = FALSE)
ged$iso3[ged$country == "Yemen (North Yemen)"] <- "YEM"
ged$iso3[ged$country == "Serbia (Yugoslavia)"] <- "SRB"
ged$month <- month(ged$date_start)

# Events as recorded by GED are originals; every duplicate made below for a
# successor or co-belligerent state is flagged is_copy = TRUE, so that charts can
# count each event once.
ged$is_copy <- FALSE

# UNICEF-approved geographic names
area <- readxl::read_xlsx(here("data", "areas.xlsx"))

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
  mutate(iso3 = "SSD", is_copy = TRUE)

# Indonesia -> Timor-Leste: events through May 2002 also count against TLS
ged_tls <- ged |>
  filter(country == "Indonesia", year < 2003) |>
  filter(!(year == 2002 & month > 5)) |>
  mutate(iso3 = "TLS", is_copy = TRUE)

# Yugoslavia (gw = 345) -> successor states, each only through the month it
# became independent (same convention as South Sudan above). Slovenia, Croatia,
# North Macedonia and Bosnia were independent before our 1996 baseline, so their
# copies only matter for pre-1996 years.
yugo_cutoffs <- tribble(
  ~code,  ~end_year, ~end_month,
  "SVN",  1991,      6,    # 25 June 1991
  "HRV",  1991,      6,    # 25 June 1991
  "MKD",  1991,      9,    # 8 September 1991
  "BIH",  1992,      3,    # 3 March 1992
  "MNE",  2006,      6,    # 3 June 2006
  "KOS",  2008,      2     # 17 February 2008
)
ged_yugo <- ged |> filter(country_id == 345)
ged_balkans <- map(seq_len(nrow(yugo_cutoffs)), \(i) {
  ged_yugo |>
    filter(year < yugo_cutoffs$end_year[i] |
             (year == yugo_cutoffs$end_year[i] & month <= yugo_cutoffs$end_month[i])) |>
    mutate(iso3 = yugo_cutoffs$code[i], is_copy = TRUE)
}) |> list_rbind()

# Ethiopia (gw = 530) -> Eritrea, through Dec 2000 (end of the 1998-2000 war)
ged_eritrea <- ged |>
  filter(country_id == 530, year < 2001) |>
  mutate(iso3 = "ERI", is_copy = TRUE)

# South Africa (gw = 560) -> Namibia: pre-independence events through March 1990
# (Namibia became independent 21 March 1990) also count against NAM
ged_namibia <- ged |>
  filter(country_id == 560, year < 1991) |>
  filter(!(year == 1990 & month > 3)) |>
  mutate(iso3 = "NAM", is_copy = TRUE)

# Soviet Union (gw = 365) -> Azerbaijan/Armenia: late-Soviet Nagorno-Karabakh
# events (1989-1991) count against both successor states
ged_soviet     <- ged |> filter(country_id == 365, year %in% 1989:1991)
ged_soviet_aze <- ged_soviet |> mutate(iso3 = "AZE", is_copy = TRUE)
ged_soviet_arm <- ged_soviet |> mutate(iso3 = "ARM", is_copy = TRUE)

# Azerbaijan/Armenia war: each side's events also count against the other's
# population. Recode the copies' iso3 to the other side (PRIO's code left it
# unchanged, which duplicated every event within its own country).
ged_azerbaijan <- ged |> filter(iso3 == "ARM") |> mutate(iso3 = "AZE", is_copy = TRUE)
ged_armenia    <- ged |> filter(iso3 == "AZE") |> mutate(iso3 = "ARM", is_copy = TRUE)

# Israel/Palestine (gw = 666): assign each event to the territory it occurred in,
# using GED adm_1. No spillover across the two. East Jerusalem (GED "Jerusalem
# district") and events with missing adm_1 stay with Israel.
pse_event <- ged$country_id == 666 & ged$adm_1 %in% c("Gaza Strip", "West Bank")
ged$iso3[pse_event] <- "PSE"

# Morocco (gw = 600) -> Western Sahara: Moroccan events also count against ESH;
# clipping to ESH's borders keeps only those within 50 km of the territory
ged_wsh <- ged |> filter(country_id == 600) |> mutate(iso3 = "ESH", is_copy = TRUE)

ref <- ne_countries(scale = "medium", returnclass = "sf") |> filter(iso_n3 == "643")
pts <- st_as_sf(ged, coords = c("longitude", "latitude"), crs = 4326)
hit <- ged$country_id == 369 & lengths(st_intersects(pts, ref)) > 0
ged$iso3[hit] <- countrycode(643, "iso3n", "iso3c")

# Originals first, so distinct(id) downstream keeps the original row
ged <- bind_rows(
  ged, ged_ssd, ged_tls, ged_balkans, ged_eritrea, ged_namibia,
  ged_soviet_aze, ged_soviet_arm,
  ged_azerbaijan, ged_armenia, ged_wsh
) |>
  filter(!is.na(iso3)) |>
  # Replace GED's country names with UNICEF-approved names, keyed on the assigned
  # iso3 (so a copy carries its successor's name)
  select(-country) |>
  left_join(select(area, iso3, country), by = "iso3")

stopifnot(!anyNA(ged$country))           # every assigned iso3 must be in areas.xlsx
stopifnot(!anyDuplicated(ged[c("id", "iso3")]))   # no event twice in one country

# Save to disk
saveRDS(ged, file = here("data_derived", "GED_cleaned.RDS"))


# Pre-build clipped conflict buffers ====
# Each event's 50 km buffer clipped to present-day national borders.

# Country boundaries, from Natural Earth
countries <- ne_countries(scale = "medium", returnclass = "sf") |>
  # NE codes these -99; fold them into the UN-recognized country (Kosovo kept
  # separate).
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


# Clip a country's per-event 50 km buffers to its borders. A buffer that does not
# reach the country at all (e.g. a copy far from its successor state, or an event
# more than 50 km offshore) is dropped: violence puts at risk only the population
# of the country the event is assigned to.
ClipBuffersToCountry <- function(buffers, iso3_code, countries) {
  cntry <- st_geometry(countries)[countries$iso3 == iso3_code]
  if (length(cntry) == 0) stop("No border polygon for iso3 ", iso3_code)
  clipped <- suppressWarnings(
    st_intersection(st_make_valid(buffers), st_union(cntry))
  ) |> st_make_valid()
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
  select(id, country, iso3, year, best, is_copy)

# Clip each event's buffer to its country's modern borders (copies clip to their
# assigned successor's polygon). Buffers stay separate, not unioned, so a pixel
# can later accumulate every nearby event's deaths. Clip per country, binding a
# few large sf pieces rather than thousands of small ones (much faster).
# Parallelized: multicore (fork) when headless, multisession in the RStudio GUI.
if (interactive()) plan(multisession, workers = 4) else plan(multicore, workers = 4)

conflict_buffers_clipped <- buffers |>
  group_split(iso3) |>
  future_map(\(cnt) ClipBuffersToCountry(cnt, cnt$iso3[1], countries_eck),
             .options = furrr_options(packages = "sf", seed = TRUE)) |>
  do.call(rbind, args = _) |>     # bind the ~200 per-country results
  st_transform(4326)

plan(sequential)   # release the workers

conflict_buffers_clipped <- conflict_buffers_clipped |> 
  rename(numdeaths = best)

saveRDS(conflict_buffers_clipped, here("data_derived", "conflict_buffers_clipped.RDS"))

