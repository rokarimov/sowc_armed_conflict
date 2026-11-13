library(here)
library(tidyverse)
library(jsonlite)
library(readxl)
library(purrr)
library(maps)
library(countrycode)
library(patchwork)
library(wesanderson)


# Conflict-years with 25+ deaths (from Uppsala GED) ====

ged <- readRDS(here("data", "Uppsala GEDEvent_v25_1.rds"))
ged <- ged |>
  mutate(iso3 = countrycode(country, origin = "country.name.en", destination = "iso3c",
                            custom_match = c("Yemen (North Yemen)" = "YEM"))) |>
  filter(year > 1995)


# Load the list of all available DHS surveys from 2000 on
dhs <- read_csv(here("data", "DHS_available.csv"), col_types = cols(gps = col_logical())) |>
  filter(yr1 > 1995)

# Check which DHS surveys are available from IPUMS ====
ipums_key <- Sys.getenv("IPUMS_API_KEY")
base_url <- "https://api.ipums.org/metadata/samples?collection=dhs&version=2"

# Fetches a single page from the IPUMS DHS samples endpoint
FetchIPUMS <- function(pagenum) {
  url <- paste0(base_url, "&pageNumber=", pagenum, "&pageSize=500")
  readLines(url(url, headers = c(Authorization = ipums_key)), warn = FALSE) |>
    paste(collapse = "") |>
    fromJSON()
}

# Fetch data
first   <- FetchIPUMS(pagenum = 1)
n_pages <- ceiling(first$totalCount / 500)

ipums_raw <- bind_rows(
  first$data,
  lapply(seq_len(n_pages - 1) + 1, function(p) FetchIPUMS(p)$data)
)

# Create list of IPUMS-available surveys
ipums_df <- ipums_raw |>
  mutate(
    ipums_year = as.integer(str_extract(description, "\\d{4}")),
    ipums_country = str_extract(description, "^.*?(?=[_\\s]\\d{4})") # Some countries have underscores attached; this handles it
  ) |>
  filter(!is.na(ipums_year)) |>  # Filter out entries that have no year listed
  mutate(iso3 = map_chr(ipums_country, ~countrycode(.x, origin = "country.name.en", destination = "iso3c")))

ipums_df <- distinct(ipums_df, iso3, ipums_year)

# Add in_ipums column to dhs: TRUE if the survey iso3+year exists in IPUMS
dhs <- dhs |>
  mutate(
    in_ipums = map2_lgl(iso3, yr1,
                        ~ any(ipums_df$iso3 == .x & ipums_df$ipums_year == .y)),
    had_conflict = map_lgl(iso3, ~ .x %in% unique(ged$iso3))
  )



# Export list of DHS to request from IPUMS
dhs |>
  filter(gps == TRUE, in_ipums == TRUE, had_conflict == TRUE) |>
  write_csv("get_from_ipums_dhs.csv")


# Compute conflict windows per (conflict, country): start = first year flagged active (25+ deaths), end = last event year
spans <- ged |>
  filter(active_year == 1) |>
  summarise(startyr = min(year), .by = c(conflict_new_id, iso3)) |>
  left_join(
    ged |> summarise(endyr = max(year), .by = c(conflict_new_id, iso3)),
    by = c("conflict_new_id", "iso3")
  )

dhs_matches <- spans |>
  left_join(dhs, by = "iso3",
            suffix = c("_conflict", "_survey"),
            relationship = "many-to-many") |>
  filter(yr1 <= endyr + 1 & yr2 >= startyr) |>
  filter(yr1 > 2009) |>
  distinct(iso3, country, yr1, yr2, gps, in_ipums) |>
  mutate(survey_type = "DHS")


# Load MICS surveys and check conflict overlap ====
mics <- read_csv(here("data", "MICS_all.csv"),
                 col_types = cols(dataset_available = col_logical(),
                                  gps_available     = col_logical(),
                                  in_ipums          = col_logical(),
                                  subnational       = col_logical())) |>
  filter(dataset_available, yr1 > 1995) |>
  mutate(in_ipums = coalesce(in_ipums, FALSE),
         subnational = coalesce(subnational, FALSE),
         had_conflict = map_lgl(iso3, ~ .x %in% unique(ged$iso3)))


# Export list of MICS to request from IPUMS
mics |>
  filter(gps_available == TRUE, in_ipums == TRUE, had_conflict == TRUE) |>
  write_csv("get_from_ipums_mics.csv")

mics_matches <- spans |>
  left_join(mics, by = "iso3",
            suffix = c("_conflict", "_survey"),
            relationship = "many-to-many") |>
  filter(yr1 <= endyr + 1 & yr2 >= startyr) |>
  distinct(iso3, country, yr1, yr2, gps = gps_available, in_ipums, subnational) |>
  mutate(survey_type = "MICS")


# Combine DHS and MICS matches
matches <- bind_rows(dhs_matches, mics_matches) |>
  mutate(subnational = coalesce(subnational, FALSE)) |>
  arrange(iso3, desc(yr1))


# Maps of conflict-country survey coverage ====
world <- map_data("world") |>
  filter(region != "Antarctica") |>
  mutate(iso3 = countrycode(region, "country.name", "iso3c"))

# One row per conflict country, flagging which coverage conditions are met
coverage <- matches |>
  summarise(
    any_survey    = TRUE,
    any_gps       = any(gps),
    any_ipums     = any(in_ipums),
    any_gps_ipums = any(gps & in_ipums),
    .by = iso3
  )

# Helper to label a logical coverage column as a 3-level factor
LabelCoverage <- function(x) {
  case_when(
    x == TRUE  ~ "Yes",
    x == FALSE ~ "No",
    .default   = "No surveys"
  ) |>
    factor(levels = c("Yes", "No", "No surveys"))
}

map_df <- world |>
  left_join(coverage, by = "iso3") |>
  mutate(across(any_survey:any_gps_ipums, LabelCoverage))

fill_colours <- c(
  "Yes" = "limegreen",
  "No" = "red",
  "No surveys" = "lightgrey"
)

MakeMap <- function(fill_var, title) {
  ggplot(map_df, aes(x = long, y = lat, group = group)) +
    geom_polygon(aes(fill = .data[[fill_var]]), colour = "black", linewidth = 0.1) +
    scale_fill_manual(values = fill_colours, name = NULL, drop = FALSE) +
    labs(title = title) +
    coord_quickmap() +
    theme_void() +
    theme(
      legend.position  = "bottom",
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5)
    )
}

p1 <- MakeMap("any_survey", "Countries with Surveys") + theme(legend.position = "none")
p2 <- MakeMap("any_gps", "Surveys with GPS data")
p3 <- MakeMap("any_ipums", "Surveys in IPUMS")
p4 <- MakeMap("any_gps_ipums", "Surveys in IPUMS with GPS data")

p1 + p2
p3 + p4
