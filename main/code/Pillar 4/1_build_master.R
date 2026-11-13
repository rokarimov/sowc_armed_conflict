# Project: SOWC 2026, Pillar 4
# Goal: Assemble the master dataset for the Pillar 4 analyses
# Author: Rouslan Karimov
# Organization: UNICEF
# Created: 2 July 2026

# Load libraries
library(here)
library(tidyverse)
library(readxl)
library(countrycode)
library(wpp2024)
library(vdemdata)


# 1. Outcomes ====

# Mortality data
igme <- read_csv(
  here("data", "Pillar 4", "outcomes", "UN IGME 2025.csv"),
  col_select = c("REF_AREA", "Indicator", "Sex", "Wealth Quintile",
                 "Series Name", "REF_DATE", "OBS_VALUE"),
  show_col_types = FALSE
) |>
  filter(`Series Name` == "UN IGME estimate",
         Sex == "Total",
         `Wealth Quintile` == "Total",
         Indicator %in% c("Under-five mortality rate",
                          "Infant mortality rate",
                          "Neonatal mortality rate")) |>
  mutate(
    iso3      = REF_AREA,
    year      = as.integer(floor(REF_DATE)),
    indicator = recode(Indicator,
                       "Under-five mortality rate" = "u5mr",
                       "Infant mortality rate"     = "imr",
                       "Neonatal mortality rate"   = "nnmr"),
    value     = OBS_VALUE,
    .keep = "none"
  ) |>
  filter(!is.na(countrycode(iso3, "iso3c", "iso.name.en", warn = FALSE))) |>   # Drop regional / global aggregates
  pivot_wider(names_from = indicator, values_from = value)


# Stunting and wasting data

stunting <- read_xlsx(
  here("data", "Pillar 4", "outcomes", "JME Stunting Surveys.xlsx"),
  sheet = "Primary Data"
) |>
  filter(Stratifier == "Total") |>
  mutate(iso3 = `ISO code`, year = as.integer(`Year*`),
            stunt = as.numeric(Stunting)) |>
  select(-`ISO code`, -`Year*`, -Stunting) |>
  filter(!is.na(stunt)) |>
  mutate(dupes = duplicated(paste(iso3, year)))

# Manually filter out competing country-year values
stunting <- stunting |>
  filter(!(iso3 == "BDI" & year == 2023 & `Short Source` == "CFSVA")) |>  # Better data collection, judging from notes
  filter(!(iso3 == "BFA" & year == 2010 & `Short Source` == "SMART")) |>  # Prefer DHS to SMART
  filter(!(iso3 == "BFA" & year == 2020 & `Short Source` == "Other")) |>  # SMART has larger sample, better data collection
  filter(!(iso3 == "BFA" & year == 2021 & `Short Source` == "SMART")) |>  # Prefer DHS to SMART
  filter(!(iso3 == "BGD" & year == 2005 & `Short Source` == "Surveillance")) |>  # Surveillance was rural-only
  filter(!(iso3 == "BGD" & year == 2012 & `Short Source` == "Other")) |>   # Non-NNS source rural only, adjusted to total. Leave the two NNS, they will get averaged below.
  filter(!(iso3 == "BGD" & year == 2013 & `Short Source` != "MICS")) |>  # Prefer MICS
  filter(!(iso3 == "BGD" & year == 2014 & `Short Source` == "NNS")) |>  # Prefer DHS
  filter(!(iso3 == "BGD" & year == 2019 & `Short Source` == "Other")) |>   # Prefer MICS. The other survey was adjusted from rural.
  filter(!(iso3 == "GIN" & year == 2012 & `Short Source` == "SMART")) |>  # Prefer DHS to SMART
  filter(!(iso3 == "CAF" & year == 2019 & `Short Source` == "SMART")) |>  # Prefer MICS to SMART
  filter(!(iso3 == "GMB" & year == 2010 & `Short Source` == "Other")) |>  # Prefer MICS
  filter(!(iso3 == "GMB" & year == 2018 & `Short Source` == "Other")) |>  # Prefer MICS, larger sample
  filter(!(iso3 == "GNB" & year == 2019 & `Short Source` == "SMART")) |>  # Prefer MICS to SMART
  filter(!(iso3 == "GTM" & year == 2015 & `Short Source` == "Other")) |>  # Prefer DHS, larger sample
  filter(!(iso3 == "HTI" & year == 2012 & `Short Source` == "SMART")) |>  # Prefer DHS to SMART
  filter(!(iso3 == "MDG" & year == 2004 & `Short Source` == "Other")) |>  # Prefer DHS
  filter(!(iso3 == "MLI" & year == 2015 & `Short Source` == "SMART")) |>  # Prefer MICS to SMART, larger sample
  filter(!(iso3 == "MLI" & year == 2018 & `Short Source` == "SMART")) |>  # Prefer DHS to SMART
  filter(!(iso3 == "MNG" & year == 2010 & `Short Source` == "NNS")) |>  # Prefer MICS to NNS
  filter(!(iso3 == "MWI" & year == 2016 & `Short Source` == "SMART")) |>  # Both SMART subnational (flood zones)
  filter(!(iso3 == "MWI" & year == 2019 & `Short Source` == "SMART")) |>  # SMART subnational (flood zones)
  filter(!(iso3 == "MWI" & year == 2020 & `Short Source` == "SMART")) |>  # SMART subnational (flood/drought zones)
  filter(!(iso3 == "NER" & year == 2006 & `Short Source` == "SMART")) |>  # Prefer DHS to SMART
  filter(!(iso3 == "NER" & year == 2012 & `Short Source` == "SMART")) |>  # Prefer DHS to SMART
  filter(!(iso3 == "NGA" & year == 2018 & `Short Source` == "SMART")) |>  # Prefer DHS to SMART
  filter(!(iso3 == "NIC" & year == 1998 & `Short Source` == "LSMS")) |>  # Prefer DHS; LSMS height modality not collected
  filter(!(iso3 == "NPL" & year == 2016 & `Short Source` == "Other")) |>  # Prefer DHS, larger sample
  filter(!(iso3 == "PER" & year == 2000 & `Short Source` == "Other")) |>  # Prefer DHS, larger sample
  filter(!(iso3 == "PER" & year == 2010 & `Short Source` == "Other")) |>  # Prefer DHS
  filter(!(iso3 == "PER" & year == 2012 & `Short Source` == "Other")) |>  # Prefer DHS
  filter(!(iso3 == "PER" & year == 2015 & `Short Source` == "Other")) |>  # Prefer DHS
  filter(!(iso3 == "PER" & year == 2016 & `Short Source` == "Other")) |>  # Prefer DHS
  filter(!(iso3 == "RWA" & year == 2015 & `Short Source` == "CFSVA")) |>  # Prefer DHS; CFSVA reduced age range
  filter(!(iso3 == "SEN" & year == 2013 & `Short Source` == "SMART")) |>  # Prefer DHS to SMART
  filter(!(iso3 == "SEN" & year == 2014 & `Short Source` == "SMART")) |>  # Prefer DHS to SMART
  filter(!(iso3 == "SEN" & year == 2015 & `Short Source` == "SMART")) |>  # Prefer DHS to SMART
  filter(!(iso3 == "SEN" & year == 2019 & `Short Source` == "SMART")) |>  # Prefer DHS to SMART
  filter(!(iso3 == "SLE" & year == 2017 & `Short Source` == "SMART")) |>  # Prefer MICS to SMART
  filter(!(iso3 == "TCD" & year == 2015 & `Short Source` == "SMART")) |>  # Prefer DHS to SMART
  filter(!(iso3 == "TCD" & year == 2019 & `Short Source` == "SMART")) |>  # Prefer MICS to SMART
  filter(!(iso3 == "TGO" & year == 2010 & `Short Source` == "SMART")) |>  # Prefer MICS to SMART
  filter(!(iso3 == "UGA" & year == 2011 & `Short Source` == "LSMS")) |>  # Prefer DHS to LSMS
  filter(!(iso3 == "VNM" & year == 2020 & Source == "Viet Nam National Nutrition Surveillance Network, 2020"))  # Prefer national survey to sentinel surveillance

# The manual adjudication above covers 1996-2024 only. Competing values in
# earlier years have not been reviewed; list them so they can be adjudicated
# by methodology later (until then they fall through to the average below).
cat("\nStunting: unadjudicated competing values before 1996:\n")
stunting |>
  filter(year < 1996) |>
  add_count(iso3, year) |>
  filter(n > 1) |>
  arrange(iso3, year) |>
  select(iso3, year, `Short Source`, stunt) |>
  print(n = Inf)

# Now average remaining unresolved competing values, like the two NNS for BGD in 2012.
# Absent clear preference criteria, we just take a simple average.
stunting <- stunting |>
  summarise(stunt = mean(stunt), .by = c(iso3, year))

# Wasting data
# Construct column names
wastnames <- read_xlsx(
  here("data", "Pillar 4", "outcomes", "JME Wasting Surveys.xlsx"),
  sheet = "Trend", col_names = FALSE, range = "B7:R8"
) |> 
  t() |> as_tibble(.name_repair = ~ c("V1", "V2")) |>
  fill(V1) |> 
  mutate(nazv = ifelse(is.na(V2), V1, paste(V1, "-", V2)), .keep = "none") |>
  pull(nazv)

wasting <- read_excel(
  here("data", "Pillar 4", "outcomes", "JME Wasting Surveys.xlsx"),
  sheet = "Trend", range = "B10:R1200", col_names = wastnames
) |> 
  mutate(iso3 = ISO, year = as.integer(`Year*`),
         source_type = `Short Source`,
         title = `Full Source Title`,
         est_type = `Estimate Type`,
         wast = as.numeric(`National - Point Estimate`),
         N = `National - Sample Size`,
         note = `National - Footnote`,
         .keep = "none") |>
  filter(!is.na(iso3), !is.na(wast)) |>
  mutate(dupes = duplicated(paste(iso3, year)))

# Manually filter out competing country-year values
wasting <- wasting |>
  filter(!(iso3 == "BDI" & year == 2023 & source_type == "CFSVA")) |>  # Better data collection, judging from notes
  filter(!(iso3 == "BFA" & year == 2010 & source_type == "SMART")) |>  # Prefer DHS to SMART
  filter(!(iso3 == "BFA" & year == 2020 & source_type == "Other")) |>  # SMART has larger sample, better data collection
  filter(!(iso3 == "BFA" & year == 2021 & source_type == "SMART")) |>  # Prefer DHS to SMART
  filter(!(iso3 == "BGD" & year == 2005 & source_type == "Surveillance")) |>  # Surveillance was rural-only
  filter(!(iso3 == "BGD" & year == 2011 & source_type == "Other")) |>  # Prefer DHS
  filter(!(iso3 == "BGD" & year == 2012 & source_type == "Other")) |>  # Prefer NNS to food-security report
  filter(!(iso3 == "BGD" & year == 2013 & source_type == "Other")) |>  # Prefer MICS
  filter(!(iso3 == "BGD" & year == 2014 & source_type == "Other")) |>  # Prefer DHS
  filter(!(iso3 == "CAF" & year == 2019 & source_type == "SMART")) |>  # Prefer MICS to SMART
  filter(!(iso3 == "GIN" & year == 2012 & source_type == "SMART")) |>  # Prefer DHS to SMART
  filter(!(iso3 == "GMB" & year == 2010 & source_type == "Other")) |>  # Prefer MICS
  filter(!(iso3 == "GMB" & year == 2018 & source_type == "Other")) |>  # Prefer MICS, larger sample
  filter(!(iso3 == "GNB" & year == 2019 & source_type == "SMART")) |>  # Prefer MICS to SMART
  filter(!(iso3 == "GTM" & year == 2015 & source_type == "Other")) |>  # Prefer DHS, larger sample
  filter(!(iso3 == "HTI" & year == 2012 & source_type == "SMART")) |>  # Prefer DHS to SMART
  filter(!(iso3 == "MDG" & year == 2004 & source_type == "Other")) |>  # Prefer DHS
  filter(!(iso3 == "MLI" & year == 2015 & source_type == "SMART")) |>  # Prefer MICS to SMART, larger sample
  filter(!(iso3 == "MLI" & year == 2018 & source_type == "SMART")) |>  # Prefer DHS to SMART
  filter(!(iso3 == "MNG" & year == 2010 & source_type == "NNS")) |>  # Prefer MICS to NNS
  filter(!(iso3 == "MWI" & year == 2016 & source_type == "SMART")) |>  # Both SMART subnational (flood zones)
  filter(!(iso3 == "MWI" & year == 2019 & source_type == "SMART")) |>  # SMART subnational (flood zones)
  filter(!(iso3 == "MWI" & year == 2020 & source_type == "SMART")) |>  # SMART subnational (flood/drought zones)
  filter(!(iso3 == "NER" & year == 2006 & source_type == "SMART")) |>  # Prefer DHS to SMART
  filter(!(iso3 == "NER" & year == 2012 & source_type == "SMART")) |>  # Prefer DHS to SMART
  filter(!(iso3 == "NGA" & year == 2018 & source_type == "SMART")) |>  # Prefer DHS to SMART
  filter(!(iso3 == "NIC" & year == 1998 & source_type == "LSMS")) |>  # Prefer DHS to LSMS
  filter(!(iso3 == "NPL" & year == 2016 & source_type == "Other")) |>  # Prefer DHS, larger sample
  filter(!(iso3 == "PER" & year == 2000 & source_type == "Other")) |>  # Prefer DHS, larger sample
  filter(!(iso3 == "PER" & year == 2010 & source_type == "Other")) |>  # Prefer DHS
  filter(!(iso3 == "PER" & year == 2012 & source_type == "Other")) |>  # Prefer DHS
  filter(!(iso3 == "PER" & year == 2015 & source_type == "Other")) |>  # Prefer DHS
  filter(!(iso3 == "PER" & year == 2016 & source_type == "Other")) |>  # Prefer DHS
  filter(!(iso3 == "RWA" & year == 2015 & source_type == "CFSVA")) |>  # Prefer DHS to CFSVA
  filter(!(iso3 == "SEN" & year == 2013 & source_type == "SMART")) |>  # Prefer DHS to SMART
  filter(!(iso3 == "SEN" & year == 2014 & source_type == "SMART")) |>  # Prefer DHS to SMART
  filter(!(iso3 == "SEN" & year == 2015 & source_type == "SMART")) |>  # Prefer DHS to SMART
  filter(!(iso3 == "SEN" & year == 2019 & source_type == "SMART")) |>  # Prefer DHS to SMART
  filter(!(iso3 == "SLE" & year == 2017 & source_type == "SMART")) |>  # Prefer MICS to SMART
  filter(!(iso3 == "TCD" & year == 2015 & source_type == "SMART")) |>  # Prefer DHS to SMART
  filter(!(iso3 == "TCD" & year == 2019 & source_type == "SMART")) |>  # Prefer MICS to SMART
  filter(!(iso3 == "TGO" & year == 2010 & source_type == "SMART")) |>  # Prefer MICS to SMART
  filter(!(iso3 == "UGA" & year == 2011 & source_type == "LSMS"))  # Prefer DHS to LSMS

# As with stunting, competing values before 1996 are unadjudicated; list them
# for later review before they get averaged below.
cat("\nWasting: unadjudicated competing values before 1996:\n")
wasting |>
  filter(year < 1996) |>
  add_count(iso3, year) |>
  filter(n > 1) |>
  arrange(iso3, year) |>
  select(iso3, year, source_type, wast) |>
  print(n = Inf)

# Average remaining unresolved competing values.
wasting <- wasting |>
  summarise(wast = mean(wast), .by = c(iso3, year))


# Immunization coverage (WHO/UNICEF WUENIC) — DTP3 and measles first dose (MCV1).
# One sheet per antigen; years run across columns, one row per country.
dtp3 <- read_xlsx(here("data", "Pillar 4", "outcomes", "wuenic2025rev_web-update.xlsx")) |> 
  filter(vaccine == "dtp3") |> 
  mutate(year, iso3 = iso3c, dtp3 = coverage, .keep = "none")

mcv <- read_xlsx(here("data", "Pillar 4", "outcomes", "wuenic2025rev_web-update.xlsx")) |> 
  filter(vaccine == "mcv1") |> 
  mutate(year, iso3 = iso3c, mcv = coverage, .keep = "none")

outcomes <- reduce(
  list(igme, stunting, wasting, dtp3, mcv),
  full_join, by = c("iso3", "year")
)


# 2. Country-year spine ====

area <- read_xlsx(here("data", "areas.xlsx")) |>
  select(iso3, country, sdgregcode)

# Keep only officially recognized countries: outcome ISO3s that appear in `area`.
spine_iso3 <- sort(intersect(unique(outcomes$iso3), area$iso3))

# The spine spans every year with usable data, floored at 1989 — the first year
# of UCDP GED coverage: conflict exposure would be undefined any earlier.
spine <- expand_grid(
  iso3 = spine_iso3,
  year = 1989:max(outcomes$year, na.rm = TRUE)
) |>
  left_join(area, by = "iso3") |>
  relocate(country, sdgregcode, .after = iso3)


# 3. Population (denominator for per-100k exposure) ====
# wpp2024::pop1dt gives population in thousands. Per the package's coding
# convention, year `y` refers to calendar year `y + 1`, so we shift accordingly.

data("pop1dt", package = "wpp2024")

population <- pop1dt |>
  mutate(
    iso3      = countrycode(country_code, "iso3n", "iso3c", warn = FALSE),
    year      = as.integer(year) + 1L,
    pop_total = pop * 1000,
    .keep = "none"
  ) |>
  filter(!is.na(iso3))


# 4. Conflict exposure ====
# Then derive Jawad's four exposure specifications.

ged <- readRDS(here("data_derived", "GED_cleaned.RDS"))

battle <- ged |>
  summarise(battle_deaths = sum(best, na.rm = TRUE), .by = c(iso3, year))

exposure <- spine |>
  select(iso3, year) |>
  left_join(battle, by = c("iso3", "year")) |>
  left_join(population, by = c("iso3", "year")) |>
  mutate(
    battle_deaths       = replace_na(battle_deaths, 0),
    conflict_bin        = battle_deaths >= 25,
    conflict_cat        = factor(
      case_when(battle_deaths >= 1000 ~ "war",
                battle_deaths >= 25   ~ "minor",
                TRUE                  ~ "none"),
      levels = c("none", "minor", "war")
    ),
    battle_deaths_p100k = battle_deaths / pop_total * 1e5
  )

# Quintiles of intensity computed over conflict-affected observations only
# (battle_deaths > 0); non-conflict country-years form the "none" baseline.
q_breaks <- quantile(
  exposure$battle_deaths_p100k[exposure$battle_deaths > 0],
  probs = seq(0, 1, 0.2), na.rm = TRUE
)

exposure <- exposure |>
  mutate(
    conflict_quintile = factor(
      if_else(
        battle_deaths > 0,
        as.character(cut(battle_deaths_p100k, breaks = q_breaks,
                         labels = paste0("Q", 1:5), include.lowest = TRUE)),
        "none"
      ),
      levels = c("none", paste0("Q", 1:5)), ordered = TRUE
    )
  )


# 5. Covariates ====

# GDP per capita — World Bank WDI wide CSV (4 metadata header rows)
gdp <- read_csv(here("data", "Pillar 4", "predictors", "WB GDP Per Capita.csv"),
                skip = 4, show_col_types = FALSE) |>
  pivot_longer(matches("^[0-9]{4}$"), names_to = "year", values_to = "gdp_pc") |>
  mutate(iso3 = `Country Code`, year = as.integer(year), gdp_pc, .keep = "none") |>
  filter(!is.na(gdp_pc))

# Age dependency ratio — same World Bank WDI wide format
age_dep <- read_csv(here("data", "Pillar 4", "predictors", "WB Age Dep Ratio.csv"),
                    skip = 4, show_col_types = FALSE) |>
  pivot_longer(matches("^[0-9]{4}$"), names_to = "year", values_to = "age_dep_ratio") |>
  mutate(iso3 = `Country Code`, year = as.integer(year), age_dep_ratio, .keep = "none") |>
  filter(!is.na(age_dep_ratio))

# Urban % — WPP degree-of-urbanization; "Cities and towns" is the urban share
pct_urban <- read_csv(here("data", "Pillar 4", "predictors", "WPP Percent Urban.csv"),
                      show_col_types = FALSE) |>
  filter(Category == "Cities and towns") |>
  mutate(iso3 = Iso3, year = as.integer(Time), pct_urban = Value, .keep = "none")

# Population density — WPP, persons/km2 (mean density; NB this differs from
# Jawad's ">1,000/km2 population share" definition, but this is all I could find)
pop_density <- read_csv(here("data", "Pillar 4", "predictors", "WPP Population Density.csv"),
                        show_col_types = FALSE) |>
  mutate(iso3 = Iso3, year = as.integer(Time), pop_density = Value, .keep = "none")

# Male mean years of schooling — Our World in Data (UNDP HDR)
male_schooling <- read_csv(
  here("data", "Pillar 4", "predictors", "years-of-schooling",
       "average-years-of-schooling-among-men.csv"),
  show_col_types = FALSE
) |>
  mutate(iso3 = Code, year = as.integer(Year), male_schooling = Men, .keep = "none") |>
  filter(!is.na(iso3))

# Temperature — World Bank Climate Portal, columns "YYYY-07"
temperature <- read_excel(
  here("data", "Pillar 4", "predictors", "WB Temperature.xlsx"), sheet = "all"
) |>
  pivot_longer(matches("^[0-9]{4}-07$"), names_to = "year", values_to = "temperature") |>
  mutate(iso3 = code, year = as.integer(substr(year, 1, 4)), temperature, .keep = "none")

# Rainfall — World Bank Climate Portal, columns "YYYY-07"
rainfall <- read_excel(
  here("data", "Pillar 4", "predictors", "WB Rainfall.xlsx"), sheet = "all"
) |>
  pivot_longer(matches("^[0-9]{4}-07$"), names_to = "year", values_to = "rainfall") |>
  mutate(iso3 = code, year = as.integer(substr(year, 1, 4)), rainfall, .keep = "none")

# Earthquakes & droughts — EM-DAT, binary presence per country-year
disasters <- read_excel(
  here("data", "Pillar 4", "predictors", "EM-DAT Earthquakes Droughts.xlsx"),
  sheet = "EM-DAT Data"
) |>
  mutate(iso3 = ISO, year = as.integer(`Start Year`), type = `Disaster Type`,
         .keep = "none") |>
  group_by(iso3, year) |>
  summarise(
    earthquake = as.integer(any(type == "Earthquake")),
    drought    = as.integer(any(type == "Drought")),
    .groups = "drop"
  )

# Ethnic fractionalization — HIEF (coverage ends 2013; Jawad uses it sensitivity-only)
ethnic_frac <- read_csv(
  here("data", "Pillar 4", "predictors", "Ethnic Fractionalization.csv"),
  show_col_types = FALSE
) |>
  mutate(iso3 = countrycode(Country, "country.name", "iso3c", warn = FALSE),
         year = as.integer(Year), ethnic_frac = EFindex, .keep = "none") |>
  filter(!is.na(iso3)) |>
  # HIEF carries a few duplicate country-years; collapse to one value
  group_by(iso3, year) |>
  summarise(ethnic_frac = mean(ethnic_frac), .groups = "drop")

# OECD membership — binary, active from the accession year onward
oecd <- read_csv(here("data", "Pillar 4", "predictors", "OECD Membership.csv"),
                 show_col_types = FALSE)

# Electoral democracy index — V-Dem polyarchy
democracy <- vdem |>
  mutate(iso3 = country_text_id, year = as.integer(year),
         dem_idx = v2x_polyarchy, .keep = "none")


# 6. Assemble the master panel ====

master <- spine |>
  left_join(outcomes,       by = c("iso3", "year")) |>
  left_join(exposure,       by = c("iso3", "year")) |>
  left_join(gdp,            by = c("iso3", "year")) |>
  left_join(age_dep,        by = c("iso3", "year")) |>
  left_join(pct_urban,      by = c("iso3", "year")) |>
  left_join(pop_density,    by = c("iso3", "year")) |>
  left_join(male_schooling, by = c("iso3", "year")) |>
  left_join(temperature,    by = c("iso3", "year")) |>
  left_join(rainfall,       by = c("iso3", "year")) |>
  left_join(disasters,      by = c("iso3", "year")) |>
  left_join(ethnic_frac,    by = c("iso3", "year")) |>
  left_join(oecd,           by = "iso3") |>
  left_join(democracy,      by = c("iso3", "year")) |>
  mutate(
    # EM-DAT has global coverage, so a non-match is a true zero
    earthquake   = replace_na(earthquake, 0L),
    drought      = replace_na(drought, 0L),
    oecd_member  = as.integer(!is.na(year_joined) & year >= year_joined)
  ) |>
  select(
    iso3, country, sdgregcode, year,
    u5mr, imr, nnmr, stunt, wast, dtp3, mcv,
    battle_deaths, conflict_bin, conflict_cat, battle_deaths_p100k,
    conflict_quintile, pop_total,
    gdp_pc, age_dep_ratio, pct_urban, pop_density, male_schooling,
    temperature, rainfall, earthquake, drought, ethnic_frac,
    oecd_member, dem_idx
  ) |>
  arrange(iso3, year)


# 7. Coverage summary & save ====

cat("\nMaster panel:", nrow(master), "rows,",
    n_distinct(master$iso3), "countries,",
    min(master$year), "-", max(master$year), "\n\n")

cat("Non-missing counts by column:\n")
master |>
  summarise(across(everything(), \(x) sum(!is.na(x)))) |>
  pivot_longer(everything(), names_to = "column", values_to = "n_nonmissing") |>
  mutate(pct = round(100 * n_nonmissing / nrow(master), 1)) |>
  print(n = Inf)

saveRDS(master, here("data_derived", "pillar4_master.rds"))
