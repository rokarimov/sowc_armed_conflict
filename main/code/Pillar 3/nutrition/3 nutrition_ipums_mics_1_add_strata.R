# Project: SOWC 2026
# Goal: Build a pooled master dataset of MICS children-under-5 nutrition
#       records, merging in stratum/PSU info from the household files.
# Author: Rouslan Karimov
# Organization: UNICEF
# Created: 18 May 2026

library(here)
library(haven)
library(dplyr)

ch <- read_dta(here("data_derived", "ipums_mics_extract_nutrition_gps.dta"))
hh <- read_dta(here("data_derived", "ipums_mics_extract_hh_characteristics_gps.dta"))

# Per IPUMS MICS linking guide (https://mics.ipums.org/mics/linking.shtml),
# households link to children many-to-one on SAMPLE + CLUSTER + HHNO.
hh_keep <- hh |>
  select(sample, cluster, hhno,
         stratum, imicsstratum, psu, imicspsu, weighthh, hhrespondent) |>
  rename(psu_hh = psu, imicspsu_hh = imicspsu) |>
  distinct(sample, cluster, hhno, .keep_all = TRUE)

# IPUMS stores sample_str / country_iso3 as Python bytes-literal strings
# (e.g. "b'ng2021a'"); convert to plain values.
df <- ch |>
  left_join(hh_keep, by = c("sample", "cluster", "hhno")) |>
  mutate(
    psu        = coalesce(psu, psu_hh),
    imicspsu   = coalesce(imicspsu, imicspsu_hh),
    sample_str = sub("^b'(.*)'$", "\\1", sample_str),
    iso3       = sub("^b'(.*)'$", "\\1", iso3)
  ) |>
  select(-psu_hh, -imicspsu_hh)

saveRDS(df, here("data_derived", "mics_nutrition_for_analysis.rds"))

# Fin
