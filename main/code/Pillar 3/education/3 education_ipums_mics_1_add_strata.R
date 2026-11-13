# Project: SOWC 2026
# Goal: Build a pooled master dataset of MICS household-listing records for
#       school-age members, merging in stratum/PSU info from the household
#       files.
# Author: Rouslan Karimov
# Organization: UNICEF
# Created: 25 May 2026

library(here)
library(haven)
library(dplyr)

hl <- read_dta(here("data_derived", "ipums_mics_extract_education_gps.dta"))
hh <- read_dta(here("data_derived", "ipums_mics_extract_hh_characteristics_gps.dta"))

# Per IPUMS MICS linking guide (https://mics.ipums.org/mics/linking.shtml),
# households link to HL many-to-one on SAMPLE + CLUSTER + HHNO.
hh_keep <- hh |>
  select(sample, cluster, hhno,
         stratum, imicsstratum, psu, imicspsu, weighthh, hhrespondent) |>
  rename(psu_hh = psu, imicspsu_hh = imicspsu, weighthh_hh = weighthh) |>
  distinct(sample, cluster, hhno, .keep_all = TRUE)

# IPUMS holds sample_str / iso3 as Python bytes-literal strings (e.g.
# "b'ng2018a'"); convert to plain values when present.
df <- hl |>
  left_join(hh_keep, by = c("sample", "cluster", "hhno")) |>
  mutate(
    psu      = coalesce(psu, psu_hh),
    imicspsu = coalesce(imicspsu, imicspsu_hh),
    weighthh = coalesce(weighthh, weighthh_hh),
    iso3     = sub("^b'(.*)'$", "\\1", iso3)
  ) |>
  select(-psu_hh, -imicspsu_hh, -weighthh_hh)

saveRDS(df, here("data_derived", "mics_education_for_analysis.rds"))

# Fin
