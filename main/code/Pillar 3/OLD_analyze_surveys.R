# Project: SOWC 2026
# Goal: Survey-based subnational analysis of child outcomes inside/outside conflict zones
# Author: Rouslan Karimov
# Organization: UNICEF
# Created: 30 March 2026


library(here)
library(tidyverse)
library(labelled)
library(survey)
library(DHS.rates)


load(here("data_derived", "dhs_conflict_data.RData"))


# Sanity-check: count records missing conflict-zone flags (NA's)
table(ir$near_conflict, useNA = "always")
table(kr$near_conflict, useNA = "always")
table(br$near_conflict, useNA = "always")


# DPT1 vaccination inside vs outside conflict zones ====
kr$wgt  <- kr$v005 / 1000000
kr$dpt1 <- case_when(
  kr$h3 %in% c(1, 2, 3) ~ 1,     # card (with/without date) or mother's recall
  kr$h3 == 0            ~ 0,
  .default = NA_real_            # exclude don't know / missing
)

kr_design <- svydesign(
  id = ~v021,
  strata = ~v022,
  weights = ~wgt,
  data = kr,
  nest = TRUE
)

design_12_23 <- subset(kr_design, b19 >= 12 & b19 <= 23)  # Focusing only on target population of 12-23 month olds

svymean(~dpt1, design = design_12_23, na.rm = TRUE)

svyby(~dpt1, by = ~near_conflict, design = design_12_23, FUN = svymean, na.rm = TRUE)
svyttest(dpt1 ~ near_conflict, design = design_12_23)


# Mortality ====

chmort(br)  # Overall
(mort <- chmort(br, Class = "near_conflict", JK = "Yes") |>
    rownames_to_column())  # By conflict zone status


# Test if IMR's are statistically different for kids inside and outside conflict zones
# (paired t-test)

df <- length(unique(br$v021)) - length(unique(br$v022))  # Degrees of freedom

m1 <- mort$R[mort$rowname == "IMR"]
m2 <- mort$R[mort$rowname == "IMR1"]

se1 <- mort$SE[mort$rowname == "IMR"]
se2 <- mort$SE[mort$rowname == "IMR1"]

diff <- m1 - m2
se_diff <- sqrt(se1^2 + se2^2)

t <- diff / se_diff
(p <- 2 * pt(-abs(t), df))
(ci <- diff + c(-1, 1) * qt(0.975, df) * se_diff)


# Test if U5MR's are statistically different for kids inside and outside conflict zones
# (paired t-test)

m1 <- mort$R[mort$rowname == "U5MR"]
m2 <- mort$R[mort$rowname == "U5MR1"]

se1 <- mort$SE[mort$rowname == "U5MR"]
se2 <- mort$SE[mort$rowname == "U5MR1"]

diff <- m1 - m2
se_diff <- sqrt(se1^2 + se2^2)

t <- diff / se_diff
(p <- 2 * pt(-abs(t), df))
(ci <- diff + c(-1, 1) * qt(0.975, df) * se_diff)


# ROBUSTNESS CHECK - TO COME
# Can use migration status to filter on respondents who never moved.
# See Guide to DHS Statistics (DHS8) > Respondents' Characteristics >
# Migration (p 3.38)
