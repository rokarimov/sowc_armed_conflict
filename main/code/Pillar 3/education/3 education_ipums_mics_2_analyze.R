# Project: SOWC 2026
# Goal: Compare net and gross school attendance ratios (NAR, GAR) for primary
#       and secondary education inside and outside conflict zones using MICS
#       survey data from IPUMS.
# Author: Rouslan Karimov
# Organization: UNICEF
# Created: 25 May 2026


library(here)
library(tidyverse)
library(haven)
library(readxl)
library(sf)
library(survey)

options(survey.lonely.psu = "adjust")


# Load data ==================================================================

buffers <- readRDS(here("data_derived", "ged_country_year_buffers.rds"))

# IPUMS MICS education extract (HL: household-listing level), already joined
# with HH stratum/PSU and iso3 in step 1. zap_labels keeps comparisons numeric.
ipums <- readRDS(here("data_derived", "mics_education_for_analysis.rds")) |>
  zap_labels()


# UNESCO school-age lookup ===================================================
# Indicators kept: 299905 (primary entry age), 999975 (lower-secondary entry),
# 999977 (upper-secondary entry, unused — age_sec_max is capped at 18 per the
# DHS Chap02_PH default), 403 (academic-year start month).
unesco_raw <- read_excel(here("data", "UNESCO Info",
                              "school_age_calendar_info.xlsx"),
                         sheet = "Data") |>
  suppressWarnings() |>
  filter(indicatorId %in% c("299905", "999975", "999977", "403")) |>
  select(iso3 = geoUnit, year, indicatorId, value)

unesco <- unesco_raw |>
  pivot_wider(names_from = indicatorId, values_from = value,
              values_fn = mean) |>
  transmute(
    iso3,
    year            = as.integer(year),
    age_prim_min    = as.integer(`299905`),
    age_prim_max    = as.integer(pmax(`299905`, `999975` - 1)),
    age_sec_min     = as.integer(`999975`),
    age_sec_max     = 18L,
    school_start_mo = if_else(`403` == 0, NA_real_, `403`)
  )

# Nearest-year fallback within a country: UNESCO covers 1996-2025, entrance
# ages and start months change rarely, so this safely covers earlier surveys
# and small gap years.
LookupSchoolAges <- function(this_iso3, this_year) {
  d <- unesco[unesco$iso3 == this_iso3 & !is.na(unesco$school_start_mo), ]
  if (nrow(d) == 0) return(NULL)
  d[which.min(abs(d$year - this_year)), ]
}


# Prepare survey rows ========================================================
# MICS HL module is implicitly de-jure (no visitors), so no residence filter
# is needed — unlike DHS, which uses hhslept == 1. Sentinels: birthyr /
# interviewyr use 9990+ as missing; interviewmo / birthmo use 90+ likewise.
# edlevelcurr is in IPUMS-MICS' 3-digit hierarchical scheme: 100s less-than-
# primary / pre-primary, 200s primary, 300s lower-secondary, 400s upper-
# secondary, 500+ post-secondary; 996/997/998/999 inconsistent / DK / missing
# / NIU.
ipums <- ipums |>
  filter(age >= 5, age <= 24,
         latitude != 0, longitude != 0,
         !is.na(latitude), !is.na(longitude)) |>
  mutate(
    interviewyr  = if_else(interviewyr >= 9990, NA_real_, interviewyr),
    interviewmo  = if_else(interviewmo >= 90,   NA_real_, interviewmo),
    birthyr      = if_else(birthyr     >= 9990, NA_real_, birthyr),
    birthmo      = if_else(birthmo     >= 90,   NA_real_, birthmo),
    schoolcurr   = if_else(schoolcurr  %in% c(7, 8, 9), NA_real_, schoolcurr),
    edlevelcurr  = if_else(edlevelcurr >= 996,          NA_real_, edlevelcurr),
    in_prim = case_when(
      schoolcurr == 0                                     ~ 0L,
      schoolcurr == 1 & edlevelcurr %in% 200:299          ~ 1L,
      schoolcurr == 1 & !is.na(edlevelcurr)               ~ 0L,
      .default = NA_integer_),
    in_sec  = case_when(
      schoolcurr == 0                                     ~ 0L,
      schoolcurr == 1 & edlevelcurr %in% 300:499          ~ 1L,
      schoolcurr == 1 & !is.na(edlevelcurr)               ~ 0L,
      .default = NA_integer_),
    # Use the reported birth month when available; mid-year (6) when missing.
    birth_cmc = (birthyr - 1900) * 12 + coalesce(birthmo, 6)
  ) |>
  filter(!is.na(interviewyr), !is.na(interviewmo), !is.na(birthyr))


# Per-zone power diagnostic — identical to nutrition / DHS-education pipelines.
ZoneDiag <- function(design, sub_d, outcome, psu_var) {
  n  <- nrow(sub_d)
  nc <- n_distinct(sub_d[[psu_var]])
  if (n < 2 || nc < 2) return(list(n = n, nc = nc, m_bar = NA_real_,
                                   deff = NA_real_, rho = NA_real_,
                                   n_eff = NA_real_))
  f  <- as.formula(paste0("~", outcome))
  m  <- tryCatch(svymean(f, design, na.rm = TRUE), error = function(e) NULL)
  if (is.null(m)) return(list(n = n, nc = nc, m_bar = n / nc,
                              deff = NA_real_, rho = NA_real_,
                              n_eff = NA_real_))
  p  <- as.numeric(coef(m))[1]
  se <- as.numeric(SE(m))[1]
  srs_var <- p * (1 - p) / n
  deff_v  <- if (isTRUE(srs_var > 0)) (se^2) / srs_var else NA_real_
  m_bar   <- n / nc
  rho     <- if (!is.na(deff_v) && m_bar > 1) (deff_v - 1) / (m_bar - 1) else NA_real_
  n_eff   <- if (!is.na(deff_v) && deff_v > 0) n / deff_v else NA_real_
  list(n = n, nc = nc, m_bar = m_bar, deff = deff_v, rho = rho, n_eff = n_eff)
}


# Per-survey × outcome × exposure-threshold result row. NAR = svymean(in_level)
# restricted to age-eligible subset; GAR = svyratio(in_level, age_flag) over
# the full universe, so GAR can exceed 1 when there are over-/under-age
# attenders.
AnalyzeOne <- function(d, des, level, rate, exposed_var, threshold_lbl) {
  age_flag  <- if (level == "prim") "prim_age" else "sec_age"
  in_level  <- if (level == "prim") "in_prim"  else "in_sec"
  outcome   <- paste0(rate, "_", level)

  # NAR is defined only over the official-age subset; GAR over the full
  # universe. Subset the design by row index — equivalent to subset() but
  # avoids NSE plumbing.
  keep <- !is.na(d[[in_level]]) & !is.na(d[[age_flag]])
  if (rate == "nar") keep <- keep & d[[age_flag]] == 1L
  d_o   <- d[keep, ]
  des_o <- des[keep, ]

  if (nrow(d_o) == 0) {
    return(tibble(
      outcome = outcome, threshold = threshold_lbl,
      n = 0L, prev_overall = NA_real_,
      prev_inside = NA_real_, prev_outside = NA_real_,
      pvalue = NA_real_,
      n_clust_in = NA_integer_, n_clust_out = NA_integer_,
      deff_in = NA_real_, deff_out = NA_real_,
      rho_in  = NA_real_, rho_out  = NA_real_,
      n_eff_in = NA_real_, n_eff_out = NA_real_,
      mde = NA_real_, trustworthy = NA))
  }

  ex      <- d_o[[exposed_var]] %in% TRUE
  des_in  <- des_o[ ex, ]
  des_out <- des_o[!ex, ]
  d_in    <- d_o[ ex, ]
  d_out   <- d_o[!ex, ]

  safeStat <- function(design) {
    if (rate == "nar") {
      f <- as.formula(paste0("~", in_level))
      tryCatch(as.numeric(coef(svymean(f, design, na.rm = TRUE)))[1],
               error = function(e) NA_real_)
    } else {
      tryCatch(as.numeric(coef(svyratio(
        as.formula(paste0("~", in_level)),
        as.formula(paste0("~", age_flag)),
        design, na.rm = TRUE)))[1],
        error = function(e) NA_real_)
    }
  }

  z_in  <- ZoneDiag(des_in,  d_in,  in_level, "imicspsu")
  z_out <- ZoneDiag(des_out, d_out, in_level, "imicspsu")

  p_bar <- mean(d_o[[in_level]], na.rm = TRUE)
  mde <- if (!is.na(z_in$n_eff) && !is.na(z_out$n_eff) &&
             z_in$n_eff > 0 && z_out$n_eff > 0) {
    n_eff_h <- 2 / (1 / z_in$n_eff + 1 / z_out$n_eff)
    (qnorm(0.975) + qnorm(0.80)) * sqrt(2 * p_bar * (1 - p_bar) / n_eff_h)
  } else NA_real_

  trustworthy <- !is.na(mde) &
                 z_in$nc    >= 25 & z_out$nc    >= 25 &
                 z_in$n_eff >= 30 & z_out$n_eff >= 30 &
                 mde <= 0.10

  # NAR: svyttest on the age-eligible subset. GAR: z-test on the ratio
  # difference; design subsets are disjoint (exposed vs not), so we treat the
  # covariance as 0 — adequate for a screening test.
  pvalue <- if (rate == "nar") {
    tryCatch(
      as.numeric(svyttest(as.formula(paste0(in_level, " ~ ", exposed_var)),
                          des_o)$p.value),
      error = function(e) NA_real_)
  } else {
    r_in  <- tryCatch(svyratio(as.formula(paste0("~", in_level)),
                               as.formula(paste0("~", age_flag)),
                               des_in,  na.rm = TRUE), error = function(e) NULL)
    r_out <- tryCatch(svyratio(as.formula(paste0("~", in_level)),
                               as.formula(paste0("~", age_flag)),
                               des_out, na.rm = TRUE), error = function(e) NULL)
    if (is.null(r_in) || is.null(r_out)) NA_real_ else {
      diff <- as.numeric(coef(r_in)) - as.numeric(coef(r_out))
      se   <- sqrt(as.numeric(SE(r_in))^2 + as.numeric(SE(r_out))^2)
      if (is.na(se) || se == 0) NA_real_ else
        2 * (1 - pnorm(abs(diff / se)))
    }
  }

  tibble(
    outcome      = outcome,
    threshold    = threshold_lbl,
    n            = nrow(d_o),
    prev_overall = safeStat(des_o),
    prev_inside  = safeStat(des_in),
    prev_outside = safeStat(des_out),
    pvalue       = pvalue,
    n_clust_in   = z_in$nc,    n_clust_out = z_out$nc,
    deff_in      = z_in$deff,  deff_out    = z_out$deff,
    rho_in       = z_in$rho,   rho_out     = z_out$rho,
    n_eff_in     = z_in$n_eff, n_eff_out   = z_out$n_eff,
    mde          = mde,
    trustworthy  = trustworthy
  )
}


# Per-survey: look up UNESCO age cutoffs, compute school_age and per-person
# in/out conflict-zone exposure, then run AnalyzeOne for each (level × rate ×
# exposure-threshold) cell.
AnalyzeSurvey <- function(d) {
  this_iso3 <- d$iso3[1]
  this_year <- d$year[1]

  ages <- LookupSchoolAges(this_iso3, this_year)
  if (is.null(ages)) {
    message("Skipping ", this_iso3, " ", this_year, " — no UNESCO age data.")
    return(NULL)
  }

  # Academic-year start CMC: if interview month is before the school-year
  # start month, the kid is still in the previous calendar year's school year.
  start_year <- ifelse(d$interviewmo >= ages$school_start_mo,
                       d$interviewyr, d$interviewyr - 1)
  acad_start_cmc <- (start_year - 1900) * 12 + ages$school_start_mo
  d$school_age <- as.integer(floor((acad_start_cmc - d$birth_cmc) / 12))
  d$school_age <- if_else(d$school_age >= 0 & d$school_age <= 30,
                          d$school_age, NA_integer_)

  d$prim_age <- as.integer(d$school_age >= ages$age_prim_min &
                           d$school_age <= ages$age_prim_max)
  d$sec_age  <- as.integer(d$school_age >= ages$age_sec_min &
                           d$school_age <= ages$age_sec_max)

  clusters_sf <- d |>
    distinct(cluster, latitude, longitude) |>
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

  buffers_c <- buffers |> filter(iso3 == this_iso3)
  if (nrow(buffers_c) > 0) {
    hits <- st_intersects(clusters_sf, buffers_c, sparse = FALSE)
    buffer_years <- buffers_c$year
    cluster_idx  <- match(d$cluster, clusters_sf$cluster)

    d$conflict_exposure <- vapply(seq_len(nrow(d)), function(i) {
      iyr <- d$interviewyr[i]; age <- d$age[i]
      if (is.na(iyr) || is.na(age)) return(NA_integer_)
      sum(hits[cluster_idx[i], ] & buffer_years %in% (iyr - age):iyr)
    }, integer(1))
  } else {
    d$conflict_exposure <- 0L
  }
  d$exposed  <- d$conflict_exposure > 0
  d$exposed3 <- d$conflict_exposure > 2
  d$exposed5 <- d$conflict_exposure > 4

  des <- svydesign(id = ~imicspsu, strata = ~imicsstratum,
                   weights = ~weighthh, data = d, nest = TRUE)

  grid <- expand_grid(
    level    = c("prim", "sec"),
    rate     = c("nar", "gar"),
    exposure = c(exposed = "exposed", exposed3 = "exposed3", exposed5 = "exposed5")
  )

  pmap(grid, function(level, rate, exposure) {
    AnalyzeOne(d, des, level, rate, exposure,
               threshold_lbl = c(exposed = ">0y", exposed3 = ">2y",
                                 exposed5 = ">4y")[exposure])
  }) |>
    list_rbind() |>
    mutate(source    = "MICS",
           sample_id = d$sample[1],
           iso3      = this_iso3,
           year      = this_year,
           .before   = 1)
}

results <- ipums |>
  group_split(sample) |>
  map(AnalyzeSurvey) |>
  list_rbind() |>
  arrange(iso3, year, outcome, threshold)

write_csv(results, here("data_derived", "education_ipums_mics_results.csv"))


# Visualize ==================================================================
every_nth <- function(n) function(x) x[c(TRUE, rep(FALSE, n - 1))]

results |>
  filter(outcome == "nar_prim", threshold == ">0y") |>
  mutate(svyr   = paste0(iso3, year),
         diff   = (prev_outside - prev_inside) * 100,
         signif = pvalue < 0.051) |>
  filter(!is.na(diff)) |>
  ggplot(aes(x = reorder(svyr, diff), y = diff, fill = signif)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(breaks = every_nth(n = 5)) +
  labs(x = "Surveys", y = "Primary NAR pp difference (outzone - inzone)") +
  coord_flip() +
  theme_bw()



# Fin
