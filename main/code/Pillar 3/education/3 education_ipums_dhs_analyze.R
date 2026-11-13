# Project: SOWC 2026
# Goal: Compare net and gross school attendance ratios (NAR, GAR) for primary
#       and lower-/upper-secondary education inside and outside conflict zones
#       using DHS survey data from IPUMS.
# Author: Rouslan Karimov
# Organization: UNICEF
# Created: 25 May 2026


library(here)
library(tidyverse)
library(haven)
library(readxl)
library(sf)
library(survey)
library(countrycode)
library(furrr)

options(survey.lonely.psu = "adjust")


# Load data ==================================================================

# Conflict-zone buffers (sf, country-year polygons)
buffers <- readRDS(here("data_derived", "ged_country_year_buffers.rds"))

# IPUMS DHS education extract (PR-equivalent: household-member level, ages
# 5-24 after the Stata slim-down step). col_select keeps only what the
# pipeline actually reads — drops ~6 cols including hhbirthyr (we use birth
# CMC from births.dta), urbanhh, sex, hhresident, edlevel, hhnumall.
ipums <- read_dta(
  here("data", "IPUMS extracts", "DHS", "education.dta"),
  col_select = c(sample, country, year,
                 idhshid, idhspsu, idhsstrata, clusternoall, hhlineno,
                 hhweight, hhintyr, hhintmo, hhintcmc,
                 hhage, hhslept,
                 schoolnow, edlevelnow,
                 gpslat, gpslong)) |>
  zap_labels()

# DHS births extract (slimmed in Stata to 4 cols) — attaches CMC-precise
# dates of birth to children whose mothers are in IR. Orphans / mothers
# away fall back to hhintcmc - hhage*12 - 6 below.
births <- read_dta(
  here("data", "IPUMS extracts", "DHS", "births.dta"),
  col_select = c(idhshid, linenokid, kiddobcmc)) |>
  zap_labels() |>
  distinct(idhshid, linenokid, .keep_all = TRUE)


# UNESCO school-age lookup ===================================================
# Indicators kept: 299905 (primary entry age), 999975 (lower-secondary entry),
# 999977 (upper-secondary entry), 403 (academic-year start month). Per project
# decision (see [[feedback_no_config_constants]]), age_sec_max is hard-capped
# at 18 (the DHS Chap02_PH default) since the UNESCO file does not expose an
# upper-secondary duration. School_start_mo == 0 means "unknown" → NA.
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

# Per-survey lookup: nearest available UNESCO row for the country. UNESCO
# covers 1996-2025; entrance ages and academic-year start months change
# rarely, so a single-country nearest-year fallback is a safe stand-in for
# surveys that fall outside that window or in gap years.
LookupSchoolAges <- function(this_iso3, this_year) {
  d <- unesco[unesco$iso3 == this_iso3 & !is.na(unesco$school_start_mo), ]
  if (nrow(d) == 0) return(NULL)
  d[which.min(abs(d$year - this_year)), ]
}


# Prepare survey rows ========================================================
# Universe matches the DHS PH_SCHOL.R convention: de-facto household members
# (hhslept == 1) aged 5-24 with usable GPS. Sentinel codes for schoolnow /
# edlevelnow ride above 90 — convert to NA so they don't masquerade as zeros.
ipums <- ipums |>
  filter(hhslept == 1,
         hhage >= 5, hhage <= 24,
         gpslat != 0, gpslong != 0,
         !is.na(gpslat), !is.na(gpslong),
         !is.na(hhintyr), !is.na(hhintmo)) |>
  mutate(
    iso3        = countrycode(country, origin = "iso3n", destination = "iso3c"),
    edlevelnow  = if_else(edlevelnow >= 7, NA_real_, edlevelnow),
    schoolnow   = if_else(schoolnow  %in% c(97, 98, 99), NA_real_, schoolnow),
    in_prim     = case_when(edlevelnow == 1 ~ 1L,
                            !is.na(edlevelnow) ~ 0L,
                            .default = NA_integer_),
    in_sec      = case_when(edlevelnow == 2 ~ 1L,
                            !is.na(edlevelnow) ~ 0L,
                            .default = NA_integer_),
    wgt         = hhweight / 1e6
  ) |>
  left_join(births, by = c("idhshid", "hhlineno" = "linenokid")) |>
  mutate(
    birth_cmc = coalesce(
      kiddobcmc,
      # Synthetic CMC: interview CMC minus age in months, anchored at the
      # middle of the year (offset 6) for deterministic reproducibility.
      hhintcmc - hhage * 12 - 6
    )
  )


# Per-zone power diagnostic — same logic as the nutrition pipeline.
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


# Per-survey × outcome × exposure-threshold result row.
# NAR primary = mean(in_prim | prim_age) — restricted to children of official
# primary-school age. GAR primary = sum(in_prim) / sum(prim_age) over the
# whole 5-24 universe — captures over-/under-age primary attenders, so can
# exceed 1. Same logic for secondary.
AnalyzeOne <- function(d, des, level, rate, exposed_var, threshold_lbl) {
  age_flag  <- if (level == "prim") "prim_age" else "sec_age"
  in_level  <- if (level == "prim") "in_prim"  else "in_sec"
  outcome   <- paste0(rate, "_", level)

  # NAR is defined only over the official-age subset; GAR over the full
  # universe (numerator captures any-age attenders, denominator is age-eligible
  # population). Either way we drop rows where in_level is NA. Subset the
  # design by row index — equivalent to subset() but avoids NSE plumbing.
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

  # NAR: a simple weighted proportion → svymean. GAR: ratio of two totals
  # (attenders of any age over age-eligible kids) → svyratio. svyratio handles
  # the design-correct SE for both numerator and denominator.
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

  z_in  <- ZoneDiag(des_in,  d_in,  in_level, "idhspsu")
  z_out <- ZoneDiag(des_out, d_out, in_level, "idhspsu")

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

  # For NAR we use svyttest (a true mean comparison). For GAR we approximate
  # with a z-test on the difference of two ratios — independent design subsets
  # so covariance ≈ 0. Imperfect but matches the spirit of the nutrition row.
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
# in/out conflict-zone exposure, then run AnalyzeOne for each (outcome × rate
# × exposure-threshold) cell.
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
  start_year <- ifelse(d$hhintmo >= ages$school_start_mo,
                       d$hhintyr, d$hhintyr - 1)
  acad_start_cmc <- (start_year - 1900) * 12 + ages$school_start_mo
  d$school_age <- as.integer(floor((acad_start_cmc - d$birth_cmc) / 12))
  d$school_age <- if_else(d$school_age >= 0 & d$school_age <= 30,
                          d$school_age, NA_integer_)

  d$prim_age <- as.integer(d$school_age >= ages$age_prim_min &
                           d$school_age <= ages$age_prim_max)
  d$sec_age  <- as.integer(d$school_age >= ages$age_sec_min &
                           d$school_age <= ages$age_sec_max)

  clusters_sf <- d |>
    distinct(clusternoall, gpslat, gpslong) |>
    st_as_sf(coords = c("gpslong", "gpslat"), crs = 4326)

  buffers_c <- buffers |> filter(iso3 == this_iso3)
  if (nrow(buffers_c) > 0) {
    hits <- st_intersects(clusters_sf, buffers_c, sparse = FALSE)
    buffer_years <- buffers_c$year
    cluster_idx  <- match(d$clusternoall, clusters_sf$clusternoall)

    # Exposure window mirrors nutrition: years of life from birth to interview
    # year. For older school-age children that includes pre-school years —
    # consistent with the literature on cumulative early-life conflict shocks
    # affecting later schooling outcomes.
    d$conflict_exposure <- vapply(seq_len(nrow(d)), function(i) {
      iyr <- d$hhintyr[i]; age <- d$hhage[i]
      if (is.na(iyr) || is.na(age)) return(NA_integer_)
      sum(hits[cluster_idx[i], ] & buffer_years %in% (iyr - age):iyr)
    }, integer(1))
  } else {
    d$conflict_exposure <- 0L
  }
  d$exposed  <- d$conflict_exposure > 0
  d$exposed3 <- d$conflict_exposure > 2
  d$exposed5 <- d$conflict_exposure > 4

  des <- svydesign(id = ~idhspsu, strata = ~idhsstrata,
                   weights = ~wgt, data = d, nest = TRUE)

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
    mutate(source    = "DHS",
           sample_id = d$sample[1],
           iso3      = this_iso3,
           year      = this_year,
           .before   = 1)
}

# Per-survey work is embarrassingly parallel — each sample is a self-contained
# svydesign with no cross-survey dependencies. multisession (not multicore)
# because macOS + sf/survey have known issues with R's fork-based parallelism.
plan(multisession, workers = 6)

results <- ipums |>
  group_split(sample) |>
  future_map(AnalyzeSurvey, .progress = TRUE,
             .options = furrr_options(seed = TRUE)) |>
  list_rbind() |>
  arrange(iso3, year, outcome, threshold)

plan(sequential)   # release worker processes

write_csv(results, here("data_derived", "education_ipums_dhs_results.csv"))


# Sanity check vs. DHS-published NAR/GAR =====================================
# ED_NARP_B_BTH / ED_NARS_B_BTH = primary / secondary net attendance ratio
# (total). ED_GARP_B_BTH / ED_GARS_B_BTH = primary / secondary gross
# attendance ratio (total). See docs/DHS_API_indicator_codes.html.
results <- results |>
  mutate(dhs_cc = countrycode(iso3, "iso3c", "dhs"))

dhs_codes <- na.omit(unique(results$dhs_cc))

FetchDhsData <- function(indicator_ids, country_codes) {
  base <- "https://api.dhsprogram.com/rest/dhs/data"
  api_key <- Sys.getenv("DHS_API_KEY")
  qs <- paste0(
    "?indicatorIds=", paste(indicator_ids, collapse = ","),
    "&countryIds=",   paste(country_codes,  collapse = ","),
    "&breakdown=all&perpage=1000&f=json",
    if (nzchar(api_key)) paste0("&apiKey=", api_key) else ""
  )
  pages <- list(); page <- 1
  repeat {
    resp <- jsonlite::fromJSON(paste0(base, qs, "&page=", page),
                               simplifyDataFrame = TRUE)
    pages[[page]] <- resp$Data
    if (page >= resp$TotalPages) break
    page <- page + 1
  }
  bind_rows(pages)
}

dhs_published <- FetchDhsData(
  c("ED_NARP_B_BTH", "ED_NARS_B_BTH", "ED_GARP_B_BTH", "ED_GARS_B_BTH"),
  dhs_codes) |>
  filter(IsPreferred == 1, SurveyType == "DHS",
         CharacteristicCategory == "Total") |>
  transmute(
    dhs_cc       = DHS_CountryCode,
    survey_year  = SurveyYear,
    survey_lbl   = SurveyYearLabel,
    indicator_id = IndicatorId,
    value_pct    = Value
  )

dhs_wide <- dhs_published |>
  pivot_wider(names_from = indicator_id, values_from = value_pct,
              values_fn = mean) |>
  rename(pub_nar_prim = ED_NARP_B_BTH, pub_nar_sec = ED_NARS_B_BTH,
         pub_gar_prim = ED_GARP_B_BTH, pub_gar_sec = ED_GARS_B_BTH)

comparison <- results |>
  filter(threshold == ">0y") |>
  pivot_wider(id_cols = c(iso3, year, dhs_cc),
              names_from = outcome, values_from = prev_overall) |>
  left_join(dhs_wide, by = c("dhs_cc", "year" = "survey_year")) |>
  mutate(across(c(nar_prim, nar_sec, gar_prim, gar_sec),
                ~ round(100 * ., 1))) |>
  mutate(diff_nar_prim = nar_prim - pub_nar_prim,
         diff_nar_sec  = nar_sec  - pub_nar_sec,
         diff_gar_prim = gar_prim - pub_gar_prim,
         diff_gar_sec  = gar_sec  - pub_gar_sec) |>
  select(iso3, year, survey_lbl,
         nar_prim, pub_nar_prim, diff_nar_prim,
         nar_sec,  pub_nar_sec,  diff_nar_sec,
         gar_prim, pub_gar_prim, diff_gar_prim,
         gar_sec,  pub_gar_sec,  diff_gar_sec)


# Visualize ==================================================================
every_nth <- function(n) function(x) x[c(TRUE, rep(FALSE, n - 1))]

results |>
  filter(outcome == "nar_prim", threshold == ">0y") |>
  mutate(svyr   = paste0(iso3, year),
         diff   = (prev_outside - prev_inside) * 100,
         signif = pvalue < 0.051) |>
  filter(!is.na(diff)) |>
  ggplot(aes(x = reorder(svyr, diff), y = diff,
             fill = signif, alpha = trustworthy)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(breaks = every_nth(n = 5)) +
  labs(x = "Surveys", y = "Primary NAR pp difference (outzone - inzone)") +
  coord_flip() +
  theme_bw()



# Fin
