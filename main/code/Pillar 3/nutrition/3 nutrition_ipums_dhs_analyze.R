# Project: SOWC 2026
# Goal: Compare child stunting and wasting prevalence inside and outside
#       conflict zones using DHS survey data from IPUMS.
# Author: Rouslan Karimov
# Organization: UNICEF
# Created: 18 May 2026


library(here)
library(tidyverse)
library(haven)
library(sf)
library(survey)
library(countrycode)
library(furrr)

options(survey.lonely.psu = "adjust")

# Conflict-zone buffers (sf, country-year polygons)
buffers <- readRDS(here("data_derived", "ged_country_year_buffers.rds"))

# Load IPUMS DHS nutrition extract; drop labelled-vector class so comparisons
# stay numeric.
ipums <- read_dta(here("data", "IPUMS extracts", "DHS", "nutrition.dta")) |>
  zap_labels()

# Restrict to de facto-resident children 0-59 months with usable GPS and a
# valid interview year. This matches the denominator the DHS Indicators
# reports use. Z-scores are on the source DHS ×100 scale (sentinels
# 9995-9999); thresholds are -200 SD (moderate) and -300 SD (severe).
ipums <- ipums |>
  filter(hhslept == 1,
         agemohhlt5 >= 0, agemohhlt5 <= 59,
         gpslat != 0, gpslong != 0,
         !is.na(gpslat), !is.na(gpslong),
         !is.na(hhintyr),
         !is.na(hhweight), hhweight > 0) |>
  mutate(
    iso3      = countrycode(country, origin = "iso3n", destination = "iso3c"),
    age_years = floor(agemohhlt5 / 12),
    stunt = case_when(
      hwchazwho < -200                       ~ 1L,
      hwchazwho >= -200 & hwchazwho < 9990   ~ 0L,
      .default = NA_integer_),
    sev_stunt = case_when(
      hwchazwho < -300                       ~ 1L,
      hwchazwho >= -300 & hwchazwho < 9990   ~ 0L,
      .default = NA_integer_),
    wast = case_when(
      hwcwhzwho < -200                       ~ 1L,
      hwcwhzwho >= -200 & hwcwhzwho < 9990   ~ 0L,
      .default = NA_integer_),
    sev_wast = case_when(
      hwcwhzwho < -300                       ~ 1L,
      hwcwhzwho >= -300 & hwcwhzwho < 9990   ~ 0L,
      .default = NA_integer_),
    wgt = hhweight / 1e6
  )


# Per-zone power diagnostic: DEFF, ρ, effective n. DHS API does not expose
# DEFF/ρ; computed manually from the observed SE vs. SRS variance using the
# unweighted n. survey::svymean's built-in deff= argument uses weighted n,
# which is unstable when DHS weights aren't normalized. ρ is recovered from
# DEFF ≈ 1 + (m̄ − 1)·ρ.
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

# Per-survey, per-outcome, per-exposure-threshold result row. Row-index the
# survey design directly — equivalent to subset() but avoids NSE plumbing.
AnalyzeOne <- function(d, des, outcome, exposed_var, threshold_lbl) {
  keep  <- !is.na(d[[outcome]])
  d_o   <- d[keep, ]
  # Some IPUMS DHS samples (e.g. BEN 2001, JOR 2017, RWA 2008) carry no WHO
  # z-scores at all; MDG 2008 has HAZ but no WHZ. Skip these so the
  # row reports NA prevalence instead of svymean's empty-design 0.
  if (nrow(d_o) == 0) {
    return(tibble(
      outcome      = outcome, threshold = threshold_lbl,
      n            = 0L,
      prev_overall = NA_real_, prev_inside = NA_real_, prev_outside = NA_real_,
      pvalue       = NA_real_,
      n_clust_in   = NA_integer_, n_clust_out = NA_integer_,
      deff_in      = NA_real_,    deff_out    = NA_real_,
      rho_in       = NA_real_,    rho_out     = NA_real_,
      n_eff_in     = NA_real_,    n_eff_out   = NA_real_,
      mde          = NA_real_,
      trustworthy  = NA
    ))
  }
  des_o <- des[keep, ]

  ex      <- d_o[[exposed_var]] %in% TRUE
  des_in  <- des_o[ ex, ]
  des_out <- des_o[!ex, ]
  d_in    <- d_o[ ex, ]
  d_out   <- d_o[!ex, ]

  safeMean <- function(design) {
    tryCatch(as.numeric(coef(svymean(as.formula(paste0("~", outcome)),
                                     design, na.rm = TRUE)))[1],
             error = function(e) NA_real_)
  }

  z_in  <- ZoneDiag(des_in,  d_in,  outcome, "idhspsu")
  z_out <- ZoneDiag(des_out, d_out, outcome, "idhspsu")

  p_bar <- mean(d_o[[outcome]], na.rm = TRUE)
  mde <- if (!is.na(z_in$n_eff) && !is.na(z_out$n_eff) &&
             z_in$n_eff > 0 && z_out$n_eff > 0) {
    n_eff_h <- 2 / (1 / z_in$n_eff + 1 / z_out$n_eff)
    (qnorm(0.975) + qnorm(0.80)) * sqrt(2 * p_bar * (1 - p_bar) / n_eff_h)
  } else NA_real_

  trustworthy <- !is.na(mde) &
                 z_in$nc    >= 25 & z_out$nc    >= 25 &
                 z_in$n_eff >= 30 & z_out$n_eff >= 30 &
                 mde <= 0.10

  ttest_p <- tryCatch(
    as.numeric(svyttest(as.formula(paste0(outcome, " ~ ", exposed_var)),
                        des_o)$p.value),
    error = function(e) NA_real_)

  tibble(
    outcome     = outcome,
    threshold   = threshold_lbl,
    n           = nrow(d_o),
    prev_overall = safeMean(des_o),
    prev_inside  = safeMean(des_in),
    prev_outside = safeMean(des_out),
    pvalue      = ttest_p,
    n_clust_in  = z_in$nc,  n_clust_out = z_out$nc,
    deff_in     = z_in$deff,  deff_out  = z_out$deff,
    rho_in      = z_in$rho,   rho_out   = z_out$rho,
    n_eff_in    = z_in$n_eff, n_eff_out = z_out$n_eff,
    mde         = mde,
    trustworthy = trustworthy
  )
}

# Per-survey: build cluster exposure matrix, compute three binary exposure
# flags (any year of life in a 50 km conflict buffer, >2 years, >4 years),
# then run AnalyzeOne for each (outcome × threshold) cell.
AnalyzeSurvey <- function(d) {
  this_iso3 <- d$iso3[1]
  this_year <- d$year[1]

  clusters_sf <- d |>
    distinct(clusternoall, gpslat, gpslong) |>
    st_as_sf(coords = c("gpslong", "gpslat"), crs = 4326)

  buffers_c <- buffers |> filter(iso3 == this_iso3)
  if (nrow(buffers_c) > 0) {
    hits <- st_intersects(clusters_sf, buffers_c, sparse = FALSE)
    buffer_years <- buffers_c$year
    cluster_idx  <- match(d$clusternoall, clusters_sf$clusternoall)

    d$conflict_exposure <- vapply(seq_len(nrow(d)), function(i) {
      iyr <- d$hhintyr[i]; age <- d$age_years[i]
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
    outcome   = c("stunt", "sev_stunt", "wast", "sev_wast"),
    exposure  = c(exposed = "exposed", exposed3 = "exposed3", exposed5 = "exposed5")
  )

  pmap(grid, function(outcome, exposure) {
    AnalyzeOne(d, des, outcome, exposure,
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

write_csv(results, here("data_derived", "nutrition_ipums_dhs_results.csv"))


# Sanity check vs. DHS-published stunting/wasting ============================
# CN_NUTS_C_HA2 = % children stunted (-2 SD), CN_NUTS_C_WH2 = % wasted (-2 SD).
# Cohort is the same children under 5; match on (country, fieldwork start year).
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

dhs_published <- FetchDhsData(c("CN_NUTS_C_HA2", "CN_NUTS_C_WH2"), dhs_codes) |>
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
  rename(pub_stunt = CN_NUTS_C_HA2, pub_wast = CN_NUTS_C_WH2)

comparison <- results |>
  filter(threshold == ">0y", outcome %in% c("stunt", "wast")) |>
  pivot_wider(id_cols = c(iso3, year, dhs_cc),
              names_from = outcome, values_from = prev_overall) |>
  left_join(dhs_wide, by = c("dhs_cc", "year" = "survey_year")) |>
  mutate(stunt_pct = round(100 * stunt, 1),
         wast_pct  = round(100 * wast,  1),
         diff_stunt = round(stunt_pct - pub_stunt, 1),
         diff_wast  = round(wast_pct  - pub_wast,  1)) |>
  select(iso3, year, survey_lbl,
         stunt_pct, pub_stunt, diff_stunt,
         wast_pct,  pub_wast,  diff_wast)


# Visualize =================================================================
every_nth <- function(n) function(x) x[c(TRUE, rep(FALSE, n - 1))]

results |>
  filter(outcome == "stunt", threshold == ">0y") |>
  mutate(svyr = paste0(iso3, year),
         diff = (prev_outside - prev_inside) * 100,
         signif = pvalue < 0.051) |>
  filter(!is.na(diff)) |>
  ggplot(aes(x = reorder(svyr, diff), y = diff,
             fill = signif, alpha = trustworthy)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(breaks = every_nth(n = 5)) +
  labs(x = "Surveys", y = "Stunting pp difference (outzone - inzone)") +
  coord_flip() +
  theme_bw()



# Fin
