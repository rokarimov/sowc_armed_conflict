# Project: SOWC 2026
# Goal: Compare child stunting and wasting prevalence inside and outside
#       conflict zones using MICS survey data from IPUMS.
# Author: Rouslan Karimov
# Organization: UNICEF
# Created: 18 May 2026


library(here)
library(tidyverse)
library(haven)
library(sf)
library(survey)

options(survey.lonely.psu = "adjust")


# Load data ====

# Conflict-zone buffers (sf, country-year polygons)
buffers <- readRDS(here("data_derived", "ged_country_year_buffers.rds"))

# IPUMS MICS nutrition extract (joined with HH stratum/PSU and iso3 in step 1).
# Drop labelled-vector class so comparisons stay numeric.
ipums <- readRDS(here("data_derived", "mics_nutrition_for_analysis.rds")) |>
  zap_labels()


# Restrict to children 0-59 months with usable GPS and a valid interview year.
# Anthropometry z-scores from IPUMS MICS are plain (not ×100); the WHO-derived
# validity flag (hazflag/whzflag == 0) marks usable measurements — flag == 1
# is biologically implausible, flag == 9 is missing, and NA means measurement
# was not attempted. agechmo / interviewyr / birthyrch use 99 / 9999 as
# missing sentinels. MICS has no de facto/de jure split (the DHS hv103
# analog), so universe is anchored on a clean flag instead.
ipums <- ipums |>
  filter(agechmo >= 0, agechmo <= 59,
         latitude != 0, longitude != 0,
         !is.na(latitude), !is.na(longitude)) |>
  mutate(
    interviewyr = if_else(interviewyr >= 9990, NA_real_, interviewyr),
    birthyrch   = if_else(birthyrch   >= 9990, NA_real_, birthyrch),
    age_years   = floor(agechmo / 12),
    stunt = case_when(
      hazflag == 0 & hazwho < -2  ~ 1L,
      hazflag == 0 & hazwho >= -2 ~ 0L,
      .default = NA_integer_),
    sev_stunt = case_when(
      hazflag == 0 & hazwho < -3  ~ 1L,
      hazflag == 0 & hazwho >= -3 ~ 0L,
      .default = NA_integer_),
    wast = case_when(
      whzflag == 0 & whzwho < -2  ~ 1L,
      whzflag == 0 & whzwho >= -2 ~ 0L,
      .default = NA_integer_),
    sev_wast = case_when(
      whzflag == 0 & whzwho < -3  ~ 1L,
      whzflag == 0 & whzwho >= -3 ~ 0L,
      .default = NA_integer_)
  ) |>
  filter(!is.na(interviewyr))


# Per-zone power diagnostic — same logic as the DHS nutrition pipeline.
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

# Row-index the survey design directly — equivalent to subset() but avoids
# NSE plumbing.
AnalyzeOne <- function(d, des, outcome, exposed_var, threshold_lbl) {
  keep  <- !is.na(d[[outcome]])
  d_o   <- d[keep, ]
  # Some samples carry no usable z-scores for a given outcome. Skip cleanly
  # so the row reports NA prevalence instead of svymean's empty-design 0.
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

  z_in  <- ZoneDiag(des_in,  d_in,  outcome, "imicspsu")
  z_out <- ZoneDiag(des_out, d_out, outcome, "imicspsu")

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
    outcome      = outcome,
    threshold    = threshold_lbl,
    n            = nrow(d_o),
    prev_overall = safeMean(des_o),
    prev_inside  = safeMean(des_in),
    prev_outside = safeMean(des_out),
    pvalue       = ttest_p,
    n_clust_in   = z_in$nc,  n_clust_out = z_out$nc,
    deff_in      = z_in$deff,  deff_out  = z_out$deff,
    rho_in       = z_in$rho,   rho_out   = z_out$rho,
    n_eff_in     = z_in$n_eff, n_eff_out = z_out$n_eff,
    mde          = mde,
    trustworthy  = trustworthy
  )
}

AnalyzeSurvey <- function(d) {
  this_iso3 <- d$iso3[1]
  this_year <- d$year[1]

  clusters_sf <- d |>
    distinct(cluster, latitude, longitude) |>
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

  buffers_c <- buffers |> filter(iso3 == this_iso3)
  if (nrow(buffers_c) > 0) {
    hits <- st_intersects(clusters_sf, buffers_c, sparse = FALSE)
    buffer_years <- buffers_c$year
    cluster_idx  <- match(d$cluster, clusters_sf$cluster)

    d$conflict_exposure <- vapply(seq_len(nrow(d)), function(i) {
      iyr <- d$interviewyr[i]; age <- d$age_years[i]
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
                   weights = ~weightch, data = d, nest = TRUE)

  grid <- expand_grid(
    outcome  = c("stunt", "sev_stunt", "wast", "sev_wast"),
    exposure = c(exposed = "exposed", exposed3 = "exposed3", exposed5 = "exposed5")
  )

  pmap(grid, function(outcome, exposure) {
    AnalyzeOne(d, des, outcome, exposure,
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

write_csv(results, here("data_derived", "nutrition_ipums_mics_results.csv"))


# Visualize =================================================================
every_nth <- function(n) function(x) x[c(TRUE, rep(FALSE, n - 1))]

results |>
  filter(outcome == "stunt", threshold == ">0y") |>
  mutate(svyr = paste0(iso3, year),
         diff = (prev_outside - prev_inside) * 100,
         signif = pvalue < 0.051) |>
  filter(!is.na(diff)) |>
  ggplot(aes(x = reorder(svyr, diff), y = diff, fill = signif)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(breaks = every_nth(n = 5)) +
  labs(x = "Surveys", y = "Stunting pp difference (outzone - inzone)") +
  coord_flip() +
  theme_bw()



# Fin
