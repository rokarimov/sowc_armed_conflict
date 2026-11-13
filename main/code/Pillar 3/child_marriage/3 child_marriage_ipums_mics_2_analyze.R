# Project: SOWC 2026
# Goal: Compare child marriage prevalence inside and outside conflict zones
#       using MICS survey data from IPUMS
# Author: Rouslan Karimov
# Organization: UNICEF
# Created: 11 May 2026


library(here)
library(tidyverse)
library(haven)
library(sf)
library(survey)
library(ggthemes)

options(survey.lonely.psu = "adjust")


# Load data ====

# Conflict-zone buffers (sf, country-year polygons)
buffers <- readRDS(here("data_derived", "ged_country_year_buffers.rds"))

# IPUMS MICS extract (joined with HH stratum/PSU and iso3 in step 1).
# Drop labelled-vector class so comparisons stay numeric.
ipums <- readRDS(here("data_derived", "mics_child_marriage_for_analysis.rds")) |>
  zap_labels()


# Restrict to women 18-49 with usable GPS; build child-marriage flag.
# birthyrwm carries 9996-9999 as missing/unknown sentinels — convert to NA.
ipums <- ipums |>
  filter(agewm >= 18, agewm <= 49, latitude != 0, longitude != 0,
         !is.na(latitude), !is.na(longitude)) |>
  mutate(
    birthyrwm = if_else(birthyrwm >= 9990, NA_real_, birthyrwm),
    cm = case_when(
      marst == 30                             ~ 0L,   # never married → no CM
      agefirstmarr > 0 & agefirstmarr < 18    ~ 1L,   # married before 18
      agefirstmarr >= 18 & agefirstmarr < 95  ~ 0L,   # married at 18+
      .default = NA_integer_
    )
  )



# Per-survey: compute exposure, weighted means, and t-test
AnalyzeChildMarriage <- function(d) {
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

    d$exposed <- vapply(seq_len(nrow(d)), function(i) {
      byear <- d$birthyrwm[i]
      if (is.na(byear)) return(NA)
      any(hits[cluster_idx[i], ] & buffer_years %in% byear:(byear + 17))
    }, logical(1))
  } else {
    d$exposed <- FALSE
  }

  des <- svydesign(id = ~imicspsu, strata = ~imicsstratum,
                   weights = ~weightwm,
                   data = d, nest = TRUE)
  des_20_24 <- subset(des, agewm >= 20 & agewm <= 24)

  # MICS women's survey covers all women 15-49 directly, so no all-women
  # rescaling (DHS AWFACTT) is needed — use svymean on the proportion.
  safeMean <- function(design) {
    tryCatch(as.numeric(coef(svymean(~cm, design, na.rm = TRUE)))[1],
             error = function(e) NA_real_)
  }

  # Per-zone power diagnostic: DEFF, ρ, effective n. Computed manually from
  # cm's observed SE vs. SRS variance, using unweighted n. survey::svymean's
  # built-in deff= argument uses weighted n, which is unstable when weights
  # aren't normalized. ρ is recovered from DEFF ≈ 1 + (m̄ − 1)·ρ.
  zoneDiag <- function(design, sub_d) {
    n  <- nrow(sub_d)
    nc <- n_distinct(sub_d$imicspsu)
    if (n < 2 || nc < 2) return(list(n = n, nc = nc, m_bar = NA_real_,
                                     deff = NA_real_, rho = NA_real_,
                                     n_eff = NA_real_))
    m  <- tryCatch(svymean(~cm, design, na.rm = TRUE), error = function(e) NULL)
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

  z_in  <- zoneDiag(subset(des,  exposed), filter(d,  exposed))
  z_out <- zoneDiag(subset(des, !exposed), filter(d, !exposed))

  # MDE for difference in proportions, two-sided, α=0.05, 80% power.
  # Uses harmonic-mean effective n; pooled p_bar from raw cm.
  p_bar <- mean(d$cm, na.rm = TRUE)
  mde <- if (!is.na(z_in$n_eff) && !is.na(z_out$n_eff) &&
             z_in$n_eff > 0 && z_out$n_eff > 0) {
    n_eff_h <- 2 / (1 / z_in$n_eff + 1 / z_out$n_eff)
    (qnorm(0.975) + qnorm(0.80)) * sqrt(2 * p_bar * (1 - p_bar) / n_eff_h)
  } else {
    NA_real_
  }

  # Trust flag: ≥25 clusters per arm (cluster-robust inference behaves),
  # ≥30 effective obs per arm, MDE ≤ 10pp.
  trustworthy <- !is.na(mde) &
                 z_in$nc    >= 25 & z_out$nc    >= 25 &
                 z_in$n_eff >= 30 & z_out$n_eff >= 30 &
                 mde <= 0.10

  tibble(
    source      = "MICS",
    sample_id   = d$sample[1],
    iso3        = this_iso3,
    year        = this_year,
    n           = nrow(d),
    cm_overall  = safeMean(des_20_24),
    cm_inside   = safeMean(subset(des,  exposed)),
    cm_outside  = safeMean(subset(des, !exposed)),
    pvalue      = tryCatch(as.numeric(svyttest(cm ~ exposed, des)$p.value),
                           error = function(e) NA_real_),
    n_clust_in  = z_in$nc,
    n_clust_out = z_out$nc,
    deff_in     = z_in$deff,   deff_out  = z_out$deff,
    rho_in      = z_in$rho,    rho_out   = z_out$rho,
    n_eff_in    = z_in$n_eff,  n_eff_out = z_out$n_eff,
    mde         = mde,
    trustworthy = trustworthy
  )
}

results <- ipums |>
  group_split(sample) |>
  map(AnalyzeChildMarriage) |>
  list_rbind() |>
  arrange(iso3, year)

write_csv(results, here("data_derived", "child_marriage_ipums_mics_results.csv"))


# Visualize ====
every_nth <- function(n) {
  function(x) x[c(TRUE, rep(FALSE, n - 1))]
}

results |>
  mutate(svyr = paste0(iso3, year), cm_diff = (cm_outside - cm_inside) * 100, signif = pvalue < 0.051) |>
  filter(!is.na(cm_diff)) |>
  ggplot(aes(x = reorder(svyr, cm_diff), y = cm_diff, fill = signif)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(breaks = every_nth(n = 5)) +
  labs(x = "Surveys", y = "Percentage point difference (outzone - inzone)") +
  coord_flip() +
  theme_bw()



# Fin
