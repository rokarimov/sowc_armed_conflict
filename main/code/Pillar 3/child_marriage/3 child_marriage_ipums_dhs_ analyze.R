# Project: SOWC 2026
# Goal: Compare child marriage prevalence inside and outside conflict zones
#       using DHS survey data from IPUMS
# Author: Rouslan Karimov
# Organization: UNICEF
# Created: 6 May 2026


library(here)
library(tidyverse)
library(haven)
library(sf)
library(survey)
library(countrycode)
library(ggthemes)

options(survey.lonely.psu = "adjust")

# Conflict-zone buffers (sf, country-year polygons)
buffers <- readRDS(here("data_derived", "ged_country_year_buffers.rds"))

# Load IPUMS DHS extract; drop labelled-vector class so comparisons stay numeric
ipums <- read_dta(here("data", "IPUMS extracts", "DHS", "child_marriage.dta")) |>
  zap_labels()

# Restrict to women 18-49 with usable GPS; build child-marriage flag and ISO3
ipums <- ipums |>
  filter(age >= 18, age <= 49, gpslat != 0, gpslong != 0,
         !is.na(gpslat), !is.na(gpslong)) |>
  mutate(
    iso3 = countrycode(country, origin = "iso3n", destination = "iso3c"),
    cm = case_when(
      marstat == 10                       ~ 0L,   # never married → no CM
      agefrstmar > 0 & agefrstmar < 18    ~ 1L,   # married before 18
      agefrstmar >= 18 & agefrstmar < 95  ~ 0L,   # married at 18+
      .default = NA_integer_
    )
  )

# Per-survey: compute exposure, weighted means, and t-test
AnalyzeChildMarriage <- function(d) {
  this_iso3 <- d$iso3[1]
  this_year <- d$year[1]

  clusters_sf <- d |>
    distinct(clusterno, gpslat, gpslong) |>
    st_as_sf(coords = c("gpslong", "gpslat"), crs = 4326)

  buffers_c <- buffers |> filter(iso3 == this_iso3)
  if (nrow(buffers_c) > 0) {
    hits <- st_intersects(clusters_sf, buffers_c, sparse = FALSE)
    buffer_years <- buffers_c$year
    cluster_idx  <- match(d$clusterno, clusters_sf$clusterno)

    d$exposed <- vapply(seq_len(nrow(d)), function(i) {
      byear <- d$birthyear[i]
      if (is.na(byear)) return(NA)
      any(hits[cluster_idx[i], ] & buffer_years %in% byear:(byear + 17))
    }, logical(1))
  } else {
    d$exposed <- FALSE
  }

  des <- svydesign(id = ~idhspsu, strata = ~idhsstrata,
                   weights = ~perweight,
                   data = d, nest = TRUE)
  des_20_24 <- subset(des, age >= 20 & age <= 24)

  safeRatio <- function(design) {   # Use svyratio, not svymean, to apply AWFACTT properly. See desc of AWFACTT in codebook.
    tryCatch(as.numeric(coef(svyratio(~cm, ~awfactt, design, na.rm = TRUE)))[1],
             error = function(e) NA_real_)
  }

  # Per-zone power diagnostic: DEFF, ρ, effective n. DHS API does not expose
  # DEFF/ρ; we compute manually from cm's observed SE vs. SRS variance,
  # using unweighted n. survey::svymean's built-in deff= argument uses
  # weighted n, which is unstable when DHS weights aren't normalized.
  # ρ is recovered from DEFF ≈ 1 + (m̄ − 1)·ρ.
  zoneDiag <- function(design, sub_d) {
    n  <- nrow(sub_d)
    nc <- n_distinct(sub_d$idhspsu)
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
    source      = "DHS",
    sample_id   = d$sample[1],
    iso3        = this_iso3,
    year        = this_year,
    n           = nrow(d),
    cm_overall  = safeRatio(des_20_24),
    cm_inside   = safeRatio(subset(des,  exposed)),
    cm_outside  = safeRatio(subset(des, !exposed)),
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

write_csv(results, here("data_derived", "child_marriage_ipums_dhs_results.csv"))


# ---- Sanity check vs. DHS-published estimates --------------------------------
# Pull officially published "young women 20-24 first married by exact age 18"
# from the DHS API and align with our `cm_overall` (also computed on the 20-24
# cohort). API: https://api.dhsprogram.com (open JSON; auth is optional but
# DHS asks heavy users to identify themselves via apiKey — stored in .Renviron
# as DHS_API_KEY, read here with Sys.getenv()).

results <- results |>
  mutate(dhs_cc = countrycode(iso3, "iso3c", "dhs"))

dhs_codes <- na.omit(unique(results$dhs_cc))

fetchDhsData <- function(indicator_ids, country_codes) {
  base <- "https://api.dhsprogram.com/rest/dhs/data"
  api_key <- Sys.getenv("DHS_API_KEY")
  qs <- paste0(
    "?indicatorIds=", paste(indicator_ids, collapse = ","),
    "&countryIds=",   paste(country_codes,  collapse = ","),
    "&breakdown=all&perpage=1000&f=json",
    if (nzchar(api_key)) paste0("&apiKey=", api_key) else ""
  )
  pages <- list()
  page <- 1
  repeat {
    resp <- jsonlite::fromJSON(paste0(base, qs, "&page=", page),
                               simplifyDataFrame = TRUE)
    pages[[page]] <- resp$Data
    if (page >= resp$TotalPages) break
    page <- page + 1
  }
  bind_rows(pages)
}

dhs_published <- fetchDhsData("MA_MBAY_W_B18", dhs_codes) |>
  filter(IsPreferred == 1, SurveyType == "DHS",
         CharacteristicCategory == "Total") |>
  transmute(
    dhs_cc       = DHS_CountryCode,
    survey_year  = SurveyYear,
    survey_lbl   = SurveyYearLabel,
    indicator_id = IndicatorId,
    value_pct    = Value,
    n_weighted   = DenominatorWeighted
  )

dhs_wide <- dhs_published |>
  select(dhs_cc, survey_year, survey_lbl, indicator_id, value_pct) |>
  pivot_wider(names_from = indicator_id, values_from = value_pct) |>
  rename(pub_married_by_18 = MA_MBAY_W_B18)

# Match on (country, fieldwork start year). DHS SurveyYear is the start year of
# fieldwork; IPUMS sample year usually matches. Mismatches yield NA — inspect
# `dhs_published` to find the correct survey year manually if needed.
comparison <- results |>
  mutate(cm_overall_pct = round(100 * cm_overall, 1)) |>
  left_join(dhs_wide, by = c("dhs_cc", "year" = "survey_year")) |>
  mutate(diff_vs_pub = round(cm_overall_pct - pub_married_by_18, 1)) |>
  select(iso3, year, survey_lbl, n, cm_overall_pct,
         pub_married_by_18, diff_vs_pub)


# Visualize ====
every_nth <- function(n) {
  function(x) x[c(TRUE, rep(FALSE, n - 1))]
}

results |>
  mutate(svyr = paste0(iso3, year), cm_diff = (cm_outside - cm_inside) * 100, signif = pvalue < 0.051) |>
  filter(!is.na(cm_diff)) |>
  ggplot(aes(x = reorder(svyr, cm_diff), y = cm_diff, fill = signif, alpha = trustworthy)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(breaks = every_nth(n = 5)) +
  labs(x = "Surveys", y = "Percentage point difference (outzone - inzone)") +
  coord_flip() +
  theme_bw()



# Fin
