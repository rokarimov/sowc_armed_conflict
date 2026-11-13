# Project: SOWC 2026
# Goal: Merge per-survey education (NAR / GAR primary & secondary) results
#       from the DHS and MICS pipelines into one combined dataset for cross-
#       source analysis and plotting.
# Author: Rouslan Karimov
# Organization: UNICEF
# Created: 25 May 2026


library(here)
library(tidyverse)
library(cowplot)


# Load per-source results. Both CSVs share the same schema; upstream scripts
# tag each row with a `source` column ("DHS" or "MICS"), one row per
# (survey × outcome × exposure threshold).
dhs  <- read_csv(here("data_derived", "education_ipums_dhs_results.csv"),
                 show_col_types = FALSE)
mics <- read_csv(here("data_derived", "education_ipums_mics_results.csv"),
                 show_col_types = FALSE)

combined <- bind_rows(dhs, mics) |>
  arrange(iso3, year, source, outcome, threshold)

write_csv(combined, here("data_derived", "education_combined_results.csv"))


# Visualize ==================================================================
combined <- read_csv(here("data_derived", "education_combined_results.csv"),
                     show_col_types = FALSE)

every_nth <- function(n) function(x) x[c(TRUE, rep(FALSE, n - 1))]

MakePanel <- function(outcome_, threshold_, title_) {
  combined |>
    filter(outcome == outcome_, threshold == threshold_) |>
    mutate(svyr   = paste0(iso3, year, "-", source),
           diff   = (prev_outside - prev_inside) * 100,
           signif = pvalue <= 0.05) |>
    filter(!is.na(diff)) |>
    ggplot(aes(x = reorder(svyr, diff), y = diff,
               fill = signif, alpha = trustworthy)) +
    geom_bar(stat = "identity", linewidth = 0.3) +
    scale_x_discrete(breaks = every_nth(n = 5)) +
    labs(x = "Surveys",
         y = "pp difference (outzone - inzone)",
         title = title_) +
    coord_flip() +
    theme_bw()
}

nar_prim_1 <- MakePanel("nar_prim", ">0y", "Primary NAR (conflict 1-2 yrs)")
nar_prim_2 <- MakePanel("nar_prim", ">2y", "Primary NAR (conflict 3+ yrs)")
nar_sec_1  <- MakePanel("nar_sec",  ">0y", "Secondary NAR (conflict 1-2 yrs)")
nar_sec_2  <- MakePanel("nar_sec",  ">2y", "Secondary NAR (conflict 3+ yrs)")
gar_prim_1 <- MakePanel("gar_prim", ">0y", "Primary GAR (conflict 1-2 yrs)")
gar_prim_2 <- MakePanel("gar_prim", ">2y", "Primary GAR (conflict 3+ yrs)")
gar_sec_1  <- MakePanel("gar_sec",  ">0y", "Secondary GAR (conflict 1-2 yrs)")
gar_sec_2  <- MakePanel("gar_sec",  ">2y", "Secondary GAR (conflict 3+ yrs)")

plot_grid(nar_prim_1, nar_prim_2)
plot_grid(nar_sec_1,  nar_sec_2)
plot_grid(gar_prim_1, gar_prim_2)
plot_grid(gar_sec_1,  gar_sec_2)



# Fin
