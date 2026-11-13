# Project: SOWC 2026
# Goal: Merge per-survey nutrition (stunting / wasting) results from the DHS
#       and MICS pipelines into one combined dataset for cross-source
#       analysis and plotting.
# Author: Rouslan Karimov
# Organization: UNICEF
# Created: 18 May 2026


library(here)
library(tidyverse)
library(cowplot)


# Load per-source results. Both CSVs share the same schema; the upstream
# scripts tag each row with a `source` column ("DHS" or "MICS") and one row
# per (survey × outcome × exposure threshold).
dhs  <- read_csv(here("data_derived", "nutrition_ipums_dhs_results.csv"),
                 show_col_types = FALSE)
mics <- read_csv(here("data_derived", "nutrition_ipums_mics_results.csv"),
                 show_col_types = FALSE)

combined <- bind_rows(dhs, mics) |>
  arrange(iso3, year, source, outcome, threshold)

#write_csv(combined, here("data_derived", "nutrition_combined_results.csv"))


# Visualize =================================================================

combined <- read_csv(here("data_derived", "nutrition_combined_results.csv"))

# Default panel: moderate stunting at the lowest exposure threshold (>0y).
every_nth <- function(n) function(x) x[c(TRUE, rep(FALSE, n - 1))]

# Version showing not-significant and not-trustworthy bars, to restore if necessary
# stunt1 <- combined |>
#   filter(outcome == "stunt", threshold == ">0y") |>
#   mutate(svyr   = paste0(iso3, year, "-", source),
#          diff   = (prev_outside - prev_inside) * 100,
#          signif = pvalue <= 0.05) |>
#   filter(!is.na(diff)) |>
#   ggplot(aes(x = reorder(svyr, diff), y = diff,
#              fill = signif, alpha = trustworthy)) +
#   geom_bar(stat = "identity", linewidth = 0.3) +
#   scale_x_discrete(breaks = every_nth(n = 5)) +
#   guides(signif = "none") +
#   labs(x = "Surveys", y = "Stunting pp difference (outzone - inzone)",
#        title = "Stunting (conflict exposure 1-4 years)") +
#   coord_flip() +
#   theme_bw()

stunt1 <- combined |>
  filter(outcome == "stunt", threshold == ">0y") |>
  mutate(svyr   = paste0(iso3, year, "-", source),
         diff   = (prev_outside - prev_inside) * 100,
         signif = pvalue <= 0.05) |>
  filter(!is.na(diff)) |>
  filter(trustworthy == TRUE, signif == TRUE) |>
  ggplot(aes(x = reorder(svyr, diff), y = diff)) +
  geom_bar(stat = "identity", linewidth = 0.3, fill = "#00AEEF") +
#  scale_x_discrete(breaks = every_nth(n = 2)) +
  labs(x = "Surveys", y = "Stunting pp difference (outzone - inzone)",
       title = "Stunting (conflict exposure 1-4 years)") +
  coord_flip() +
  theme_bw()


stunt2 <- combined |>
  filter(outcome == "stunt", threshold == ">2y") |>
  mutate(svyr   = paste0(iso3, year, "-", source),
         diff   = (prev_outside - prev_inside) * 100,
         signif = pvalue <= 0.05) |>
  filter(!is.na(diff)) |>
  filter(trustworthy == TRUE, signif == TRUE) |>
  ggplot(aes(x = reorder(svyr, diff), y = diff)) +
  geom_bar(stat = "identity", linewidth = 0.3, fill = "#00AEEF") +
#  scale_x_discrete(breaks = every_nth(n = 2)) +
  labs(x = "Surveys", y = "Stunting pp difference (outzone - inzone)",
       title = "Stunting (conflict exposure 3-4 years)") +
  coord_flip() +
  theme_bw()

waste1 <- combined |>
  filter(outcome == "wast", threshold == ">0y") |>
  mutate(svyr   = paste0(iso3, year, "-", source),
         diff   = (prev_outside - prev_inside) * 100,
         signif = pvalue <= 0.05) |>
  filter(!is.na(diff)) |>
  filter(trustworthy == TRUE, signif == TRUE) |>
  ggplot(aes(x = reorder(svyr, diff), y = diff)) +
  geom_bar(stat = "identity", linewidth = 0.3, fill = "#961A49") +
#  scale_x_discrete(breaks = every_nth(n = 2)) +
  labs(x = "Surveys", y = "Wasting pp difference (outzone - inzone)",
       title = "Wasting (conflict exposure 1-4 years)") +
  coord_flip() +
  theme_bw()

waste2 <- combined |>
  filter(outcome == "wast", threshold == ">2y") |>
  mutate(svyr   = paste0(iso3, year, "-", source),
         diff   = (prev_outside - prev_inside) * 100,
         signif = pvalue <= 0.05) |>
  filter(!is.na(diff)) |>
  filter(trustworthy == TRUE, signif == TRUE) |>
  ggplot(aes(x = reorder(svyr, diff), y = diff)) +
  geom_bar(stat = "identity", linewidth = 0.3, fill = "#961A49") +
#  scale_x_discrete(breaks = every_nth(n = 2)) +
  labs(x = "Surveys", y = "Wasting pp difference (outzone - inzone)",
       title = "Wasting (conflict exposure 3-4 years)") +
  coord_flip() +
  theme_bw()

plot_grid(stunt1, stunt2)
plot_grid(waste1, waste2)


# Fin
