# Project: SOWC 2026
# Goal: Merge per-survey child-marriage / conflict-exposure results from the
#       DHS and MICS pipelines into one combined dataset for cross-source
#       analysis and plotting.
# Author: Rouslan Karimov
# Organization: UNICEF
# Created: 17 May 2026


library(here)
library(tidyverse)


# Load per-source results. Both CSVs share the same schema; the upstream
# scripts tag each row with a `source` column ("DHS" or "MICS").
dhs  <- read_csv(here("data_derived", "child_marriage_ipums_dhs_results.csv"),
                 show_col_types = FALSE)
mics <- read_csv(here("data_derived", "child_marriage_ipums_mics_results.csv"),
                 show_col_types = FALSE)

combined <- bind_rows(dhs, mics) |>
  arrange(iso3, year, source)

write_csv(combined, here("data_derived", "child_marriage_combined_results.csv"))


# Visualize ====
every_nth <- function(n) {
  function(x) x[c(TRUE, rep(FALSE, n - 1))]
}

combined |>
  mutate(svyr    = paste0(iso3, year, "-", source),
         cm_diff = (cm_outside - cm_inside) * 100,
         signif  = pvalue < 0.051) |>
  filter(!is.na(cm_diff)) |>
  ggplot(aes(x = reorder(svyr, cm_diff), y = cm_diff,
             fill = signif, color = source)) +
  geom_bar(stat = "identity", linewidth = 0.3) +
  scale_x_discrete(breaks = every_nth(n = 5)) +
  scale_color_manual(values = c(DHS = "black", MICS = "red")) +
  labs(x = "Surveys", y = "Percentage point difference (outzone - inzone)") +
  coord_flip() +
  theme_bw()



# Fin
