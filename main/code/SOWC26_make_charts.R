# Project: SOWC 2026
# Goal: Create data and example chart file for designer
# Author: Rouslan Karimov
# Organization: UNICEF
# Created: 5 November 2026
################################################################################
# Load libraries
library(here)
library(tidyverse)
library(scales)
library(readxl)
library(openxlsx)
library(ggthemes)
library(ggrepel)
library(showtext)  # For NotoSans
font_add_google("Noto Sans")
showtext_auto()


# UNICEF official color palette
unicefcol <- c('#00AEEF', '#9ADBE8', '#0047BB', '#004C45', '#FFB500', '#FF8200', '#961A49', '#E2231A')

# UNICEF official country names
area <- read_xlsx(here("data", "areas.xlsx"))

# Load data
datafile <- here("outputs", "CONFIDENTIAL MRM Dataset.xlsx")

################################################################################
# TRENDS OVER TIME (TOTAL AND BY SEX) ====

# Figure 1.1 ----
out1.1 <- read_xlsx(datafile, sheet = "1.1 Total GV 2005-2025", range = "B2:C22")

df1.1 <- out1.1 |> 
  rename(year = 1, totalgv = 2)

p1.1 <- df1.1 |> 
  ggplot(aes(x = year, y = totalgv)) + 
  geom_line(linewidth = 1, colour = unicefcol[8]) + 
  geom_text(
    aes(x = year, y = totalgv,
        label = case_when(year %in% c(2006, 2015, 2025) ~ totalgv,
                          .default = NA)
    )) +
  scale_x_continuous(breaks = breaks_width(1)) +
  scale_y_continuous(breaks = breaks_width(5000)) + 
  ylab("Total violations") +
  theme_hc(base_family = "Noto Sans") + 
  theme(axis.title.x = element_blank())

print(p1.1)

# Figure 1.2 ----
out1.2 <- read_xlsx(datafile, sheet = "3.2 K and M 2005-2025", range = "D33:H53")

df1.2 <- out1.2 |> 
  pivot_longer(cols = 2:5, names_to = "group", values_to = "value") |> 
  rename(year = 1)

p1.2 <- df1.2 |> 
  ggplot(aes(x = year)) +
  geom_line(data = df1.2 |> filter(group == "Total KM"),
            aes(y = value),
            linewidth = 1,
            colour = unicefcol[8]) + 
  geom_point(
    data = df1.2 |> filter(group == "Total KM",
                           year %in% c(2006, 2009, 2014, 2016, 2020, 2025)),
    aes(y = value),
    size = 3,
    colour = unicefcol[8]
  ) +
  geom_area(data = df1.2 |> filter(group != "Total KM"),
            aes(y = value, fill = group), alpha = 0.3) + 
  # Labels for totals top line
  geom_text(
    data = df1.2 |> filter(group == "Total KM"),
    aes(y = value,
        label = case_when(year %in% c(2006, 2009, 2014, 2016, 2020, 2025) ~ value,
                          .default = NA)),
    vjust = -1,
    colour = unicefcol[8]) +
  # Labels for stacked areas, located at band midpoint
  geom_text_repel(
    data = df1.2 |> filter(group != "Total KM"),
    aes(y = value, group = group,
        label = case_when(year %in% c(2014, 2016, 2020, 2025) ~ value,
                          .default = NA),
        colour = group),
    position = position_stack(vjust = 0.5),
    point.size = NA,
    direction = "y",
    min.segment.length = Inf,
    seed = 1) +
  scale_fill_discrete(palette = unicefcol[c(2, 4, 6)]) +
  scale_colour_discrete(
    palette = c("darkblue", "darkgreen", "darkred"),
    guide = "none"
  ) +
  scale_x_continuous(breaks = breaks_width(1)) +
  scale_y_continuous(
    limits = c(0, 15000),
    breaks = breaks_width(2500)
  ) + 
  labs(fill = "Sex:", y = "Killing and Maiming Cases") +
  theme_hc(base_family = "Noto Sans") + 
  theme(axis.title.x = element_blank())

print(p1.2)


# Figure 2.1 ----
out2.1 <- read_xlsx(datafile, sheet = "4.2 RU 2005-2025 sex disaggr.", range = "E3:I23")

df2.1 <- out2.1 |> 
  pivot_longer(cols = 2:5, names_to = "group", values_to = "value") |> 
  rename(year = 1)

# Years to label: first, turning points and last
yrs_tot2.1   <- c(2006, 2007, 2010, 2017, 2021, 2025)  # total line
yrs_stack2.1 <- c(2010, 2017, 2021, 2025)              # bands by sex

p2.1 <- df2.1 |> 
  ggplot(aes(x = year)) +
  geom_line(data = df2.1 |> filter(str_detect(group, "Total")),
            aes(y = value),
            linewidth = 1,
            colour = unicefcol[8]) + 
  geom_point(
    data = df2.1 |> filter(str_detect(group, "Total"),
                           year %in% yrs_tot2.1),
    aes(y = value),
    size = 3,
    colour = unicefcol[8]
  ) +
  geom_area(data = df2.1 |> filter(!str_detect(group, "Total")),
            aes(y = value, fill = group), alpha = 0.3) + 
  # Labels for totals top line
  geom_text(
    data = df2.1 |> filter(str_detect(group, "Total")),
    aes(y = value,
        label = case_when(year %in% yrs_tot2.1 ~ value,
                          .default = NA)),
    vjust = -1,
    colour = unicefcol[8]) +
  # Labels for stacked areas, located at band midpoint
  geom_text_repel(
    data = df2.1 |> filter(!str_detect(group, "Total")),
    aes(y = value, group = group,
        label = case_when(year %in% yrs_stack2.1 ~ value,
                          .default = NA),
        colour = group),
    position = position_stack(vjust = 0.5),
    point.size = NA,
    direction = "y",
    min.segment.length = Inf,
    seed = 1) +
  scale_fill_discrete(palette = unicefcol[c(2, 4, 6)]) +
  scale_colour_discrete(
    palette = c("darkblue", "darkgreen", "darkred"),
    guide = "none"
  ) +
  scale_x_continuous(breaks = breaks_width(1)) +
  scale_y_continuous(
    limits = c(0, 12500),
    breaks = breaks_width(2500)
  ) + 
  labs(fill = "Sex:", y = "Recruitment and Use Cases") +
  theme_hc(base_family = "Noto Sans") + 
  theme(axis.title.x = element_blank())

print(p2.1)


# Figure 3.1 ----
out3.1 <- read_xlsx(datafile, sheet = "5.2 SV 2005-2025 sex disaggr.", range = "E5:I25")

df3.1 <- out3.1 |> 
  pivot_longer(cols = 2:5, names_to = "group", values_to = "value") |> 
  rename(year = 1)

# Years to label: first, turning points and last
yrs_tot3.1   <- c(2006, 2008, 2013, 2020, 2024, 2025)  # total line
yrs_stack3.1 <- c(2013, 2020, 2024, 2025)              # bands by sex

p3.1 <- df3.1 |> 
  ggplot(aes(x = year)) +
  geom_line(data = df3.1 |> filter(str_detect(group, "Total")),
            aes(y = value),
            linewidth = 1,
            colour = unicefcol[8]) + 
  geom_point(
    data = df3.1 |> filter(str_detect(group, "Total"),
                           year %in% yrs_tot3.1),
    aes(y = value),
    size = 3,
    colour = unicefcol[8]
  ) +
  geom_area(data = df3.1 |> filter(!str_detect(group, "Total")),
            aes(y = value, fill = group), alpha = 0.3) + 
  # Labels for totals top line
  geom_text(
    data = df3.1 |> filter(str_detect(group, "Total")),
    aes(y = value,
        label = case_when(year %in% yrs_tot3.1 ~ value,
                          .default = NA)),
    vjust = -1,
    colour = unicefcol[8]) +
  # Labels for stacked areas, located at band midpoint
  geom_text_repel(
    data = df3.1 |> filter(!str_detect(group, "Total")),
    aes(y = value, group = group,
        label = case_when(year %in% yrs_stack3.1 ~ value,
                          .default = NA),
        colour = group),
    position = position_stack(vjust = 0.5),
    point.size = NA,
    direction = "y",
    min.segment.length = Inf,
    seed = 1) +
  scale_fill_discrete(palette = unicefcol[c(2, 4, 6)]) +
  scale_colour_discrete(
    palette = c("darkblue", "darkgreen", "darkred"),
    guide = "none"
  ) +
  scale_x_continuous(breaks = breaks_width(1)) +
  scale_y_continuous(
    limits = c(0, 4500),
    breaks = breaks_width(1000)
  ) + 
  labs(fill = "Sex:", y = "Sexual Violence Cases") +
  theme_hc(base_family = "Noto Sans") + 
  theme(axis.title.x = element_blank())

print(p3.1)


# Figure 4.1 ----
out4.1 <- read_xlsx(datafile, sheet = "6.2 ABD 2005-2025 sex disaggr.", range = "E5:I25")

df4.1 <- out4.1 |> 
  pivot_longer(cols = 2:5, names_to = "group", values_to = "value") |> 
  rename(year = 1)

# Years to label: first, turning points and last
yrs_tot4.1   <- c(2006, 2007, 2014, 2016, 2020, 2025)  # total line
yrs_stack4.1 <- c(2014, 2016, 2020, 2025)              # bands by sex

p4.1 <- df4.1 |> 
  ggplot(aes(x = year)) +
  geom_line(data = df4.1 |> filter(str_detect(group, "Total")),
            aes(y = value),
            linewidth = 1,
            colour = unicefcol[8]) + 
  geom_point(
    data = df4.1 |> filter(str_detect(group, "Total"),
                           year %in% yrs_tot4.1),
    aes(y = value),
    size = 3,
    colour = unicefcol[8]
  ) +
  geom_area(data = df4.1 |> filter(!str_detect(group, "Total")),
            aes(y = value, fill = group), alpha = 0.3) + 
  # Labels for totals top line
  geom_text(
    data = df4.1 |> filter(str_detect(group, "Total")),
    aes(y = value,
        label = case_when(year %in% yrs_tot4.1 ~ value,
                          .default = NA)),
    vjust = -1,
    colour = unicefcol[8]) +
  # Labels for stacked areas, located at band midpoint
  geom_text_repel(
    data = df4.1 |> filter(!str_detect(group, "Total")),
    aes(y = value, group = group,
        label = case_when(year %in% yrs_stack4.1 ~ value,
                          .default = NA),
        colour = group),
    position = position_stack(vjust = 0.5),
    point.size = NA,
    direction = "y",
    min.segment.length = Inf,
    seed = 1) +
  scale_fill_discrete(palette = unicefcol[c(2, 4, 6)]) +
  scale_colour_discrete(
    palette = c("darkblue", "darkgreen", "darkred"),
    guide = "none"
  ) +
  scale_x_continuous(breaks = breaks_width(1)) +
  scale_y_continuous(
    limits = c(0, 6000),
    breaks = breaks_width(1000)
  ) + 
  labs(fill = "Sex:", y = "Abduction Cases") +
  theme_hc(base_family = "Noto Sans") + 
  theme(axis.title.x = element_blank())

print(p4.1)


################################################################################
# TRENDS BY PERPETRATOR ====

# One colour per perpetrator, same in all charts 
pal_perp <- c("State actors"     = unicefcol[1],
              "Non-State actors" = unicefcol[2],
              "Joint State-NSAG" = unicefcol[3],
              "Unattributed"     = unicefcol[4],
              "Others"           = unicefcol[5],
              "Unspecified"      = unicefcol[6])

# Categories with stronger colour in all charts
strong_perp <- c("State actors", "Non-State actors")

# Figure 1.3 ----
out1.3 <- read_xlsx(datafile, sheet = "3.3 KM 2005-2025 by perpetrator", range = "E3:L23")

df1.3 <- out1.3 |> 
  pivot_longer(cols = 2:8, names_to = "group", values_to = "value") |> 
  rename(year = 1) |> 
  mutate(group = str_trim(group),               # remove trailing spaces ("Unattributed ")
         value = replace_na(value, 0))          # empty cells = 0, so the area is not cut off

# Perpetrator categories, in Excel order
cats1.3 <- df1.3 |> filter(!str_detect(group, "Total")) |> distinct(group) |> pull(group)

# Category to highlight 
top1.3 <- "State actors"
stopifnot(top1.3 %in% cats1.3)                  # stops if the name does not match

df1.3 <- df1.3 |> 
  mutate(highlight = if_else(group %in% strong_perp, "top", "other"),
         group = factor(group, levels = c(cats1.3, setdiff(unique(group), cats1.3))))

# Same colour per perpetrator in all charts
pal1.3 <- pal_perp

# Years to label 
yrs_tot1.3   <- c(2006, 2009, 2014, 2016, 2020, 2025)  # total line
yrs_stack1.3 <- c(2014, 2016, 2020, 2025)              # highlighted band

p1.3 <- df1.3 |> 
  ggplot(aes(x = year)) +
  geom_line(data = df1.3 |> filter(str_detect(group, "Total")),
            aes(y = value),
            linewidth = 1,
            colour = unicefcol[8]) + 
  geom_point(
    data = df1.3 |> filter(str_detect(group, "Total"),
                           year %in% yrs_tot1.3),
    aes(y = value),
    size = 3,
    colour = unicefcol[8]
  ) +
  # Stacked areas in Excel order, bottom to top
  geom_area(data = df1.3 |> filter(!str_detect(group, "Total")),
            aes(y = value, fill = group, alpha = highlight),
            position = position_stack(reverse = TRUE)) + 
  # Labels for totals top line
  geom_text(
    data = df1.3 |> filter(str_detect(group, "Total")),
    aes(y = value,
        label = case_when(year %in% yrs_tot1.3 ~ value,
                          .default = NA)),
    vjust = -1,
    colour = unicefcol[8]) +
  # Labels only for the highlighted band, at band midpoint
  geom_text_repel(
    data = df1.3 |> filter(!str_detect(group, "Total")),
    aes(y = value, group = group,
        label = case_when(group == top1.3 & year %in% yrs_stack1.3 ~ value,
                          .default = NA)),
    colour = "grey10",
    position = position_stack(vjust = 0.5, reverse = TRUE),
    point.size = NA,
    direction = "y",
    min.segment.length = Inf,
    seed = 1) +
  scale_fill_manual(values = pal1.3) +
  scale_alpha_manual(values = c(top = 0.9, other = 0.25), guide = "none") +
  # Legend shows the same transparency as the areas
  guides(fill = guide_legend(
    override.aes = list(alpha = if_else(cats1.3 %in% strong_perp, 0.9, 0.25))
  )) +
  scale_x_continuous(breaks = breaks_width(1)) +
  scale_y_continuous(
    limits = c(0, 15000),
    breaks = breaks_width(2500)
  ) + 
  labs(fill = "Perpetrator:", y = "Killing and Maiming Cases") +
  theme_hc(base_family = "Noto Sans") + 
  theme(axis.title.x = element_blank())

print(p1.3)


# Figure 2.2 ----
out2.2 <- read_xlsx(datafile, sheet = "4.3 RU 2005-2025 by perpetrator", range = "E4:L24")

df2.2 <- out2.2 |> 
  pivot_longer(cols = 2:8, names_to = "group", values_to = "value") |> 
  rename(year = 1) |> 
  mutate(group = str_trim(group),               # remove trailing spaces
         value = replace_na(value, 0))          # empty cells = 0, so the area is not cut off

# Perpetrator categories, in Excel order
cats2.2 <- df2.2 |> filter(!str_detect(group, "Total")) |> distinct(group) |> pull(group)

# Category to highlight 
top2.2 <- "Non-State actors"
stopifnot(top2.2 %in% cats2.2)                  # stops if the name does not match

df2.2 <- df2.2 |> 
  mutate(highlight = if_else(group %in% strong_perp, "top", "other"),
         group = factor(group, levels = c(cats2.2, setdiff(unique(group), cats2.2))))

# Same colour per perpetrator in all charts
pal2.2 <- pal_perp

# Years to label 
yrs_tot2.2   <- c(2006, 2007, 2010, 2017, 2021, 2025)  # total line
yrs_stack2.2 <- c(2010, 2017, 2021, 2025)           # highlighted band

p2.2 <- df2.2 |> 
  ggplot(aes(x = year)) +
  geom_line(data = df2.2 |> filter(str_detect(group, "Total")),
            aes(y = value),
            linewidth = 1,
            colour = unicefcol[8]) + 
  geom_point(
    data = df2.2 |> filter(str_detect(group, "Total"),
                           year %in% yrs_tot2.2),
    aes(y = value),
    size = 3,
    colour = unicefcol[8]
  ) +
  # Stacked areas in Excel order, bottom to top
  geom_area(data = df2.2 |> filter(!str_detect(group, "Total")),
            aes(y = value, fill = group, alpha = highlight),
            position = position_stack(reverse = TRUE)) + 
  # Labels for totals top line
  geom_text(
    data = df2.2 |> filter(str_detect(group, "Total")),
    aes(y = value,
        label = case_when(year %in% yrs_tot2.2 ~ value,
                          .default = NA)),
    vjust = -1,
    colour = unicefcol[8]) +
  # Labels only for the highlighted band, at band midpoint
  geom_text_repel(
    data = df2.2 |> filter(!str_detect(group, "Total")),
    aes(y = value, group = group,
        label = case_when(group == top2.2 & year %in% yrs_stack2.2 ~ value,
                          .default = NA)),
    colour = "grey10",
    position = position_stack(vjust = 0.5, reverse = TRUE),
    point.size = NA,
    direction = "y",
    min.segment.length = Inf,
    seed = 1) +
  scale_fill_manual(values = pal2.2) +
  scale_alpha_manual(values = c(top = 0.9, other = 0.25), guide = "none") +
  # Legend shows the same transparency as the areas
  guides(fill = guide_legend(
    override.aes = list(alpha = if_else(cats2.2 %in% strong_perp, 0.9, 0.25))
  )) +
  scale_x_continuous(breaks = breaks_width(1)) +
  scale_y_continuous(
    limits = c(0, 12500),
    breaks = breaks_width(2500)
  ) + 
  labs(fill = "Perpetrator:", y = "Recruitment and Use Cases") +
  theme_hc(base_family = "Noto Sans") + 
  theme(axis.title.x = element_blank())

print(p2.2)


# Figure 3.2 ----
out3.2 <- read_xlsx(datafile, sheet = "5.3 SV 2005-2025 by perpetrator", range = "E4:L24")

df3.2 <- out3.2 |> 
  pivot_longer(cols = 2:8, names_to = "group", values_to = "value") |> 
  rename(year = 1) |> 
  mutate(group = str_trim(group),               # remove trailing spaces
         value = replace_na(value, 0))          # empty cells = 0, so the area is not cut off

# Perpetrator categories, in Excel order
cats3.2 <- df3.2 |> filter(!str_detect(group, "Total")) |> distinct(group) |> pull(group)

# Category to highlight 
top3.2 <- "Non-State actors"
stopifnot(top3.2 %in% cats3.2)                  

df3.2 <- df3.2 |> 
  mutate(highlight = if_else(group %in% strong_perp, "top", "other"),
         group = factor(group, levels = c(cats3.2, setdiff(unique(group), cats3.2))))

# Same colour per perpetrator in all charts
pal3.2 <- pal_perp

# Years to label 
yrs_tot3.2   <- c(2006, 2008, 2013, 2020, 2024, 2025)  # total line
yrs_stack3.2 <- c(2013, 2020, 2024, 2025)              # highlighted band

p3.2 <- df3.2 |> 
  ggplot(aes(x = year)) +
  geom_line(data = df3.2 |> filter(str_detect(group, "Total")),
            aes(y = value),
            linewidth = 1,
            colour = unicefcol[8]) + 
  geom_point(
    data = df3.2 |> filter(str_detect(group, "Total"),
                           year %in% yrs_tot3.2),
    aes(y = value),
    size = 3,
    colour = unicefcol[8]
  ) +
  # Stacked areas in Excel order, bottom to top
  geom_area(data = df3.2 |> filter(!str_detect(group, "Total")),
            aes(y = value, fill = group, alpha = highlight),
            position = position_stack(reverse = TRUE)) + 
  # Labels for totals top line
  geom_text(
    data = df3.2 |> filter(str_detect(group, "Total")),
    aes(y = value,
        label = case_when(year %in% yrs_tot3.2 ~ value,
                          .default = NA)),
    vjust = -1,
    colour = unicefcol[8]) +
  # Labels only for the highlighted band, at band midpoint
  geom_text_repel(
    data = df3.2 |> filter(!str_detect(group, "Total")),
    aes(y = value, group = group,
        label = case_when(group == top3.2 & year %in% yrs_stack3.2 ~ value,
                          .default = NA)),
    colour = "grey10",
    position = position_stack(vjust = 0.5, reverse = TRUE),
    point.size = NA,
    direction = "y",
    min.segment.length = Inf,
    seed = 1) +
  scale_fill_manual(values = pal3.2) +
  scale_alpha_manual(values = c(top = 0.9, other = 0.25), guide = "none") +
  # Legend shows the same transparency as the areas
  guides(fill = guide_legend(
    override.aes = list(alpha = if_else(cats3.2 %in% strong_perp, 0.9, 0.25))
  )) +
  scale_x_continuous(breaks = breaks_width(1)) +
  scale_y_continuous(
    limits = c(0, 4500),
    breaks = breaks_width(1000)
  ) + 
  labs(fill = "Perpetrator:", y = "Sexual Violence Cases") +
  theme_hc(base_family = "Noto Sans") + 
  theme(axis.title.x = element_blank())

print(p3.2)


# Figure 4.2 ----
out4.2 <- read_xlsx(datafile, sheet = "6.3 ABD 2005-2025 by perpetrato", range = "E4:L24")

df4.2 <- out4.2 |> 
  pivot_longer(cols = 2:8, names_to = "group", values_to = "value") |> 
  rename(year = 1) |> 
  mutate(group = str_trim(group),               # remove trailing spaces
         value = replace_na(value, 0))          # empty cells = 0, so the area is not cut off

# Perpetrator categories, in Excel order
cats4.2 <- df4.2 |> filter(!str_detect(group, "Total")) |> distinct(group) |> pull(group)

# Category to highlight
top4.2 <- "Non-State actors"
stopifnot(top4.2 %in% cats4.2)                  

df4.2 <- df4.2 |> 
  mutate(highlight = if_else(group %in% strong_perp, "top", "other"),
         group = factor(group, levels = c(cats4.2, setdiff(unique(group), cats4.2))))

# Same colour per perpetrator in all charts
pal4.2 <- pal_perp

# Years to label 
yrs_tot4.2   <- c(2006, 2007, 2014, 2016, 2020, 2025)  # total line
yrs_stack4.2 <- c(2014, 2016, 2020, 2025)              # highlighted band

p4.2 <- df4.2 |> 
  ggplot(aes(x = year)) +
  geom_line(data = df4.2 |> filter(str_detect(group, "Total")),
            aes(y = value),
            linewidth = 1,
            colour = unicefcol[8]) + 
  geom_point(
    data = df4.2 |> filter(str_detect(group, "Total"),
                           year %in% yrs_tot4.2),
    aes(y = value),
    size = 3,
    colour = unicefcol[8]
  ) +
  # Stacked areas in Excel order, bottom to top
  geom_area(data = df4.2 |> filter(!str_detect(group, "Total")),
            aes(y = value, fill = group, alpha = highlight),
            position = position_stack(reverse = TRUE)) + 
  # Labels for totals top line
  geom_text(
    data = df4.2 |> filter(str_detect(group, "Total")),
    aes(y = value,
        label = case_when(year %in% yrs_tot4.2 ~ value,
                          .default = NA)),
    vjust = -1,
    colour = unicefcol[8]) +
  # Labels only for the highlighted band, at band midpoint
  geom_text_repel(
    data = df4.2 |> filter(!str_detect(group, "Total")),
    aes(y = value, group = group,
        label = case_when(group == top4.2 & year %in% yrs_stack4.2 ~ value,
                          .default = NA)),
    colour = "grey10",
    position = position_stack(vjust = 0.5, reverse = TRUE),
    point.size = NA,
    direction = "y",
    min.segment.length = Inf,
    seed = 1) +
  scale_fill_manual(values = pal4.2) +
  scale_alpha_manual(values = c(top = 0.9, other = 0.25), guide = "none") +
  # Legend shows the same transparency as the areas
  guides(fill = guide_legend(
    override.aes = list(alpha = if_else(cats4.2 %in% strong_perp, 0.9, 0.25))
  )) +
  scale_x_continuous(breaks = breaks_width(1)) +
  scale_y_continuous(
    limits = c(0, 6000),
    breaks = breaks_width(1000)
  ) + 
  labs(fill = "Perpetrator:", y = "Abduction Cases") +
  theme_hc(base_family = "Noto Sans") + 
  theme(axis.title.x = element_blank())

print(p4.2)


# Figure 5.1 ----
out5.1 <- read_xlsx(datafile, sheet = "7.2 ASH 2005-2025 by perpetrat", range = "E4:L24")

df5.1 <- out5.1 |> 
  pivot_longer(cols = 2:8, names_to = "group", values_to = "value") |> 
  rename(year = 1) |> 
  mutate(group = str_trim(group),               # remove trailing spaces
         value = replace_na(value, 0))          # empty cells = 0, so the area is not cut off

# Perpetrator categories, in Excel order
cats5.1 <- df5.1 |> filter(!str_detect(group, "Total")) |> distinct(group) |> pull(group)

# Category to highlight 
top5.1 <- "State actors"
stopifnot(top5.1 %in% cats5.1)                  

df5.1 <- df5.1 |> 
  mutate(highlight = if_else(group %in% strong_perp, "top", "other"),
         group = factor(group, levels = c(cats5.1, setdiff(unique(group), cats5.1))))

# Same colour per perpetrator in all charts
pal5.1 <- pal_perp

# Years to label: first, turning points and last
yrs_tot5.1   <- c(2006, 2011, 2015, 2020, 2022, 2025)  # total line
yrs_stack5.1 <- c(2015, 2020, 2022, 2025)              # highlighted band

p5.1 <- df5.1 |> 
  ggplot(aes(x = year)) +
  geom_line(data = df5.1 |> filter(str_detect(group, "Total")),
            aes(y = value),
            linewidth = 1,
            colour = unicefcol[8]) + 
  geom_point(
    data = df5.1 |> filter(str_detect(group, "Total"),
                           year %in% yrs_tot5.1),
    aes(y = value),
    size = 3,
    colour = unicefcol[8]
  ) +
  # Stacked areas in Excel order, bottom to top
  geom_area(data = df5.1 |> filter(!str_detect(group, "Total")),
            aes(y = value, fill = group, alpha = highlight),
            position = position_stack(reverse = TRUE)) + 
  # Labels for totals top line
  geom_text(
    data = df5.1 |> filter(str_detect(group, "Total")),
    aes(y = value,
        label = case_when(year %in% yrs_tot5.1 ~ value,
                          .default = NA)),
    vjust = -1,
    colour = unicefcol[8]) +
  # Labels only for the highlighted band, at band midpoint
  geom_text_repel(
    data = df5.1 |> filter(!str_detect(group, "Total")),
    aes(y = value, group = group,
        label = case_when(group == top5.1 & year %in% yrs_stack5.1 ~ value,
                          .default = NA)),
    colour = "grey10",
    position = position_stack(vjust = 0.5, reverse = TRUE),
    point.size = NA,
    direction = "y",
    min.segment.length = Inf,
    seed = 1) +
  scale_fill_manual(values = pal5.1) +
  scale_alpha_manual(values = c(top = 0.9, other = 0.25), guide = "none") +
  # Legend shows the same transparency as the areas
  guides(fill = guide_legend(
    override.aes = list(alpha = if_else(cats5.1 %in% strong_perp, 0.9, 0.25))
  )) +
  scale_x_continuous(breaks = breaks_width(1)) +
  scale_y_continuous(
    limits = c(0, 3000),
    breaks = breaks_width(500)
  ) + 
  labs(fill = "Perpetrator:", y = "Attacks on Schools and Hospitals") +
  theme_hc(base_family = "Noto Sans") + 
  theme(axis.title.x = element_blank())

print(p5.1)


# Figure 6.1 ----
out6.1 <- read_xlsx(datafile, sheet = "8.2 DHA 2005-2025 by perpetrat", range = "E4:L24")

df6.1 <- out6.1 |> 
  pivot_longer(cols = 2:8, names_to = "group", values_to = "value") |> 
  rename(year = 1) |> 
  mutate(group = str_trim(group),               # remove trailing spaces
         value = replace_na(value, 0))          # empty cells = 0, so the area is not cut off

# Perpetrator categories, in Excel order
cats6.1 <- df6.1 |> filter(!str_detect(group, "Total")) |> distinct(group) |> pull(group)

# Category to highlight 
top6.1 <- "State actors"
stopifnot(top6.1 %in% cats6.1)                 

df6.1 <- df6.1 |> 
  mutate(highlight = if_else(group %in% strong_perp, "top", "other"),
         group = factor(group, levels = c(cats6.1, setdiff(unique(group), cats6.1))))

# Same colour per perpetrator in all charts
pal6.1 <- pal_perp

# Years to label: first, turning points and last
yrs_tot6.1   <- c(2006, 2008, 2017, 2019, 2022, 2025)  # total line
yrs_stack6.1 <- c(2017, 2019, 2022, 2025)              # highlighted band

p6.1 <- df6.1 |> 
  ggplot(aes(x = year)) +
  geom_line(data = df6.1 |> filter(str_detect(group, "Total")),
            aes(y = value),
            linewidth = 1,
            colour = unicefcol[8]) + 
  geom_point(
    data = df6.1 |> filter(str_detect(group, "Total"),
                           year %in% yrs_tot6.1),
    aes(y = value),
    size = 3,
    colour = unicefcol[8]
  ) +
  # Stacked areas in Excel order, bottom to top
  geom_area(data = df6.1 |> filter(!str_detect(group, "Total")),
            aes(y = value, fill = group, alpha = highlight),
            position = position_stack(reverse = TRUE)) + 
  # Labels for totals top line
  geom_text(
    data = df6.1 |> filter(str_detect(group, "Total")),
    aes(y = value,
        label = case_when(year %in% yrs_tot6.1 ~ value,
                          .default = NA)),
    vjust = -1,
    colour = unicefcol[8]) +
  # Labels only for the highlighted band, at band midpoint
  geom_text_repel(
    data = df6.1 |> filter(!str_detect(group, "Total")),
    aes(y = value, group = group,
        label = case_when(group == top6.1 & year %in% yrs_stack6.1 ~ value,
                          .default = NA)),
    colour = "grey10",
    position = position_stack(vjust = 0.5, reverse = TRUE),
    point.size = NA,
    direction = "y",
    min.segment.length = Inf,
    seed = 1) +
  scale_fill_manual(values = pal6.1) +
  scale_alpha_manual(values = c(top = 0.9, other = 0.25), guide = "none") +
  # Legend shows the same transparency as the areas
  guides(fill = guide_legend(
    override.aes = list(alpha = if_else(cats6.1 %in% strong_perp, 0.9, 0.25))
  )) +
  scale_x_continuous(breaks = breaks_width(1)) +
  scale_y_continuous(
    limits = c(0, 10000),
    breaks = breaks_width(2500)
  ) + 
  labs(fill = "Perpetrator:", y = "Denial of Humanitarian Access") +
  theme_hc(base_family = "Noto Sans") + 
  theme(axis.title.x = element_blank())

print(p6.1)


# Figure 7.1 ----
out7.1 <- read_xlsx(datafile, sheet = "1.2 GV 2005-2025 Perpetrators", range = "B3:I23")

df7.1 <- out7.1 |> 
  pivot_longer(cols = 2:8, names_to = "group", values_to = "value") |> 
  rename(year = 1) |> 
  mutate(group = str_trim(group),               # remove trailing spaces
         group = if_else(str_detect(group, "TOTAL"), "Total", group),  # "TOTAL 6 VIOLATIONS" -> "Total"
         value = replace_na(value, 0))          # empty cells = 0, so the area is not cut off

# Perpetrator categories, in Excel order
cats7.1 <- df7.1 |> filter(!str_detect(group, "Total")) |> distinct(group) |> pull(group)

# Category to highlight 
top7.1 <- "State actors"
stopifnot(top7.1 %in% cats7.1)                

df7.1 <- df7.1 |> 
  mutate(highlight = if_else(group %in% strong_perp, "top", "other"),
         group = factor(group, levels = c(cats7.1, setdiff(unique(group), cats7.1))))

# Same colour per perpetrator in all charts
pal7.1 <- pal_perp

# Years to label: first, turning points and last
yrs_tot7.1   <- c(2006, 2008, 2010, 2014, 2021, 2025)  # total line
yrs_stack7.1 <- c(2010, 2014, 2021, 2025)              # highlighted band

p7.1 <- df7.1 |> 
  ggplot(aes(x = year)) +
  geom_line(data = df7.1 |> filter(str_detect(group, "Total")),
            aes(y = value),
            linewidth = 1,
            colour = unicefcol[8]) + 
  geom_point(
    data = df7.1 |> filter(str_detect(group, "Total"),
                           year %in% yrs_tot7.1),
    aes(y = value),
    size = 3,
    colour = unicefcol[8]
  ) +
  # Stacked areas in Excel order, bottom to top
  geom_area(data = df7.1 |> filter(!str_detect(group, "Total")),
            aes(y = value, fill = group, alpha = highlight),
            position = position_stack(reverse = TRUE)) + 
  # Labels for totals top line
  geom_text(
    data = df7.1 |> filter(str_detect(group, "Total")),
    aes(y = value,
        label = case_when(year %in% yrs_tot7.1 ~ value,
                          .default = NA)),
    vjust = -1,
    colour = unicefcol[8]) +
  # Labels only for the highlighted band, at band midpoint
  geom_text_repel(
    data = df7.1 |> filter(!str_detect(group, "Total")),
    aes(y = value, group = group,
        label = case_when(group == top7.1 & year %in% yrs_stack7.1 ~ value,
                          .default = NA)),
    colour = "grey10",
    position = position_stack(vjust = 0.5, reverse = TRUE),
    point.size = NA,
    direction = "y",
    min.segment.length = Inf,
    seed = 1) +
  scale_fill_manual(values = pal7.1) +
  scale_alpha_manual(values = c(top = 0.9, other = 0.25), guide = "none") +
  # Legend shows the same transparency as the areas
  guides(fill = guide_legend(
    override.aes = list(alpha = if_else(cats7.1 %in% strong_perp, 0.9, 0.25))
  )) +
  scale_x_continuous(breaks = breaks_width(1)) +
  scale_y_continuous(
    limits = c(0, 50000),
    breaks = breaks_width(10000)
  ) + 
  labs(fill = "Perpetrator:", y = "Total violations") +
  theme_hc(base_family = "Noto Sans") + 
  theme(axis.title.x = element_blank())

print(p7.1)


################################################################################
# FIGURES BY SEX: 2006, 2015, 2025 (BARS AND PIES) ====




lab_size <- 3.35


# Figure 1.4 ----
out1.4 <- read_xlsx(datafile, sheet = "3.2 K and M 2005-2025", range = "D33:H53")

# Years to show
yrs1.4 <- c(2006, 2015, 2025)

df1.4 <- out1.4 |> 
  pivot_longer(cols = 2:5, names_to = "group", values_to = "value") |> 
  rename(year = 1) |> 
  filter(year %in% yrs1.4) |> 
  mutate(year = factor(year, levels = rev(yrs1.4)))   # 2006 on top, 2025 at the bottom

p1.4 <- df1.4 |> 
  ggplot(aes(y = year)) +
  # Bars stacked by sex
  geom_col(data = df1.4 |> filter(group != "Total KM"),
           aes(x = value, fill = group),
           position = position_stack(reverse = TRUE),
           alpha = 0.6,
           width = 0.6) +
  # Labels inside each segment
  geom_text(data = df1.4 |> filter(group != "Total KM"),
            aes(x = value, label = value, colour = group, group = group),
            size = lab_size,
            position = position_stack(vjust = 0.5, reverse = TRUE)) +
  # Total label at the end of the bar
  geom_text(data = df1.4 |> filter(group == "Total KM"),
            aes(x = value, label = value),
            hjust = -0.3,
            fontface = "bold",
            colour = unicefcol[8]) +
  scale_fill_discrete(palette = unicefcol[c(2, 4, 6)]) +
  scale_colour_discrete(
    palette = c("darkblue", "darkgreen", "darkred"),
    guide = "none"
  ) +
  scale_x_continuous(
    limits = c(0, 16000),
    breaks = breaks_width(2500)
  ) + 
  labs(fill = "Sex:", x = "Killing and Maiming Cases") +
  theme_hc(base_family = "Noto Sans") + 
  theme(axis.title.y = element_blank(),
        panel.grid.major.y = element_blank(),                  # no horizontal gridlines
        panel.grid.major.x = element_line(colour = "grey90"))  # vertical gridlines

print(p1.4)


# Figure 1.5 ----
# Pies by sex (2006, 2015, 2025), using the same data as Fig 1.4
df1.5 <- df1.4 |> 
  mutate(year = as.numeric(as.character(year)),      # back to numeric to reorder
         value = replace_na(value, 0)) |>            # empty cells = 0
  group_by(year) |> 
  mutate(total = value[group == "Total KM"]) |>      # total for each year
  ungroup() |> 
  filter(group != "Total KM") |> 
  group_by(year) |> 
  mutate(pct = value / sum(value)) |>                # share of each sex
  ungroup() |> 
  mutate(year_lab = paste0(year, "\nTotal: ", total),
         year_lab = factor(year_lab, levels = unique(year_lab[order(year)])))  # 2006, 2015, 2025

p1.5 <- df1.5 |> 
  ggplot(aes(x = "", y = pct, fill = group)) +       # pct: each pie adds up to 100%
  geom_col(alpha = 0.6, width = 1, colour = "white",
           position = position_stack(reverse = TRUE)) +
  # Percentage inside each slice
  geom_text(aes(label = case_when(pct == 0  ~ NA,
                                  pct < 0.01 ~ "<1%",
                                  .default = percent(pct, accuracy = 1)),
                colour = group),
            size = lab_size,
            position = position_stack(vjust = 0.5, reverse = TRUE)) +
  coord_polar(theta = "y") +
  facet_wrap(~ year_lab) +
  scale_fill_discrete(palette = unicefcol[c(2, 4, 6)]) +
  scale_colour_discrete(
    palette = c("darkblue", "darkgreen", "darkred"),
    guide = "none"
  ) +
  labs(fill = "Sex:") +
  theme_void(base_family = "Noto Sans") + 
  theme(legend.position = "bottom",
        strip.text = element_text(colour = unicefcol[8], face = "bold",
                                  margin = margin(b = 5)))

print(p1.5)


# Figure 2.3 ----
out2.3 <- read_xlsx(datafile, sheet = "4.2 RU 2005-2025 sex disaggr.", range = "E3:I23")

# Years to show
yrs2.3 <- c(2006, 2015, 2025)

df2.3 <- out2.3 |> 
  pivot_longer(cols = 2:5, names_to = "group", values_to = "value") |> 
  rename(year = 1) |> 
  filter(year %in% yrs2.3) |> 
  mutate(year = factor(year, levels = rev(yrs2.3)))   # 2006 on top, 2025 at the bottom

p2.3 <- df2.3 |> 
  ggplot(aes(y = year)) +
  # Bars stacked by sex
  geom_col(data = df2.3 |> filter(!str_detect(group, "Total")),
           aes(x = value, fill = group),
           position = position_stack(reverse = TRUE),
           alpha = 0.6,
           width = 0.6) +
  # Labels inside each segment
  geom_text(data = df2.3 |> filter(!str_detect(group, "Total")),
            aes(x = value, label = value, colour = group, group = group),
            size = lab_size,
            position = position_stack(vjust = 0.5, reverse = TRUE)) +
  # Total label at the end of the bar
  geom_text(data = df2.3 |> filter(str_detect(group, "Total")),
            aes(x = value, label = value),
            hjust = -0.3,
            fontface = "bold",
            colour = unicefcol[8]) +
  scale_fill_discrete(palette = unicefcol[c(2, 4, 6)]) +
  scale_colour_discrete(
    palette = c("darkblue", "darkgreen", "darkred"),
    guide = "none"
  ) +
  scale_x_continuous(
    limits = c(0, 10000),
    breaks = breaks_width(2500)
  ) + 
  labs(fill = "Sex:", x = "Recruitment and Use Cases") +
  theme_hc(base_family = "Noto Sans") + 
  theme(axis.title.y = element_blank(),
        panel.grid.major.y = element_blank(),                  # no horizontal gridlines
        panel.grid.major.x = element_line(colour = "grey90"))  # vertical gridlines

print(p2.3)


# Figure 2.4 ----
# Pies by sex (2006, 2015, 2025), using the same data as Fig 2.3
df2.4 <- df2.3 |> 
  mutate(year = as.numeric(as.character(year)),      # back to numeric to reorder
         value = replace_na(value, 0)) |>            # empty cells = 0
  group_by(year) |> 
  mutate(total = value[str_detect(group, "Total")]) |>  # total for each year
  ungroup() |> 
  filter(!str_detect(group, "Total")) |> 
  group_by(year) |> 
  mutate(pct = value / sum(value)) |>                # share of each sex
  ungroup() |> 
  mutate(year_lab = paste0(year, "\nTotal: ", total),
         year_lab = factor(year_lab, levels = unique(year_lab[order(year)])))  # 2006, 2015, 2025

p2.4 <- df2.4 |> 
  ggplot(aes(x = "", y = pct, fill = group)) +       # pct: each pie adds up to 100%
  geom_col(alpha = 0.6, width = 1, colour = "white",
           position = position_stack(reverse = TRUE)) +
  # Percentage inside each slice
  geom_text(aes(label = case_when(pct == 0  ~ NA,
                                  pct < 0.01 ~ "<1%",
                                  .default = percent(pct, accuracy = 1)),
                colour = group),
            size = lab_size,
            position = position_stack(vjust = 0.5, reverse = TRUE)) +
  coord_polar(theta = "y") +
  facet_wrap(~ year_lab) +
  scale_fill_discrete(palette = unicefcol[c(2, 4, 6)]) +
  scale_colour_discrete(
    palette = c("darkblue", "darkgreen", "darkred"),
    guide = "none"
  ) +
  labs(fill = "Sex:") +
  theme_void(base_family = "Noto Sans") + 
  theme(legend.position = "bottom",
        strip.text = element_text(colour = unicefcol[8], face = "bold",
                                  margin = margin(b = 5)))

print(p2.4)


# Figure 3.3 ----
out3.3 <- read_xlsx(datafile, sheet = "5.2 SV 2005-2025 sex disaggr.", range = "E5:I25")

# Years to show
yrs3.3 <- c(2006, 2015, 2025)

df3.3 <- out3.3 |> 
  pivot_longer(cols = 2:5, names_to = "group", values_to = "value") |> 
  rename(year = 1) |> 
  filter(year %in% yrs3.3) |> 
  mutate(year = factor(year, levels = rev(yrs3.3)))   # 2006 on top, 2025 at the bottom

p3.3 <- df3.3 |> 
  ggplot(aes(y = year)) +
  # Bars stacked by sex
  geom_col(data = df3.3 |> filter(!str_detect(group, "Total")),
           aes(x = value, fill = group),
           position = position_stack(reverse = TRUE),
           alpha = 0.6,
           width = 0.6) +
  # Labels inside each segment
  geom_text(data = df3.3 |> filter(!str_detect(group, "Total")),
            aes(x = value, label = value, colour = group, group = group),
            size = lab_size,
            position = position_stack(vjust = 0.5, reverse = TRUE)) +
  # Total label at the end of the bar
  geom_text(data = df3.3 |> filter(str_detect(group, "Total")),
            aes(x = value, label = value),
            hjust = -0.3,
            fontface = "bold",
            colour = unicefcol[8]) +
  scale_fill_discrete(palette = unicefcol[c(2, 4, 6)]) +
  scale_colour_discrete(
    palette = c("darkblue", "darkgreen", "darkred"),
    guide = "none"
  ) +
  scale_x_continuous(
    limits = c(0, 2000),
    breaks = breaks_width(500)
  ) + 
  labs(fill = "Sex:", x = "Sexual Violence Cases") +
  theme_hc(base_family = "Noto Sans") + 
  theme(axis.title.y = element_blank(),
        panel.grid.major.y = element_blank(),                  # no horizontal gridlines
        panel.grid.major.x = element_line(colour = "grey90"))  # vertical gridlines

print(p3.3)


# Figure 3.4 ----
# Pies by sex (2006, 2015, 2025), using the same data as Fig 3.3
df3.4 <- df3.3 |> 
  mutate(year = as.numeric(as.character(year)),      # back to numeric to reorder
         value = replace_na(value, 0)) |>            # empty cells = 0
  group_by(year) |> 
  mutate(total = value[str_detect(group, "Total")]) |>  # total for each year
  ungroup() |> 
  filter(!str_detect(group, "Total")) |> 
  group_by(year) |> 
  mutate(pct = value / sum(value)) |>                # share of each sex
  ungroup() |> 
  mutate(year_lab = paste0(year, "\nTotal: ", total),
         year_lab = factor(year_lab, levels = unique(year_lab[order(year)])))  # 2006, 2015, 2025

p3.4 <- df3.4 |> 
  ggplot(aes(x = "", y = pct, fill = group)) +       # pct: each pie adds up to 100%
  geom_col(alpha = 0.6, width = 1, colour = "white",
           position = position_stack(reverse = TRUE)) +
  # Percentage inside each slice
  geom_text(aes(label = case_when(pct == 0  ~ NA,
                                  pct < 0.01 ~ "<1%",
                                  .default = percent(pct, accuracy = 1)),
                colour = group),
            size = lab_size,
            position = position_stack(vjust = 0.5, reverse = TRUE)) +
  coord_polar(theta = "y") +
  facet_wrap(~ year_lab) +
  scale_fill_discrete(palette = unicefcol[c(2, 4, 6)]) +
  scale_colour_discrete(
    palette = c("darkblue", "darkgreen", "darkred"),
    guide = "none"
  ) +
  labs(fill = "Sex:") +
  theme_void(base_family = "Noto Sans") + 
  theme(legend.position = "bottom",
        strip.text = element_text(colour = unicefcol[8], face = "bold",
                                  margin = margin(b = 5)))

print(p3.4)


# Figure 4.3 ----
out4.3 <- read_xlsx(datafile, sheet = "6.2 ABD 2005-2025 sex disaggr.", range = "E5:I25")

# Years to show
yrs4.3 <- c(2006, 2015, 2025)

df4.3 <- out4.3 |> 
  pivot_longer(cols = 2:5, names_to = "group", values_to = "value") |> 
  rename(year = 1) |> 
  filter(year %in% yrs4.3) |> 
  mutate(year = factor(year, levels = rev(yrs4.3)))   # 2006 on top, 2025 at the bottom

p4.3 <- df4.3 |> 
  ggplot(aes(y = year)) +
  # Bars stacked by sex
  geom_col(data = df4.3 |> filter(!str_detect(group, "Total")),
           aes(x = value, fill = group),
           position = position_stack(reverse = TRUE),
           alpha = 0.6,
           width = 0.6) +
  # Labels inside each segment
  geom_text(data = df4.3 |> filter(!str_detect(group, "Total")),
            aes(x = value, label = value, colour = group, group = group),
            size = lab_size,
            position = position_stack(vjust = 0.5, reverse = TRUE)) +
  # Total label at the end of the bar
  geom_text(data = df4.3 |> filter(str_detect(group, "Total")),
            aes(x = value, label = value),
            hjust = -0.3,
            fontface = "bold",
            colour = unicefcol[8]) +
  scale_fill_discrete(palette = unicefcol[c(2, 4, 6)]) +
  scale_colour_discrete(
    palette = c("darkblue", "darkgreen", "darkred"),
    guide = "none"
  ) +
  scale_x_continuous(
    limits = c(0, 6000),
    breaks = breaks_width(1000)
  ) + 
  labs(fill = "Sex:", x = "Abduction Cases") +
  theme_hc(base_family = "Noto Sans") + 
  theme(axis.title.y = element_blank(),
        panel.grid.major.y = element_blank(),                  # no horizontal gridlines
        panel.grid.major.x = element_line(colour = "grey90"))  # vertical gridlines

print(p4.3)


# Figure 4.4 ----
# Pies by sex (2006, 2015, 2025), using the same data as Fig 4.3
df4.4 <- df4.3 |> 
  mutate(year = as.numeric(as.character(year)),      # back to numeric to reorder
         value = replace_na(value, 0)) |>            # empty cells = 0
  group_by(year) |> 
  mutate(total = value[str_detect(group, "Total")]) |>  # total for each year
  ungroup() |> 
  filter(!str_detect(group, "Total")) |> 
  group_by(year) |> 
  mutate(pct = value / sum(value)) |>                # share of each sex
  ungroup() |> 
  mutate(year_lab = paste0(year, "\nTotal: ", total),
         year_lab = factor(year_lab, levels = unique(year_lab[order(year)])))  # 2006, 2015, 2025

p4.4 <- df4.4 |> 
  ggplot(aes(x = "", y = pct, fill = group)) +       # pct: each pie adds up to 100%
  geom_col(alpha = 0.6, width = 1, colour = "white",
           position = position_stack(reverse = TRUE)) +
  # Percentage inside each slice
  geom_text(aes(label = case_when(pct == 0  ~ NA,
                                  pct < 0.01 ~ "<1%",
                                  .default = percent(pct, accuracy = 1)),
                colour = group),
            size = lab_size,
            position = position_stack(vjust = 0.5, reverse = TRUE)) +
  coord_polar(theta = "y") +
  facet_wrap(~ year_lab) +
  scale_fill_discrete(palette = unicefcol[c(2, 4, 6)]) +
  scale_colour_discrete(
    palette = c("darkblue", "darkgreen", "darkred"),
    guide = "none"
  ) +
  labs(fill = "Sex:") +
  theme_void(base_family = "Noto Sans") + 
  theme(legend.position = "bottom",
        strip.text = element_text(colour = unicefcol[8], face = "bold",
                                  margin = margin(b = 5)))

print(p4.4)






################################################################################
# Output to Excel ----
wb <- createWorkbook()

# Trends over time (totals and by sex)
addWorksheet(wb, sheetName = "fig1.1")
addWorksheet(wb, sheetName = "fig1.2")
addWorksheet(wb, sheetName = "fig2.1")
addWorksheet(wb, sheetName = "fig3.1")
addWorksheet(wb, sheetName = "fig4.1")

# Trends by perpetrator
addWorksheet(wb, sheetName = "fig1.3")
addWorksheet(wb, sheetName = "fig2.2")
addWorksheet(wb, sheetName = "fig3.2")
addWorksheet(wb, sheetName = "fig4.2")
addWorksheet(wb, sheetName = "fig5.1")
addWorksheet(wb, sheetName = "fig6.1")
addWorksheet(wb, sheetName = "fig7.1")

# Snapshots by sex: 2006, 2015, 2025 (bars and pies)
addWorksheet(wb, sheetName = "fig1.4")
addWorksheet(wb, sheetName = "fig1.5")
addWorksheet(wb, sheetName = "fig2.3")
addWorksheet(wb, sheetName = "fig2.4")
addWorksheet(wb, sheetName = "fig3.3")
addWorksheet(wb, sheetName = "fig3.4")
addWorksheet(wb, sheetName = "fig4.3")
addWorksheet(wb, sheetName = "fig4.4")

# Titles
writeData(wb, sheet = "fig1.1", "Total Grave Violation Cases", startRow = 1)
writeData(wb, sheet = "fig1.2", "Killing and Maiming Cases by Gender, 2006-2025", startRow = 1)
writeData(wb, sheet = "fig1.3", "Killing and Maiming Cases by Perpetrator, 2006-2025", startRow = 1)
writeData(wb, sheet = "fig1.4", "Killing and Maiming Cases by Gender, 2006, 2015 and 2025", startRow = 1)
writeData(wb, sheet = "fig1.5", "Killing and Maiming Cases by Gender (%), 2006, 2015 and 2025", startRow = 1)
writeData(wb, sheet = "fig2.1", "Recruitment and Use Cases by Gender, 2006-2025", startRow = 1)
writeData(wb, sheet = "fig2.2", "Recruitment and Use Cases by Perpetrator, 2006-2025", startRow = 1)
writeData(wb, sheet = "fig2.3", "Recruitment and Use Cases by Gender, 2006, 2015 and 2025", startRow = 1)
writeData(wb, sheet = "fig2.4", "Recruitment and Use Cases by Gender (%), 2006, 2015 and 2025", startRow = 1)
writeData(wb, sheet = "fig3.1", "Sexual Violence Cases by Gender, 2006-2025", startRow = 1)
writeData(wb, sheet = "fig3.2", "Sexual Violence Cases by Perpetrator, 2006-2025", startRow = 1)
writeData(wb, sheet = "fig3.3", "Sexual Violence Cases by Gender, 2006, 2015 and 2025", startRow = 1)
writeData(wb, sheet = "fig3.4", "Sexual Violence Cases by Gender (%), 2006, 2015 and 2025", startRow = 1)
writeData(wb, sheet = "fig4.1", "Abduction Cases by Gender, 2006-2025", startRow = 1)
writeData(wb, sheet = "fig4.2", "Abduction Cases by Perpetrator, 2006-2025", startRow = 1)
writeData(wb, sheet = "fig4.3", "Abduction Cases by Gender, 2006, 2015 and 2025", startRow = 1)
writeData(wb, sheet = "fig4.4", "Abduction Cases by Gender (%), 2006, 2015 and 2025", startRow = 1)
writeData(wb, sheet = "fig5.1", "Attacks on Schools and Hospitals by Perpetrator, 2006-2025", startRow = 1)
writeData(wb, sheet = "fig6.1", "Denial of Humanitarian Access by Perpetrator, 2006-2025", startRow = 1)
writeData(wb, sheet = "fig7.1", "Total Grave Violation Cases by Perpetrator, 2006-2025", startRow = 1)


# Data and graphics
writeData(wb, sheet = "fig1.1", out1.1, startRow = 3)
writeData(wb, sheet = "fig1.2", out1.2, startRow = 3)
writeData(wb, sheet = "fig1.3", out1.3, startRow = 3)
writeData(wb, sheet = "fig1.4", out1.4[out1.4[[1]] %in% yrs1.4, ], startRow = 3)   # only the 3 years shown
writeData(wb, sheet = "fig1.5", df1.5 |> select(year, group, value, pct), startRow = 3)
writeData(wb, sheet = "fig2.1", out2.1, startRow = 3)
writeData(wb, sheet = "fig2.2", out2.2, startRow = 3)
writeData(wb, sheet = "fig2.3", out2.3[out2.3[[1]] %in% yrs2.3, ], startRow = 3)   # only the 3 years shown
writeData(wb, sheet = "fig2.4", df2.4 |> select(year, group, value, pct), startRow = 3)
writeData(wb, sheet = "fig3.1", out3.1, startRow = 3)
writeData(wb, sheet = "fig3.2", out3.2, startRow = 3)
writeData(wb, sheet = "fig3.3", out3.3[out3.3[[1]] %in% yrs3.3, ], startRow = 3)   # only the 3 years shown
writeData(wb, sheet = "fig3.4", df3.4 |> select(year, group, value, pct), startRow = 3)
writeData(wb, sheet = "fig4.1", out4.1, startRow = 3)
writeData(wb, sheet = "fig4.2", out4.2, startRow = 3)
writeData(wb, sheet = "fig4.3", out4.3[out4.3[[1]] %in% yrs4.3, ], startRow = 3)   # only the 3 years shown
writeData(wb, sheet = "fig4.4", df4.4 |> select(year, group, value, pct), startRow = 3)
writeData(wb, sheet = "fig5.1", out5.1, startRow = 3)
writeData(wb, sheet = "fig6.1", out6.1, startRow = 3)
writeData(wb, sheet = "fig7.1", out7.1, startRow = 3)


p1.1; insertPlot(wb, sheet = "fig1.1", startRow = 3, startCol = 6,  width = 10, height = 5)
p1.2; insertPlot(wb, sheet = "fig1.2", startRow = 3, startCol = 6,  width = 10, height = 5)
p1.3; insertPlot(wb, sheet = "fig1.3", startRow = 3, startCol = 10, width = 10, height = 5)
p1.4; insertPlot(wb, sheet = "fig1.4", startRow = 3, startCol = 7)
p1.5; insertPlot(wb, sheet = "fig1.5", startRow = 3, startCol = 6)
p2.1; insertPlot(wb, sheet = "fig2.1", startRow = 3, startCol = 7,  width = 10, height = 5)
p2.2; insertPlot(wb, sheet = "fig2.2", startRow = 3, startCol = 10, width = 10, height = 5)
p2.3; insertPlot(wb, sheet = "fig2.3", startRow = 3, startCol = 7)
p2.4; insertPlot(wb, sheet = "fig2.4", startRow = 3, startCol = 6)
p3.1; insertPlot(wb, sheet = "fig3.1", startRow = 3, startCol = 7,  width = 10, height = 5)
p3.2; insertPlot(wb, sheet = "fig3.2", startRow = 3, startCol = 10, width = 10, height = 5)
p3.3; insertPlot(wb, sheet = "fig3.3", startRow = 3, startCol = 7)
p3.4; insertPlot(wb, sheet = "fig3.4", startRow = 3, startCol = 6)
p4.1; insertPlot(wb, sheet = "fig4.1", startRow = 3, startCol = 7,  width = 10, height = 5)
p4.2; insertPlot(wb, sheet = "fig4.2", startRow = 3, startCol = 10, width = 10, height = 5)
p4.3; insertPlot(wb, sheet = "fig4.3", startRow = 3, startCol = 7)
p4.4; insertPlot(wb, sheet = "fig4.4", startRow = 3, startCol = 6)
p5.1; insertPlot(wb, sheet = "fig5.1", startRow = 3, startCol = 10, width = 10, height = 5)
p6.1; insertPlot(wb, sheet = "fig6.1", startRow = 3, startCol = 10, width = 10, height = 5)
p7.1; insertPlot(wb, sheet = "fig7.1", startRow = 3, startCol = 10, width = 10, height = 5)


saveWorkbook(wb, here("outputs", "For Designer.xlsx"), overwrite = TRUE)


