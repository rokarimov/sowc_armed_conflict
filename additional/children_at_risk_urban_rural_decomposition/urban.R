#3Nov26

# =====================================================================
# Children at risk of armed conflict: urban vs rural
# Shapley-Owen-Shorrocks decomposition of the change in the number of children
# at risk (2015 -> 2025) into four factors:
#   N (child population), m (urbanization), R (country's overall exposure),
#   q (urban tilt of conflict):   A_ci = N x m_ci x R x q_ci
#
# Files needed in the project folder:
#   - num_children_in_confl_globpop.Rdta (confl_num_urb)
#   - pop_2015.tif, pop_2025.tif
#   - worldpop_degurba_l1_2015.tif, worldpop_degurba_l1_2025.tif
# The first run takes ~8 minutes (step 4a). Results are saved in results/.
# =====================================================================


# =====================================================================
# 0. SETUP
# =====================================================================
# dplyr is loaded before terra, as in the tested run
library(dplyr);   library(tidyr)
library(wpp2024); library(countrycode)
library(terra);   library(sf);   library(exactextractr);   library(rnaturalearth)

# load 'num_children_in_confl_globpop.Rdata'
# Contains confl_num_urb, confl_num_countries, confl_num_urb_global, ...
path <- paste0(dirname(dirname(getwd())), "/main/data_derived/num_children_in_confl_globpop.Rdata")
load(path)

YEAR_0 <- 2015            # start year
YEAR_1 <- 2025            # end year
YEARS  <- c(YEAR_0, YEAR_1)

# --- Checks -------------------
stopifnot(exists("confl_num_urb"))
range(confl_num_urb$year)                      # expected: 2015 to 2025
table(confl_num_urb$year)[as.character(YEARS)] # number of rows in each of the two years


# =====================================================================
# 1. A_ci: CHILDREN AT RISK by country, class (urb / rur) and year
#    Source: confl_num_urb$children_in_conflict
# =====================================================================
A_ci <- confl_num_urb |>
  filter(year %in% YEARS) |>
  mutate(class = if_else(urbanicity == "Rural", "rur", "urb")) |>   # urb = town + city
  group_by(iso3, class, year) |>
  summarise(A_ci = sum(children_in_conflict), .groups = "drop") |>
  complete(iso3, class, year = YEARS, fill = list(A_ci = 0))        # zeros: classes / years with no conflict

# --- Checks -------------------
nrow(A_ci)                          # expected: 284 (71 countries x 2 classes x 2 years)
n_distinct(A_ci$iso3)               # expected: 71
anyNA(A_ci$A_ci)                    # expected: FALSE
count(A_ci, class, year)            # each combination should have the same number of rows

# Totals by year must match the global table (same source, so it should be exact)
A_ci |>
  summarise(total = sum(A_ci), .by = year) |>
  inner_join(
    confl_num_urb_global |>
      filter(year %in% YEARS) |>
      summarise(global = sum(children_confl_urb), .by = year),
    by = "year") |>
  mutate(diff = total - global)


# =====================================================================
# 2. UNIVERSE: countries with children at risk in at least one of the two years
#    Type A = conflict in both years
#    Type B = enters or exits conflict (peaceful in one of the two years)
#    Type C = Countries at peace in both years are left out
# =====================================================================
tipos <- A_ci |>
  summarise(A_c = sum(A_ci), .by = c(iso3, year)) |>
  pivot_wider(names_from = year, values_from = A_c, names_prefix = "y") |>
  rename(A_c_0 = all_of(paste0("y", YEAR_0)),
         A_c_1 = all_of(paste0("y", YEAR_1))) |>
  mutate(type = case_when(A_c_0 > 0 & A_c_1 > 0 ~ "A: conflict in both years",
                          A_c_0 > 0 | A_c_1 > 0 ~ "B: enters or exits conflict",
                          TRUE                  ~ "none"),
         peaceful_year = case_when(type == "B: enters or exits conflict" & A_c_0 == 0 ~ YEAR_0,
                                   type == "B: enters or exits conflict" & A_c_1 == 0 ~ YEAR_1,
                                   TRUE ~ NA_real_))

paises <- tipos |> filter(type != "none") |> pull(iso3)

# --- Checks -------------------
count(tipos, type)                  # expected: A = 41, B = 30
length(paises)                      # expected: 71
count(filter(tipos, type == "B: enters or exits conflict"), peaceful_year)


# =====================================================================
# 3. N_c: TOTAL CHILDREN (under 18) by country and year
#    Source: WPP 2024. Same rule as R's scripts: the WPP year is one year
#    behind the raster year (2014 -> 2015, 2024 -> 2025), and 2024 comes from
#    the projections because popAge1dt only goes up to 2023.
# =====================================================================
data(popAge1dt)
data(popprojAge1dt)

popAge <- bind_rows(
  popAge1dt,
  popprojAge1dt |> filter(year == 2024) |> mutate(year = as.integer(year))
) |> select(any_of(names(popAge1dt)))

wpp <- popAge |>
  filter(year %in% (YEARS - 1), country_code < 900) |>       # country_code >= 900 are aggregates
  summarise(N_c         = sum(pop[age < 18], na.rm = TRUE) * 1000,   # persons (WPP is in thousands)
            total_pop   = sum(pop, na.rm = TRUE) * 1000,
            .by = c(country_code, year)) |>
  mutate(iso3        = countrycode(country_code, "un", "iso3c", warn = FALSE),
         year        = year + 1,                              # WPP year -> raster year
         share_child = N_c / total_pop) |>                    # used in step 4
  filter(iso3 %in% paises) |>
  select(iso3, year, N_c, total_pop, share_child)

# --- Checks -------------------
nrow(wpp)                                    # expected: 142 (71 countries x 2 years)
setdiff(paises, wpp$iso3)                    # expected: character(0)
anyNA(wpp)                                   # expected: FALSE

# Where R's table has childpop (thousands), the two must coincide
confl_num_urb |>
  distinct(iso3, year, childpop) |>
  filter(year %in% YEARS) |>
  inner_join(wpp, by = c("iso3", "year")) |>
  summarise(rows = n(), max_diff = max(abs(childpop * 1000 - N_c)))   # expected: 112 rows, ~0


# =====================================================================
# 4a. N_ci: POPULATION BY URBANICITY CLASS (whole country, not only conflict zones)
#     Same procedure as 2_calc_country_totals.R, without the conflict-zone mask:
#     sum the population raster by DEGURBA class inside each country border.
#     DEGURBA values: 1 = Rural, 2 = Urban cluster/town, 3 = Urban centre/city
#     Results are cached in N_ci_personas_<year>.rds (first run takes ~8 minutes;
#     delete these files to rebuild).
# =====================================================================

# Country borders: same Natural Earth borders as R's scripts
countries <- ne_countries(scale = "medium", returnclass = "sf") |>
  filter(iso_a3_eh != "-99") |>
  rename(iso3 = iso_a3_eh) |>
  group_by(iso3) |>
  summarise(.groups = "drop") |>
  st_make_valid()

PopByClass <- function(iso, pop, urb, y) {
  country <- countries |> filter(iso3 == iso)
  stopifnot(nrow(country) == 1)
  
  pop_c <- crop(pop, ext(vect(country)))
  urb_c <- urb |>
    crop(project(ext(pop_c), from = crs(pop_c), to = crs(urb))) |>
    project(pop_c, method = "near")            # align DEGURBA to the population grid
  
  # Whole-cell counting, as in 2_calc_country_totals.R: a cell belongs to the country
  # if its center is inside the border, and then counts in full
  pop_m <- mask(pop_c, rasterize(vect(country), pop_c, field = 1, background = NA))
  total <- global(pop_m, "sum", na.rm = TRUE)[[1]]
  cl    <- sapply(1:3, function(k) global(pop_m * (urb_c == k), "sum", na.rm = TRUE)[[1]])

  # Fractional-cell counting (previous version): border cells count by the share of
  # the cell inside the country
  # total <- exact_extract(pop_c, country, "sum")
  # cl    <- sapply(1:3, function(k) exact_extract(pop_c * (urb_c == k), country, "sum"))

  tibble(iso3 = iso, year = y, pop_total = total,
         pop_cl1 = cl[1], pop_cl2 = cl[2], pop_cl3 = cl[3],
         pop_no_class = total - sum(cl))       # people in cells with no DEGURBA class
}

PopByClassYear <- function(y, country_list) {
  cache <- sprintf("N_ci_personas_wholecell_%d.rds", y)   # fractional-cell results: N_ci_personas_<year>.rds
  if (file.exists(cache)) { message("Using cached file: ", cache); return(readRDS(cache)) }
  
  pop <- rast(sprintf("pop_%d.tif", y))
  urb <- rast(sprintf("worldpop_degurba_l1_%d.tif", y))
  
  res <- lapply(seq_along(country_list), function(i) {
    iso <- country_list[i]
    message(y, "  ", i, "/", length(country_list), "  ", iso, "  ", format(Sys.time(), "%H:%M:%S"))
    tryCatch(PopByClass(iso, pop, urb, y),
             error = function(e) { message("  ERROR in ", iso, ": ", conditionMessage(e))
               tibble(iso3 = iso, year = y) })
  }) |> bind_rows()
  
  saveRDS(res, cache)
  res
}

pop_by_class <- bind_rows(lapply(YEARS, PopByClassYear, country_list = paises))

# --- Checks -------------------
nrow(pop_by_class)                           # expected: 142
filter(pop_by_class, is.na(pop_total))       # countries that failed, expected: none
names(readRDS("N_ci_personas_wholecell_2015.rds"))     # expected: last column is pop_no_class

# Raster total vs WPP total population: should be ~1 now that the 2025 raster is fixed
pop_by_class |>
  inner_join(wpp, by = c("iso3", "year")) |>
  mutate(ratio = pop_total / total_pop) |>
  summarise(median = median(ratio), min = min(ratio), max = max(ratio), .by = year)

# Countries that are not close to 1 (expected: only ESH 'Western Sahara', in both years)
pop_by_class |>
  inner_join(wpp, by = c("iso3", "year")) |>
  mutate(ratio = pop_total / total_pop) |>
  filter(abs(ratio - 1) > 0.01)

filter(A_ci, iso3 == "ESH")


# People in cells with no DEGURBA class, as % of the country (expected: small)
pop_by_class |>
  mutate(no_class_pct = 100 * pop_no_class / pop_total) |>
  summarise(median = median(no_class_pct), max = max(no_class_pct), .by = year)


# Notes on the results:
# - Western Sahara (ESH) is the only country whose raster total is clearly below the WPP
#   total: 0.916 in 2015 and 0.844 in 2025 (raster 439,050 vs WPP 479,451 in 2015; 503,043
#   vs 595,829 in 2025). All other countries are within 1% of WPP in both years. The cause
#   for ESH is not known. It has no children at risk in 2015 and about 3,600 in 2025 (of
#   576.7 million in total), all in the rural class.

# - Cells with no DEGURBA class hold a very small share of the population: median 0.062%
#   in 2015 and 0.006% in 2025 (maximum 0.48% and 0.12%). N_c is defined as the sum of the
#   classes (step 5), so these people are left out of N_c, and the urban and rural shares
#   add up to exactly 1 (step 6, check 2).

# =====================================================================
# 4b. N_ci: CHILDREN BY CLASS (whole country, inside and outside conflict zones)
#     N_ci = population in class i x national under-18 share (WPP).
#     Same construction as children_in_conflict in R's tables, so that
#     N_ci and A_ci are consistent.
#     urb = Urban cluster/town (2) + Urban centre/city (3); rur = Rural (1)
# =====================================================================
N_ci <- pop_by_class |>
  inner_join(select(wpp, iso3, year, share_child), by = c("iso3", "year")) |>
  transmute(iso3, year,
            rur = pop_cl1 * share_child,
            urb = (pop_cl2 + pop_cl3) * share_child) |>
  pivot_longer(c(rur, urb), names_to = "class", values_to = "N_ci")


# --- Checks -------------------
nrow(N_ci)                                   # expected: 284 (71 countries x 2 classes x 2 years)
anyNA(N_ci$N_ci)                             # expected: FALSE

# Sum of the two classes vs total children from WPP (N_c)
N_ci |>
  summarise(N_sum = sum(N_ci), .by = c(iso3, year)) |>
  inner_join(select(wpp, iso3, year, N_c), by = c("iso3", "year")) |>
  mutate(ratio = N_sum / N_c) |>
  summarise(median = median(ratio), min = min(ratio), max = max(ratio), .by = year)

# Countries not close to 1 (expected: only ESH)
N_ci |>
  summarise(N_sum = sum(N_ci), .by = c(iso3, year)) |>
  inner_join(select(wpp, iso3, year, N_c), by = c("iso3", "year")) |>
  mutate(ratio = N_sum / N_c) |>
  filter(ratio < 0.99)

# Children at risk should not exceed children in the class (expected: the 6 cases noted below)
A_ci |>
  inner_join(N_ci, by = c("iso3", "class", "year")) |>
  filter(A_ci > N_ci) |>
  mutate(excess_pct = 100 * (A_ci / N_ci - 1)) |>
  select(iso3, class, year, excess_pct)


# Note: in 6 country-class-year cases (BDI, LBN, PSE, SYR) A_ci exceeds N_ci by at most
# 0.14%, i.e. the exposure rate A_ci / N_ci is slightly above 1. The cause has not been
# investigated. Left as is so that A_ci still adds up exactly to confl_num_urb_global
# (step 1 check).


# =====================================================================
# 5. INPUT TABLE: one row per country, class and year
#    A_ci : children at risk in class i          (step 1)
#    N_ci : children in class i                  (step 4)
#    N_c  : total children = N_c,urb + N_c,rur   (sum of the two classes, so that
#           the urban and rural shares add up to exactly 1; the WPP total is kept
#           as N_c_wpp for reference)
# =====================================================================
input <- A_ci |>
  inner_join(N_ci, by = c("iso3", "class", "year")) |>
  mutate(N_c = sum(N_ci), .by = c(iso3, year)) |>
  inner_join(select(wpp, iso3, year, N_c_wpp = N_c), by = c("iso3", "year")) |>
  arrange(iso3, class, year)

# --- Checks -------------------
nrow(input)                                  # expected: 284
anyNA(input)                                 # expected: FALSE
count(input, class, year)                    # expected: 71 rows in each combination

# Exposure rate of each class (A_ci / N_ci): should be between 0 and ~1
summary(input$A_ci / input$N_ci)             # expected: max about 1.0014 (the 6 cases noted above)

# N_c (sum of classes) vs WPP total: only ESH should be away from 1
input |>
  distinct(iso3, year, N_c, N_c_wpp) |>
  mutate(ratio = N_c / N_c_wpp) |>
  filter(ratio < 0.99)                       # expected: ESH, both years


# N_c (sum of classes) vs WPP total: only ESH should be away from 1
input |>
  distinct(iso3, year, N_c, N_c_wpp) |>
  mutate(ratio = N_c / N_c_wpp) |>
  filter(abs(ratio - 1) > 0.01)              # expected: ESH, both years


# =====================================================================
# 6. THE FOUR FACTORS (one row per country, class and year)
#    A_ci = N x m_ci x R x q_ci
# =====================================================================
factors <- input |>
  mutate(A_c = sum(A_ci), .by = c(iso3, year)) |>       # children at risk in the whole country
  mutate(
    # Factor 1 - N: child population of the country
    N = N_c,
    
    # Factor 2 - m: share of the country's children living in class i (urbanization)
    m = N_ci / N_c,
    
    # Factor 3 - R: share of ALL the country's children inside a conflict zone
    R = A_c / N_c,
    
    # Factor 4 - q: exposure of class i relative to the country average (urban tilt)
    #   r_ci = A_ci / N_ci is the exposure rate of the class; q = r_ci / R
    #   Convention: q = 1 when R = 0 (no conflict, nothing to compare)
    r = if_else(N_ci > 0, A_ci / N_ci, 0),
    q = if_else(R > 0, r / R, 1)
  ) |>
  select(iso3, class, year, A_ci, N, m, R, q)

# --- Checks -------------------
# 1. The factors must rebuild A_ci exactly
factors |> summarise(max_diff = max(abs(N * m * R * q - A_ci)))      # expected: ~0 (rounding)

# 2. Urban and rural shares add up to 1 in every country and year
factors |> summarise(sum_m = sum(m), .by = c(iso3, year)) |>
  summarise(min = min(sum_m), max = max(sum_m))                      # expected: 1 and 1

# 3. Constraint of the document: sum of m x q = 1 in every country and year
factors |> summarise(sum_mq = sum(m * q), .by = c(iso3, year)) |>
  summarise(min = min(sum_mq), max = max(sum_mq))                    # expected: 1 and 1

# 4. R is a share of children: between 0 and ~1
summary(factors$R)

# 5. A few countries to look at: Colombia, and two that enter / exit conflict
filter(factors, iso3 == "COL")
filter(factors, iso3 %in% c("ARG", "ARM"))

# 6. Country-years where R exceeds 1
factors |> filter(R > 1) |> distinct(iso3, year, R) 


# Notes:
# - N and R are country-level: they repeat in the urban and rural rows of a country-year.
#   Only m and q differ between classes.
# - R is above 1 only in Lebanon (LBN) and Palestine (PSE) in 2025 (maximum 1.0012, see
#   checks 4 and 6). Both are among the 6 cases noted in step 4b, where A_ci exceeds N_ci.
# - sum(m * q) = 1 in every country-year (check 3), including countries with R = 0,
#   where q = 1 and the sum equals the sum of the m's (check 2).


# =====================================================================
# 7a. SHAPLEY-OWEN-SHORROCKS DECOMPOSITION (function and tests)
#     Splits the change in A_ci = N x m x R x q between the four factors.
#     For each factor j: weighted average of [A(T + j) - A(T)] over all subsets T of
#     the other factors, where the factors in T are already at their year-1 value.
#     Weights |T|! (k-1-|T|)! / k!  with k = 4  ->  1/4, 1/12, 1/12, 1/4
# =====================================================================
Shapley4 <- function(f0, f1) {
  # f0, f1: numeric matrices, one row per observation, columns = factors
  #         (same names and order), values at the start year (f0) and the end year (f1)
  nm <- colnames(f0); k <- length(nm); n <- nrow(f0)
  
  # All 2^k combinations of "factor already switched to its year-1 value"
  masks <- as.matrix(expand.grid(rep(list(c(FALSE, TRUE)), k)))
  colnames(masks) <- nm
  
  # Value of the product under each combination (n rows x 2^k columns)
  v <- matrix(sapply(seq_len(nrow(masks)), function(s) {
    Reduce(`*`, lapply(nm, function(j) if (masks[s, j]) f1[, j] else f0[, j]))
  }), nrow = n)
  
  # Contribution of each factor
  phi <- sapply(nm, function(j) {
    out <- numeric(n)
    for (s in which(!masks[, j])) {                          # combinations without j
      s_with_j <- which(apply(masks, 1, function(r) all(r == replace(masks[s, ], j, TRUE))))
      size <- sum(masks[s, ])
      out  <- out + factorial(size) * factorial(k - 1 - size) / factorial(k) *
        (v[, s_with_j] - v[, s])
    }
    out
  })
  matrix(phi, nrow = n, dimnames = list(NULL, nm))
}

# --- Tests -------------------
f <- c("N", "m", "R", "q")

# Test 1: the made-up country 
# Expected: N = 15.08, m = 32.78, R = 18.38, q = 37.75; they add up to 104
p0 <- matrix(c(1000, .4, .2,  .5), nrow = 1, dimnames = list(NULL, f))
p1 <- matrix(c(1200, .6, .25, .8), nrow = 1, dimnames = list(NULL, f))
Shapley4(p0, p1)
sum(Shapley4(p0, p1))                                   # expected: 104 (= 144 - 40)

# Test 2: a country at peace in both years (R = 0, q = 1 by convention)
# Expected: all four contributions equal to 0
z0 <- matrix(c(1000, .4, 0, 1), nrow = 1, dimnames = list(NULL, f))
z1 <- matrix(c(1200, .6, 0, 1), nrow = 1, dimnames = list(NULL, f))
Shapley4(z0, z1)

# Test 3: several rows at once give the same answer as one row at a time
Shapley4(rbind(p0, z0), rbind(p1, z1))                  # expected: row 1 as in test 1, row 2 all zeros

rm(f, p0, p1, z0, z1)


# =====================================================================
# 7b. SHAPLEY DECOMPOSITION BY COUNTRY AND CLASS
#     delta A_ci = phi_N + phi_m + phi_R + phi_q
#     Countries that enter / exit conflict are handled by the same code: in their
#     peaceful year R = 0 and q = 1 (convention), while N and m are real values.
# =====================================================================
f_names <- c("N", "m", "R", "q")

# One row per country and class; factors at start and end year side by side
wide <- factors |>
  pivot_wider(id_cols = c(iso3, class), names_from = year,
              values_from = c(A_ci, N, m, R, q))

GetFactors <- function(df, y) {
  mat <- as.matrix(df[paste0(f_names, "_", y)])
  colnames(mat) <- f_names
  mat
}

phi <- Shapley4(GetFactors(wide, YEAR_0), GetFactors(wide, YEAR_1)) |>
  as_tibble() |>
  rename_with(~ paste0("phi_", .x))

shap <- wide |>
  transmute(iso3, class,
            A_start = .data[[paste0("A_ci_", YEAR_0)]],
            A_end   = .data[[paste0("A_ci_", YEAR_1)]],
            delta_A = A_end - A_start) |>
  bind_cols(phi) |>
  mutate(phi_total = phi_N + phi_m + phi_R + phi_q)

# --- Checks -------------------
nrow(shap)                                   # expected: 142 (71 countries x 2 classes)
anyNA(shap)                                  # expected: FALSE

# 1. The four contributions add up to the change in every row
max(abs(shap$phi_total - shap$delta_A))      # expected: ~0 (rounding)

# 2. The change adds up to the global totals we checked in step 1
sum(shap$delta_A)                            # expected: 203,037,185 (= 576,682,449 - 373,645,264)

# 3. Global decomposition by class
shap |>
  summarise(across(c(delta_A, starts_with("phi_")), sum), .by = class) |>
  mutate(across(starts_with("phi_") & !phi_total, ~ 100 * .x / delta_A, .names = "pct_{.col}"))

shap |>
  summarise(across(c(delta_A, phi_N, phi_m, phi_R, phi_q), ~ round(sum(.x) / 1e6, 1)), .by = class) |>
  as.data.frame()


# 4. One country as an example (Colombia)
filter(shap, iso3 == "COL")

# 5. phi_q added across the two classes
sum(shap$phi_q)

# Notes on the results:
# - The four contributions add up to delta_A in every row, and delta_A adds up to
#   203,037,185 children, the same total as in step 1.
# - Percentages of the rural class are not meaningful: its total change is negative and
#   small (about -19.5 million) compared with its contributions, so dividing by it flips
#   signs and inflates the shares. Report the rural class in millions of children.
# - phi_q does not add up to zero across the two classes (check 5: about -22.7 million).
# - In the peaceful year of countries that enter / exit conflict, q = 1 by convention
#   (R = 0). See the sensitivity check in step 8b.

# =====================================================================
# 8a. WHICH COUNTRIES DRIVE EACH COMPONENT (urban class)
# =====================================================================

# --- Part 1: countries ---
# Top 5 countries by absolute contribution, for each factor
shap |>
  filter(class == "urb") |>
  pivot_longer(c(phi_N, phi_m, phi_R, phi_q), names_to = "factor", values_to = "phi") |>
  mutate(factor = sub("phi_", "", factor)) |>
  group_by(factor) |>
  slice_max(abs(phi), n = 5) |>
  ungroup() |>
  select(factor, iso3, phi) |>
  arrange(factor, desc(abs(phi))) |>
  print(n = 20)

# Share of each factor held by its five largest countries
shap |>
  filter(class == "urb") |>
  pivot_longer(c(phi_N, phi_m, phi_R, phi_q), names_to = "factor", values_to = "phi") |>
  mutate(factor = sub("phi_", "", factor)) |>
  group_by(factor) |>
  summarise(top5  = sum(phi[order(abs(phi), decreasing = TRUE)][1:5]),
          total = sum(phi),
          rest  = total - top5,
          pct_top5 = 100 * top5 / total)

# --- Part 2: country types ---
# Contribution of type A countries (conflict in both years) vs type B (enter / exit)
shap |>
  filter(class == "urb") |>
  left_join(select(tipos, iso3, type), by = "iso3") |>
  summarise(countries = n(), across(c(delta_A, phi_N, phi_m, phi_R, phi_q), sum), .by = type)

# Share of the urban change by country type
shap |>
  filter(class == "urb") |>
  left_join(select(tipos, iso3, type), by = "iso3") |>
  summarise(delta_A = sum(delta_A), .by = type) |>
  mutate(pct_of_change = 100 * delta_A / sum(delta_A))



# Notes on the results:
# Part 1 (countries)
# - Nigeria and Ethiopia are both among the five largest contributions in all four
#   factors (urban class).
# - The five largest countries hold 76% of phi_N, 60% of phi_R and 47% of phi_m. For phi_q
#   they add up to 107% of the total (-16.0 vs -15.0 million); the rest of the countries
#   add +1.0 million.
# Part 2 (country types)
# - Type B countries (enter / exit conflict, q = 1 in their peaceful year) have a phi_q of
#   +1.6 million, against -16.6 million for type A countries (total phi_q for the urban
#   class: -15.0 million). They account for 10.5% of the urban change.



# =====================================================================
# 8b. SENSITIVITY: alternative convention for q in the peaceful year
#     Baseline: q = 1 when R = 0.  Alternative: use q of the other year.
# =====================================================================
RunShapley <- function(fct) {
  wide <- fct |>
    pivot_wider(id_cols = c(iso3, class), names_from = year,
                values_from = c(A_ci, N, m, R, q))
  phi <- Shapley4(GetFactors(wide, YEAR_0), GetFactors(wide, YEAR_1)) |>
    as_tibble() |> rename_with(~ paste0("phi_", .x))
  wide |>
    transmute(iso3, class,
              delta_A = .data[[paste0("A_ci_", YEAR_1)]] - .data[[paste0("A_ci_", YEAR_0)]]) |>
    bind_cols(phi)
}

factors_alt <- factors |>
  mutate(q = if_else(R == 0 & any(R > 0), q[R > 0][1], q), .by = c(iso3, class))

shap_alt <- RunShapley(factors_alt)

# --- Checks -------------------
# 1. The function reproduces the baseline when given the baseline factors
all.equal(RunShapley(factors)$phi_q, shap$phi_q)          # expected: TRUE

# 2. Baseline vs alternative, urban class, in millions
bind_rows(
  shap     |> mutate(version = "baseline (q = 1)"),
  shap_alt |> mutate(version = "alternative (q of the other year)")
) |>
  filter(class == "urb") |>
  summarise(across(c(delta_A, phi_N, phi_m, phi_R, phi_q), ~ sum(.x) / 1e6), .by = version)

# 3. With the alternative, phi_q of the type B countries is zero by construction
shap_alt |>
  left_join(select(tipos, iso3, type), by = "iso3") |>
  filter(class == "urb", type == "B: enters or exits conflict") |>
  summarise(phi_q_typeB = sum(phi_q) / 1e6)               # expected: ~0


# 4. With the alternative, sum(m * q) is no longer 1 in the type B countries
factors_alt |>
  summarise(sum_mq = sum(m * q), .by = c(iso3, year)) |>
  left_join(select(tipos, iso3, type), by = "iso3") |>
  summarise(min = min(sum_mq), max = max(sum_mq), .by = type)     # expected: type A = 1 and 1; type B away from 1

# Notes on the results:
# - Switching to the alternative convention (q of the other year in the peaceful year)
#   changes phi_q for the urban class from -15.0 to -16.6 million, and phi_R by about
#   +1.1 million; delta_A is unchanged and phi_m and phi_N barely move. phi_q is negative
#   under both conventions.
# - With the alternative, q is the same in both years for type B countries, so their
#   phi_q is zero by construction (the baseline gives them about +1.6 million).
# - Under the alternative, sum(m * q) in type B countries ranges from 0.71 to 1.80
#   (check 4); it stays at exactly 1 in type A countries.



# =====================================================================
# 9. SAVE RESULTS (R format and csv)
# =====================================================================
dir.create("results", showWarnings = FALSE)

# Decomposition by country and class (baseline: q = 1 in the peaceful year)
decomp_country <- shap |>
  left_join(select(tipos, iso3, type), by = "iso3")

# Global decomposition by class
decomp_global <- shap |>
  summarise(across(c(delta_A, starts_with("phi_")), sum), .by = class)

# Sensitivity to the q convention
decomp_sensitivity <- bind_rows(
  shap     |> mutate(version = "baseline (q = 1)"),
  shap_alt |> mutate(version = "alternative (q of the other year)")
) |>
  summarise(across(c(delta_A, phi_N, phi_m, phi_R, phi_q), sum), .by = c(version, class))

# R format: one file with the three tables (and the input table, so the
# decomposition can be traced back to A_ci, N_ci and N_c)
save(decomp_country, decomp_global, decomp_sensitivity, input,
     file = "results/decomposition_results.RData")

# csv: one file per table
write.csv(decomp_country,     "results/decomposition_by_country.csv",  row.names = FALSE)
write.csv(decomp_global,      "results/decomposition_global.csv",      row.names = FALSE)
write.csv(decomp_sensitivity, "results/decomposition_sensitivity.csv", row.names = FALSE)
write.csv(input,              "results/input_table.csv",               row.names = FALSE)

# --- Check -------------------
file.info(list.files("results", full.names = TRUE))[, "mtime"]   # expected: today's date