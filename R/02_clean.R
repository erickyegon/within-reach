# =============================================================================
# 02_clean.R — Tidy names, types, labels, and audit data quality
# -----------------------------------------------------------------------------
# Output: `cities_clean` (one row per urban centre, analysis-ready types)
#         `quality_audit` (a small table of data-quality findings, reused in
#                          the app's "Data gaps" tab)
# =============================================================================

source("R/01_import.R")

# --- 2.1 Readable names --------------------------------------------------------
cities_clean <- health_raw |>
  rename(
    id              = ID_UC_G0,
    city            = GC_UCN_MAI_2025,
    country         = GC_CNT_GAD_2025,
    area_km2        = GC_UCA_KM2_2025,
    pop             = GC_POP_TOT_2025,
    income          = GC_DEV_WIG_2025,
    region          = GC_DEV_USR_2025,
    hosp_n          = HL_FCL_HOS_2024,
    pharm_n         = HL_FCL_PHA_2024,
    hosp_per_km2    = HL_FDE_HOS_2024,
    pharm_per_km2   = HL_FDE_PHA_2024,
    hosp_per_cap    = HL_FPC_HOS_2025,
    pharm_per_cap   = HL_FPC_PHA_2025,
    hosp_pop_1km    = HL_POP_HOS_2025,
    pharm_pop_1km   = HL_POP_PHA_2025,
    hosp_share_1km  = HL_SHP_HOS_2025,
    pharm_share_1km = HL_SHP_PHA_2025
  )

# --- 2.2 Text hygiene and factors ----------------------------------------------
cities_clean <- cities_clean |>
  mutate(
    city    = str_squish(city),
    city    = if_else(is.na(city) | city == "", paste0("Unnamed centre #", id), city),
    country = str_squish(country),
    # Ordered income factor; the 9 unclassified centres are kept but labelled
    income  = factor(income, levels = INCOME_LEVELS, ordered = TRUE),
    income_label = fct_na_value_to_level(factor(as.character(income), levels = INCOME_LEVELS, ordered = FALSE),
                                         level = "Not classified"),
    region  = factor(region)
  )

# --- 2.3 Unique, human-readable labels for search -----------------------------
# ~180 name+country pairs repeat (e.g., several "San Jose" centres in one
# country). Append the population to disambiguate them in the search box.
cities_clean <- cities_clean |>
  add_count(city, country, name = "name_dupes") |>
  mutate(
    label = if_else(
      name_dupes > 1,
      sprintf("%s, %s (pop. %s)", city, country,
              fmt_pop(pop)),
      sprintf("%s, %s", city, country)
    ),
    # If two same-named centres have the same rounded pop, fall back to the ID
    label = if_else(duplicated(label) | duplicated(label, fromLast = TRUE),
                    paste0(label, " #", id), label)
  ) |>
  select(-name_dupes)

stopifnot(!anyDuplicated(cities_clean$label))

# --- 2.4 Derived rates in human units ------------------------------------------
# Per-capita values like 0.000039 are unreadable; per 100,000 is the
# epidemiological convention. We rebuild rates from counts (and verify below
# that they match the published per-capita fields).
cities_clean <- cities_clean |>
  mutate(
    density          = pop / area_km2,                 # people per km2
    hosp_per_100k    = hosp_n  / pop * 1e5,
    pharm_per_100k   = pharm_n / pop * 1e5,
    hosp_share_1km   = hosp_share_1km  / 100,           # store as proportions
    pharm_share_1km  = pharm_share_1km / 100
  )

# --- 2.5 Data-quality audit ------------------------------------------------------
# Four findings that change how the data may honestly be used.

obs <- cities_clean |>
  summarise(
    n = n(),
    # (a) Censoring: the smallest recorded count is 2, never 0 or 1.
    min_hosp_count    = min(hosp_n,  na.rm = TRUE),
    min_pharm_count   = min(pharm_n, na.rm = TRUE),
    # (b) Access share present while count missing (1 facility, or facilities
    #     just outside the centre boundary whose 1 km buffer reaches inside)
    share_without_count = sum(!is.na(hosp_share_1km) & is.na(hosp_n)),
    # (c) Heaping: share of recorded counts that are even numbers
    pct_even_hosp  = mean(hosp_n  %% 2 == 0, na.rm = TRUE),
    pct_even_pharm = mean(pharm_n %% 2 == 0, na.rm = TRUE),
    # (d) Rate consistency: our rate vs the published per-capita field
    max_rate_diff  = max(abs(hosp_per_100k - hosp_per_cap * 1e5) /
                           (hosp_per_cap * 1e5), na.rm = TRUE)
  )

quality_audit <- tibble::tribble(
  ~issue, ~finding, ~implication,
  "Censored small counts",
  sprintf("Smallest recorded hospital count is %d and pharmacy count %d; no zeros or ones exist.",
          as.integer(obs$min_hosp_count), as.integer(obs$min_pharm_count)),
  "A missing count may mean 0, 1, or 'not mapped'. NA is never treated as zero.",

  "Access without a count",
  sprintf("%s centres have a hospital access share but no hospital count.",
          comma(obs$share_without_count)),
  "Access is measured from facilities within 1 km, which can lie outside the boundary. We use access share as the primary outcome.",

  "Even-number heaping",
  sprintf("%s of hospital counts and %s of pharmacy counts are even numbers.",
          percent(obs$pct_even_hosp, 1), percent(obs$pct_even_pharm, 1)),
  "Consistent with facilities being recorded twice (e.g., as a point and a building outline). Absolute counts are indicative; rankings and access shares are more robust.",

  "Rates reproduce",
  sprintf("Rebuilt rates match published per-capita fields within %s.",
          percent(obs$max_rate_diff, 0.1)),
  "Counts (2024) and population (2025) differ by a year; the mismatch is negligible."
)

message("\nData-quality audit:")
walk2(quality_audit$issue, quality_audit$finding,
      \(i, f) message("  • ", i, ": ", f))
