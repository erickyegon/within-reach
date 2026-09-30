# =============================================================================
# 05_map_data.R — Geography layer for the "World Map" tab
# -----------------------------------------------------------------------------
# The source file has no coordinates, so the map is country-level. Country
# names are converted to ISO-3 codes HERE (build time), so the deployed app
# needs no extra packages: it only draws a plotly choropleth from `iso3`.
#
# Adds `iso3` to `cities` and a `map_findings` list to `findings`:
#   * country_map : one row per country, unfiltered (used for headlines/README)
#   * the app re-aggregates from city rows so the sidebar filters still apply
# =============================================================================

if (!exists("findings")) source("R/04_analysis.R")   # skip if already run
library(countrycode)

# Names countrycode cannot resolve on its own
iso_overrides <- c("México" = "MEX", "Kosovo" = "XKX")

country_iso <- tibble(country = sort(unique(cities$country))) |>
  mutate(iso3 = countrycode(country, "country.name", "iso3c", warn = FALSE),
         iso3 = coalesce(iso_overrides[country], iso3))

stopifnot("Unmatched country names" = !anyNA(country_iso$iso3))

cities <- cities |> left_join(country_iso, by = "country")

# --- Country summary (same definitions the app uses) ------------------------------
# Population-weighted access = share of ALL scored residents in the country who
# live within 1 km of a hospital. Countries with few scored cities are noisy,
# so a minimum n is applied before anything is ranked.
MAP_MIN_SCORED <- 5

country_map <- cities |>
  filter(in_frame) |>
  group_by(iso3) |>                      # Cyprus + Northern Cyprus share one code
  summarise(
    country     = names(which.max(table(country))),
    region      = names(which.max(table(region))),
    n_frame     = n(),
    n_scored    = sum(has_hosp),
    coverage    = n_scored / n_frame,
    income      = names(which.max(table(income_label))),
    access_w    = if (any(has_hosp)) weighted.mean(hosp_share_1km[has_hosp], pop[has_hosp]) else NA_real_,
    median_gap  = if (any(has_hosp)) median(gap_pp[has_hosp]) else NA_real_,
    h100_median = if (any(!is.na(hosp_per_100k))) median(hosp_per_100k, na.rm = TRUE) else NA_real_,
    .groups = "drop"
  )

ranked <- country_map |> filter(n_scored >= MAP_MIN_SCORED)

findings$map <- list(
  country_map = country_map,
  min_scored  = MAP_MIN_SCORED,
  best_gap    = ranked |> arrange(desc(median_gap)) |> slice_head(n = 5),
  worst_gap   = ranked |> arrange(median_gap)       |> slice_head(n = 5),
  best_access = ranked |> arrange(desc(access_w))   |> slice_head(n = 5),
  worst_access = ranked |> arrange(access_w)        |> slice_head(n = 5),
  n_countries = nrow(country_map),
  n_ranked    = nrow(ranked)
)

saveRDS(cities,   CONFIG$processed_path)
saveRDS(findings, "data/processed/findings.rds")

message(sprintf(
  "\nMAP: %d countries, %d with >= %d scored cities.\n  Best vs peers : %s\n  Worst vs peers: %s",
  nrow(country_map), nrow(ranked), MAP_MIN_SCORED,
  paste(head(findings$map$best_gap$country, 3),  collapse = ", "),
  paste(head(findings$map$worst_gap$country, 3), collapse = ", ")))
