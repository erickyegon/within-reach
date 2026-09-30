# =============================================================================
# 07_export_app_data.R — Package exactly what the Shiny app needs
# -----------------------------------------------------------------------------
# The app never recomputes the heavy analysis. It loads one small .rds file,
# so it starts fast and deploys cleanly (the app/ folder is self-contained).
# =============================================================================

if (!exists("findings") || is.null(findings$map)) source("R/05_map_data.R")   # skip if already run

app_cities <- cities |>
  select(id, label, city, country, iso3, region, income_label, pop, area_km2, density,
         density_band, size_band, in_frame, has_hosp, has_pharm, confidence,
         likely_map_gap, hosp_n, pharm_n, hosp_per_100k, pharm_per_100k,
         hosp_share_1km, pharm_share_1km, peer_group, peer_n, peer_median,
         peer_pct, gap_pp, expected_model, resid_pp, verdict, quadrant) |>
  mutate(region = as.character(region))

app_data <- list(
  cities    = app_cities,
  findings  = findings,
  quad_med  = attr(cities, "quadrant_medians"),
  settings  = CONFIG[c("min_pop", "min_peers", "beats_pct", "below_pct", "gap_pop")],
  palettes  = list(income = PAL_INCOME, verdict = PAL_VERDICT, conf = PAL_CONF),
  levels    = list(income = INCOME_LEVELS, verdict = VERDICT_LEVELS, conf = CONF_LEVELS),
  built     = Sys.Date()
)

saveRDS(app_data, CONFIG$app_data_path)
message(sprintf("Saved %s (%s KB)", CONFIG$app_data_path,
                round(file.size(CONFIG$app_data_path) / 1024)))
