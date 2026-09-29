# =============================================================================
# run_all.R — Rebuild everything from raw data to deployable app, in order
# -----------------------------------------------------------------------------
#   01 import   -> 02 clean  -> 03 process -> 04 analysis
#   05 static poster (output/figures/within_reach_poster.png)
#   06 export app data (app/data/app_data.rds)
# Each script sources its predecessor, so any single step can also be run alone.
# =============================================================================
source("R/05_static_dashboard.R")
source("R/06_export_app_data.R")
message("\nAll done. Launch the app with:  shiny::runApp('app')")
