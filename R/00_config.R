# =============================================================================
# 00_config.R — Project-wide settings
# -----------------------------------------------------------------------------
# Every analytic choice that could be questioned lives here, in one place,
# so a reviewer can see (and change) the assumptions without hunting.
# =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(forcats)
  library(purrr)
  library(ggplot2)
  library(scales)
})

CONFIG <- list(
  # --- Source ----------------------------------------------------------------
  data_url = paste0(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/",
    "data/2026/2026-09-29/health.csv"
  ),
  raw_path       = "data/raw/health.csv",
  processed_path = "data/processed/cities.rds",
  app_data_path  = "app/data/app_data.rds",

  # --- Analytic frame ----------------------------------------------------------
  # Per-capita rates are unstable for small centres (one hospital in a town of
  # 50k looks "better" than 100 in a city of 10M). 100k is the analysis floor.
  min_pop = 1e5,

  # --- Peer groups for "expected access" ---------------------------------------
  # A city's peers share its World Bank income group AND its density quintile.
  # Density matters because the 1 km access share is largely a function of how
  # compact the city is (our core finding).
  n_density_bands = 5,
  min_peers       = 30,     # fewer peers than this -> fall back to income only

  # --- Verdict thresholds (percentile within peer group) -----------------------
  beats_pct = 0.80,         # top 20% of peers  -> "Beats the odds"
  below_pct = 0.20,         # bottom 20%        -> "Below expected"

  # --- Mapping-gap flag --------------------------------------------------------
  # A city this large with no recorded hospital is far more likely a gap in the
  # open map data than a genuine absence of hospitals.
  gap_pop = 1e6,

  # --- Reproducibility ---------------------------------------------------------
  seed   = 2026,
  n_boot = 1000
)

# --- Small formatting helpers (base R, so they work on any scales version) ----
fmt_pop <- function(x) {
  ifelse(x >= 1e6, sprintf("%.1fM", x / 1e6), sprintf("%.0fk", x / 1e3))
}

# --- Shared visual identity (used by static figures AND the Shiny app) --------
INCOME_LEVELS <- c("Low income", "Lower Middle", "Upper Middle", "High income")

# Ordinal palette: warm (lower income) -> cool (higher income)
PAL_INCOME <- c(
  "Low income"   = "#C8553D",
  "Lower Middle" = "#E9A03B",
  "Upper Middle" = "#4F9A94",
  "High income"  = "#2E4A6B"
)

VERDICT_LEVELS <- c("Beats the odds", "As expected", "Below expected", "No access data")
PAL_VERDICT <- c(
  "Beats the odds" = "#1F9E89",
  "As expected"    = "#9AA5B1",
  "Below expected" = "#D1495B",
  "No access data" = "#D9D9D9"
)

CONF_LEVELS <- c("High", "Medium", "Low")
PAL_CONF <- c("High" = "#1F9E89", "Medium" = "#E9A03B", "Low" = "#D1495B")

theme_reach <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title.position = "plot",
      plot.title    = element_text(face = "bold", size = rel(1.25)),
      plot.subtitle = element_text(colour = "grey35", margin = margin(b = 8)),
      plot.caption  = element_text(colour = "grey45", size = rel(0.75), hjust = 0),
      panel.grid.minor = element_blank(),
      legend.position  = "top",
      legend.justification = "left",
      strip.text = element_text(face = "bold", hjust = 0)
    )
}

dir.create("data/raw",       recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("app/data",       recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
