# =============================================================================
# 01_import.R — Get the raw data, once, and check it is what we expect
# -----------------------------------------------------------------------------
# Principles:
#   * Download once and cache locally (reproducible, offline-friendly).
#   * Declare column types explicitly: never let readr guess silently.
#   * Fail loudly if the schema drifts from the data dictionary.
# =============================================================================

source("R/00_config.R")

# --- 1.1 Download (cached) ----------------------------------------------------
if (!file.exists(CONFIG$raw_path)) {
  message("Downloading raw data ...")
  download.file(CONFIG$data_url, CONFIG$raw_path, mode = "wb", quiet = TRUE)
} else {
  message("Using cached raw data: ", CONFIG$raw_path)
}

# --- 1.2 Read with an explicit schema -----------------------------------------
# The upstream cleaning script coded "-" as NA; we keep that and add "" too.
raw_spec <- cols(
  ID_UC_G0        = col_integer(),
  GC_UCN_MAI_2025 = col_character(),
  GC_CNT_GAD_2025 = col_character(),
  GC_UCA_KM2_2025 = col_double(),
  GC_POP_TOT_2025 = col_double(),
  GC_DEV_WIG_2025 = col_character(),
  GC_DEV_USR_2025 = col_character(),
  HL_FCL_HOS_2024 = col_double(),
  HL_FCL_PHA_2024 = col_double(),
  HL_FDE_HOS_2024 = col_double(),
  HL_FDE_PHA_2024 = col_double(),
  HL_FPC_HOS_2025 = col_double(),
  HL_FPC_PHA_2025 = col_double(),
  HL_POP_HOS_2025 = col_double(),
  HL_POP_PHA_2025 = col_double(),
  HL_SHP_HOS_2025 = col_double(),
  HL_SHP_PHA_2025 = col_double()
)

health_raw <- read_csv(CONFIG$raw_path, col_types = raw_spec,
                       na = c("", "NA", "-"), progress = FALSE)

# --- 1.3 Schema and integrity checks ------------------------------------------
check <- function(ok, msg) {
  if (!isTRUE(ok)) stop("Import check failed: ", msg, call. = FALSE)
  message("  ✓ ", msg)
}

message("Import checks:")
check(identical(names(health_raw), names(raw_spec$cols)), "columns match data dictionary")
check(nrow(health_raw) > 10000,                  "row count plausible (>10,000 urban centres)")
check(!anyDuplicated(health_raw$ID_UC_G0),       "ID_UC_G0 is a unique key")
check(all(health_raw$GC_POP_TOT_2025 > 0),        "population strictly positive")
check(all(health_raw$GC_UCA_KM2_2025 > 0),        "area strictly positive")
check(all(between(na.omit(health_raw$HL_SHP_HOS_2025), 0, 100)), "hospital access share within 0-100")
check(all(between(na.omit(health_raw$HL_SHP_PHA_2025), 0, 100)), "pharmacy access share within 0-100")

message(sprintf("Imported %s urban centres x %s variables.",
                comma(nrow(health_raw)), ncol(health_raw)))
