# =============================================================================
# 03_process.R — Build the analytic layer: peers, expectations, verdicts
# -----------------------------------------------------------------------------
# The question each city gets answered:
#   "Given how rich and how compact you are, is your walking access to a
#    hospital better or worse than cities like you?"
#
# Two estimates of "expected" access, deliberately:
#   (A) Peer median  - transparent; used for the public-facing verdict.
#   (B) Median regression - adjusts continuously for density and size;
#       used as a robustness check (the two should broadly agree).
# =============================================================================

source("R/02_clean.R")
suppressPackageStartupMessages(library(quantreg))

# --- 3.1 Analysis frame ---------------------------------------------------------
cities <- cities_clean |>
  mutate(
    in_frame  = pop >= CONFIG$min_pop & income_label != "Not classified",
    has_hosp  = !is.na(hosp_share_1km),
    has_pharm = !is.na(pharm_share_1km)
  )

# --- 3.2 Bands for size and density ---------------------------------------------
# Density quintiles are cut on ALL in-frame cities (not just those with data),
# so a city's band does not depend on whether its hospitals were mapped.
density_breaks <- cities |>
  filter(in_frame) |>
  pull(density) |>
  quantile(probs = seq(0, 1, length.out = CONFIG$n_density_bands + 1))
density_breaks[c(1, length(density_breaks))] <- c(0, Inf)

band_labels <- c("Most sprawling", "Sprawling", "Middle", "Compact", "Most compact")

cities <- cities |>
  mutate(
    density_band = cut(density, density_breaks, labels = band_labels,
                       include.lowest = TRUE, ordered_result = TRUE),
    size_band = cut(pop, c(0, 1e5, 2.5e5, 1e6, 5e6, Inf),
                    labels = c("<100k", "100k-250k", "250k-1M", "1M-5M", "5M+"),
                    right = FALSE, ordered_result = TRUE)
  )

# --- 3.3 Data-confidence grade ------------------------------------------------
# Honest labelling beats silent exclusion. Every city carries a grade.
cities <- cities |>
  mutate(
    confidence = case_when(
      has_hosp & !is.na(hosp_n) & has_pharm ~ "High",    # both services mapped
      has_hosp                              ~ "Medium",  # hospitals only
      TRUE                                  ~ "Low"      # no hospital data
    ),
    confidence = factor(confidence, levels = CONF_LEVELS),
    likely_map_gap = !has_hosp & pop >= CONFIG$gap_pop
  )

# --- 3.4 (A) Peer-median expectation -----------------------------------------
# Peers = same income group x same density quintile. Where a cell has fewer
# than `min_peers` observed cities (e.g., sprawling low-income cities), fall
# back to the whole income group and record that we did so.
scored <- cities |> filter(in_frame, has_hosp)

cell_n <- scored |> count(income_label, density_band, name = "cell_n")

# Mid-rank percentile: ties share credit; always strictly inside (0, 1)
mid_pct <- function(x) (rank(x, ties.method = "average") - 0.5) / length(x)

scored <- scored |>
  left_join(cell_n, by = c("income_label", "density_band")) |>
  # statistics against the narrow peer set (income x density) ...
  group_by(income_label, density_band) |>
  mutate(cell_median = median(hosp_share_1km),
         cell_pct    = mid_pct(hosp_share_1km)) |>
  # ... and against the broad fallback set (whole income group)
  group_by(income_label) |>
  mutate(inc_n      = n(),
         inc_median = median(hosp_share_1km),
         inc_pct    = mid_pct(hosp_share_1km)) |>
  ungroup() |>
  mutate(
    use_cell    = cell_n >= CONFIG$min_peers,
    peer_group  = if_else(use_cell,
                          paste(income_label, "·", density_band),
                          paste(income_label, "· all densities (small peer cell)")),
    peer_n      = if_else(use_cell, cell_n, inc_n),
    peer_median = if_else(use_cell, cell_median, inc_median),
    peer_pct    = if_else(use_cell, cell_pct, inc_pct),
    gap_pp      = (hosp_share_1km - peer_median) * 100
  ) |>
  select(-c(cell_n, cell_median, cell_pct, inc_n, inc_median, inc_pct, use_cell))

# --- 3.5 (B) Model-based expectation ---------------------------------------
# Median (tau = 0.5) regression on the logit scale:
#   * medians resist the heavy tails seen in the exploratory plots
#   * the logit keeps predictions inside 0-100%
# Shares of exactly 0 or 1 are squeezed slightly so the logit is finite.
squeeze <- function(p, eps = 0.005) pmin(pmax(p, eps), 1 - eps)

model_df <- scored |>
  mutate(
    y           = qlogis(squeeze(hosp_share_1km)),
    log_density = log(density),
    log_pop     = log(pop),
    income_f    = factor(as.character(income), levels = INCOME_LEVELS)
  )

fit_expected <- suppressWarnings(
  rq(y ~ income_f + log_density + log_pop, tau = 0.5, data = model_df)
)

scored <- scored |>
  mutate(
    expected_model = plogis(predict(fit_expected, newdata = model_df)),
    resid_pp       = (hosp_share_1km - expected_model) * 100
  )

agreement <- cor(scored$gap_pp, scored$resid_pp, method = "spearman")
message(sprintf("Peer-median vs model residual agreement (Spearman): %.2f", agreement))

# --- 3.6 Verdicts ------------------------------------------------------------------
scored <- scored |>
  mutate(
    verdict = case_when(
      peer_pct >= CONFIG$beats_pct ~ "Beats the odds",
      peer_pct <= CONFIG$below_pct ~ "Below expected",
      TRUE                         ~ "As expected"
    )
  )

# --- 3.7 Hospital-vs-pharmacy quadrants (only where BOTH are observed) -----------
both <- scored |> filter(has_pharm)
q_med <- c(hosp = median(both$hosp_share_1km), pharm = median(both$pharm_share_1km))

scored <- scored |>
  mutate(
    quadrant = case_when(
      !has_pharm ~ NA_character_,
      hosp_share_1km >= q_med["hosp"] & pharm_share_1km >= q_med["pharm"] ~ "Well served",
      hosp_share_1km <  q_med["hosp"] & pharm_share_1km >= q_med["pharm"] ~ "Pharmacy-led",
      hosp_share_1km >= q_med["hosp"] & pharm_share_1km <  q_med["pharm"] ~ "Hospital-led",
      TRUE ~ "Thin on both"
    )
  )

# --- 3.8 Reassemble: every city, scored or not -----------------------------------
cities <- cities |>
  left_join(
    scored |> select(id, peer_group, peer_n, peer_median, peer_pct, gap_pp,
                     expected_model, resid_pp, verdict, quadrant),
    by = "id"
  ) |>
  mutate(
    verdict = case_when(
      !is.na(verdict) ~ verdict,
      TRUE            ~ "No access data"
    ),
    verdict = factor(verdict, levels = VERDICT_LEVELS)
  )

attr(cities, "quadrant_medians") <- q_med
attr(cities, "density_breaks")   <- density_breaks
attr(cities, "model_agreement")  <- agreement

saveRDS(cities, CONFIG$processed_path)

message(sprintf(
  "Processed: %s cities in frame, %s scored. Verdicts: %s",
  comma(sum(cities$in_frame)), comma(nrow(scored)),
  paste(names(table(scored$verdict)), table(scored$verdict), sep = " = ", collapse = "; ")
))
