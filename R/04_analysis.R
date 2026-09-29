# =============================================================================
# 04_analysis.R — Quantify the story, with uncertainty
# -----------------------------------------------------------------------------
# Findings produced here feed both the static poster (05) and the app (06).
#   F1  Money buys facilities       (hospitals per 100k rise with income)
#   F2  ...but not walking access   (raw access share does NOT rise)
#   F3  Compactness is the hidden   (compare like-with-like density and
#       variable                     the income gradient reappears)
#   F4  The map is least complete where need is likely greatest
# =============================================================================

source("R/03_process.R")
set.seed(CONFIG$seed)

frame <- cities |> filter(in_frame)
obs   <- frame  |> filter(has_hosp)

# --- 4.1 Bootstrap helper -----------------------------------------------------
# Percentile bootstrap CI for a median. Cheap, assumption-light, and honest
# about how much groups really differ.
boot_median <- function(x, n_boot = CONFIG$n_boot) {
  x <- x[!is.na(x)]
  if (length(x) < 5) return(tibble(est = median(x), lo = NA_real_, hi = NA_real_))
  b <- replicate(n_boot, median(sample(x, replace = TRUE)))
  tibble(est = median(x), lo = unname(quantile(b, .025)), hi = unname(quantile(b, .975)))
}

# --- 4.2 F1 + F2: income gradients, raw ---------------------------------------
by_income <- obs |>
  group_by(income_label) |>
  summarise(
    n = n(),
    share = list(boot_median(hosp_share_1km)),
    h100  = list(boot_median(hosp_per_100k)),
    density_median = median(density),
    .groups = "drop"
  ) |>
  unnest_wider(c(share, h100), names_sep = "_")

# --- 4.3 F3: the reversal, two ways -------------------------------------------
# (a) Stratified: median access by income WITHIN each density quintile
within_band <- obs |>
  group_by(density_band, income_label) |>
  summarise(n = n(), share = median(hosp_share_1km), .groups = "drop") |>
  mutate(share = if_else(n < 15, NA_real_, share))   # suppress unstable cells

# (b) Modelled: median regression, adding covariates step by step
mdf <- obs |>
  mutate(
    y = qlogis(squeeze(hosp_share_1km)),
    income_f = factor(as.character(income), levels = INCOME_LEVELS),
    log_density = log(density), log_pop = log(pop)
  )

model_specs <- list(
  "M1: income only"                  = y ~ income_f,
  "M2: + density & size"             = y ~ income_f + log_density + log_pop,
  "M3: + density, size & UN region"  = y ~ income_f + log_density + log_pop + region
)

model_table <- imap_dfr(model_specs, \(f, nm) {
  fit <- suppressWarnings(rq(f, tau = 0.5, data = mdf))
  s   <- suppressWarnings(summary(fit, se = "boot", R = 300))$coefficients
  tibble(model = nm, term = rownames(s), est = s[, 1], se = s[, 2])
}) |>
  filter(str_detect(term, "income_f|log_")) |>
  mutate(
    term = term |>
      str_remove("income_f") |>
      recode(log_density = "log density", log_pop = "log population"),
    lo = est - 1.96 * se, hi = est + 1.96 * se,
    # Odds ratio on the median access share (vs Low income for income terms)
    or = exp(est), or_lo = exp(lo), or_hi = exp(hi)
  )

# Adjusted access by income: predicted median at the global median density and
# population (i.e., "if every city had the same compactness and size").
fit_m2 <- suppressWarnings(rq(model_specs[[2]], tau = 0.5, data = mdf))
ref <- tibble(
  income_f    = factor(INCOME_LEVELS, levels = INCOME_LEVELS),
  log_density = median(mdf$log_density),
  log_pop     = median(mdf$log_pop)
)
adjusted <- ref |>
  mutate(adj_share = plogis(predict(fit_m2, newdata = ref)),
         income_label = factor(INCOME_LEVELS, levels = levels(obs$income_label))) |>
  select(income_label, adj_share)

by_income <- by_income |> left_join(adjusted, by = "income_label")

# --- 4.4 F4: coverage (who is missing from the map?) ----------------------------
coverage <- frame |>
  group_by(income_label, region) |>
  summarise(n = n(),
            hosp_cov  = mean(has_hosp),
            pharm_cov = mean(has_pharm),
            .groups = "drop")

coverage_income <- frame |>
  group_by(income_label) |>
  summarise(n = n(), hosp_cov = mean(has_hosp), pharm_cov = mean(has_pharm),
            both_cov = mean(has_hosp & has_pharm), .groups = "drop")

map_gaps <- cities |>
  filter(likely_map_gap) |>
  arrange(desc(pop)) |>
  select(label, country, income_label, region, pop)

# --- 4.5 Fair leaderboards --------------------------------------------------------
# Country medians with the n-filter applied BEFORE ranking (fixes the n = 3
# Ukraine artefact in the exploratory chart).
country_table <- obs |>
  filter(!is.na(hosp_per_100k)) |>
  group_by(country, income_label) |>
  summarise(n = n(),
            h100_median  = median(hosp_per_100k),
            share_median = median(hosp_share_1km),
            .groups = "drop") |>
  filter(n >= 8) |>
  arrange(desc(share_median))

top_deviants <- obs |>
  filter(pop >= 5e5) |>
  arrange(desc(gap_pp)) |>
  slice_head(n = 15) |>
  select(label, income_label, density_band, hosp_share_1km, peer_median, gap_pp, peer_pct)

bottom_deviants <- obs |>
  filter(pop >= 5e5) |>
  arrange(gap_pp) |>
  slice_head(n = 15) |>
  select(label, income_label, density_band, hosp_share_1km, peer_median, gap_pp, peer_pct)

# --- 4.6 Headline numbers (plain-language, used as value boxes) ------------------
hi <- by_income |> filter(income_label == "High income")
lo <- by_income |> filter(income_label == "Low income")

headlines <- list(
  h100_ratio      = hi$h100_est / lo$h100_est,
  share_hi        = hi$share_est,
  share_lo        = lo$share_est,
  adj_share_hi    = hi$adj_share,
  adj_share_lo    = lo$adj_share,
  density_ratio   = lo$density_median / hi$density_median,
  pharm_cov_lo    = coverage_income$pharm_cov[coverage_income$income_label == "Low income"],
  pharm_cov_hi    = coverage_income$pharm_cov[coverage_income$income_label == "High income"],
  n_scored        = nrow(obs),
  n_frame         = nrow(frame),
  model_agreement = attr(cities, "model_agreement"),
  pop_far         = sum(obs$pop - obs$pop * obs$hosp_share_1km),
  pop_scored      = sum(obs$pop)
)

findings <- list(
  by_income = by_income, within_band = within_band, model_table = model_table,
  coverage = coverage, coverage_income = coverage_income, map_gaps = map_gaps,
  country_table = country_table, top_deviants = top_deviants,
  bottom_deviants = bottom_deviants, headlines = headlines,
  quality_audit = quality_audit
)

saveRDS(findings, "data/processed/findings.rds")

with(headlines, message(sprintf(paste0(
  "\nHEADLINES\n",
  "  High-income cities have %.1fx the hospitals per 100k of low-income cities.\n",
  "  Raw median walking access: low %s vs high %s.\n",
  "  At equal density & size (M2): low %s vs high %s.\n",
  "  Low-income cities are %.1fx as dense as high-income cities.\n",
  "  Pharmacy data coverage: low %s vs high %s."),
  h100_ratio, percent(share_lo, 1), percent(share_hi, 1),
  percent(adj_share_lo, 1), percent(adj_share_hi, 1),
  density_ratio, percent(pharm_cov_lo, 1), percent(pharm_cov_hi, 1))))
