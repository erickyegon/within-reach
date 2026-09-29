# =============================================================================
# 05_static_dashboard.R — One-page shareable poster for #TidyTuesday
# -----------------------------------------------------------------------------
# Four panels, one argument:
#   A  Money buys facilities
#   B  ...but not walking access, until you compare like with like
#   C  The like-with-like view (income x density)
#   D  The map is least complete where it matters most
# =============================================================================

if (!exists("findings")) source("R/04_analysis.R")   # skip if already run
suppressPackageStartupMessages({ library(patchwork) })

f  <- findings
bi <- f$by_income |> filter(income_label %in% INCOME_LEVELS) |>
  mutate(income_label = factor(income_label, levels = INCOME_LEVELS))

# --- A. Hospitals per 100k, median with bootstrap 95% CI --------------------------
pA <- ggplot(bi, aes(x = income_label, y = h100_est, colour = income_label)) +
  geom_linerange(aes(ymin = h100_lo, ymax = h100_hi), linewidth = 4, alpha = .3) +
  geom_point(size = 4) +
  geom_text(aes(label = sprintf("%.1f", h100_est)), nudge_x = .3, size = 3.6,
            colour = "grey20", fontface = "bold") +
  scale_colour_manual(values = PAL_INCOME, guide = "none") +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, .1))) +
  labs(title = "A  Money buys facilities",
       subtitle = sprintf("Median hospitals per 100,000 people.\nHigh-income cities have %.1fx the low-income rate.",
                          f$headlines$h100_ratio),
       x = NULL, y = NULL) +
  theme_reach(11)

# --- B. Raw vs density-adjusted access (dumbbell) -----------------------------------
bB <- bi |>
  select(income_label, raw = share_est, adjusted = adj_share) |>
  pivot_longer(-income_label, names_to = "type", values_to = "share")

bB_seg <- bi |> select(income_label, raw = share_est, adjusted = adj_share)

pB <- ggplot(bB, aes(x = share, y = income_label)) +
  # arrow runs FROM observed TO adjusted, so the direction of change is visible
  geom_segment(data = bB_seg, aes(x = raw, xend = adjusted, y = income_label,
                                  yend = income_label),
               colour = "grey70", linewidth = 1.1,
               arrow = arrow(length = unit(2.2, "mm"), type = "closed")) +
  geom_point(aes(shape = type, colour = income_label), size = 3.6, stroke = 1.3) +
  scale_shape_manual(values = c(raw = 1, adjusted = 16), breaks = c("raw", "adjusted"),
                     labels = c(raw = "As observed", adjusted = "At equal density & size"),
                     name = NULL) +
  scale_colour_manual(values = PAL_INCOME, guide = "none") +
  scale_x_continuous(labels = percent_format(1), breaks = seq(0.20, 0.40, 0.02)) +
  labs(title = "B  ...compactness buys walking access",
       subtitle = "Median share of residents within 1 km of a hospital.\nAdjust for density and the ranking flips.",
       x = NULL, y = NULL) +
  theme_reach(11)

# --- C. Like with like: income x density heatmap ------------------------------------
pC <- f$within_band |>
  filter(income_label %in% INCOME_LEVELS) |>
  mutate(income_label = factor(income_label, levels = INCOME_LEVELS)) |>
  ggplot(aes(x = income_label, y = density_band, fill = share)) +
  geom_tile(colour = "white", linewidth = 1.2) +
  geom_text(aes(label = if_else(is.na(share), "too few", percent(share, 1)),
                colour = share > 0.33), size = 3.4, fontface = "bold") +
  scale_fill_gradient(low = "#EEF3F7", high = "#2E4A6B", na.value = "grey95",
                      labels = percent_format(1), guide = "none") +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "grey20"),
                      na.value = "grey55", guide = "none") +
  labs(title = "C  Among cities of similar density, richer is generally better",
       subtitle = "Median 1 km hospital access, cities of similar compactness",
       x = NULL, y = NULL) +
  theme_reach(11) +
  theme(panel.grid = element_blank())

# --- D. Coverage: the map is thinnest where need is likely greatest ----------------
pD <- f$coverage_income |>
  filter(income_label %in% INCOME_LEVELS) |>
  mutate(income_label = factor(income_label, levels = INCOME_LEVELS)) |>
  select(income_label, Hospitals = hosp_cov, Pharmacies = pharm_cov) |>
  pivot_longer(-income_label, names_to = "facility", values_to = "cov") |>
  ggplot(aes(x = cov, y = income_label, fill = facility)) +
  geom_col(position = position_dodge(width = .75), width = .7) +
  geom_text(aes(label = percent(cov, 1)), position = position_dodge(width = .75),
            hjust = -0.15, size = 3.3) +
  scale_fill_manual(values = c(Hospitals = "#2E4A6B", Pharmacies = "#E9A03B"), name = NULL) +
  scale_x_continuous(labels = percent_format(1), limits = c(0, 1.08),
                     expand = expansion(mult = c(0, 0))) +
  labs(title = "D  The map is thinnest where need is greatest",
       subtitle = "Share of cities (pop. 100k+) with any recorded facility access data",
       x = NULL, y = NULL) +
  theme_reach(11)

# --- Assemble ----------------------------------------------------------------------
poster <- (pA | pB) / (pC | pD) +
  plot_annotation(
    title = "Within Reach: money buys hospitals, compactness buys access",
    subtitle = paste0(
      "Richer cities have far more hospitals per person, yet their residents are ",
      "less likely to live within 1 km of one, because they sprawl.\n",
      "Compare cities of similar density and the advantage of wealth reappears. ",
      sprintf("Based on %s urban centres of 100,000+ people with recorded access data.",
              comma(f$headlines$n_scored))),
    caption = paste0(
      "Data: GHS Urban Centre Database R2024A, European Commission JRC (#TidyTuesday 2026 wk 39). ",
      "Medians with bootstrap 95% CIs; adjusted values from median regression on logit access, log density, log population.\n",
      "Caveats: a 1 km straight-line buffer is not travel time; a mapped facility is not proof of quality or functioning; ",
      "missing is not zero. Associations, not causes.  |  Analysis: Erick K. Yegon"),
    theme = theme_reach(12) +
      theme(plot.title = element_text(size = 20, face = "bold"),
            plot.subtitle = element_text(size = 11.5, lineheight = 1.15))
  )

ggsave("output/figures/within_reach_poster.png", poster,
       width = 14, height = 10, dpi = 300, bg = "white")
message("Saved output/figures/within_reach_poster.png")
