# =============================================================================
# 06_static_dashboard.R — One-page shareable poster for #TidyTuesday
# -----------------------------------------------------------------------------
# Five panels, one argument:
#   A  Money buys facilities
#   B  ...but not walking access, until you compare like with like
#   C  The like-with-like view (income x density)
#   D  The map is least complete where it matters most
#   E  World map: which countries beat the odds, and which fall short
# =============================================================================

if (!exists("findings") || is.null(findings$map)) source("R/05_map_data.R")   # skip if already run
suppressPackageStartupMessages({ library(patchwork); library(sf); library(rnaturalearth) })

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

# --- E. World map: median gap against comparable cities ----------------------------
# Same definition as the app's "Beats the odds?" lens. Countries with fewer than
# POSTER_MIN_SCORED scored cities are left grey rather than shown as noise. This is
# stricter than the app's default (5) because a poster cannot be filtered.
GAP_CAP <- 15               # colour scale is capped at +/- this many points
POSTER_MIN_SCORED <- f$map$strict_min

poster_map <- f$map$country_map |> filter(n_scored >= POSTER_MIN_SCORED)
best_gap   <- poster_map |> arrange(desc(median_gap)) |> slice_head(n = 3)
worst_gap  <- poster_map |> arrange(median_gap)       |> slice_head(n = 3)

world <- ne_countries(scale = "medium", returnclass = "sf") |>
  filter(continent != "Antarctica") |>
  mutate(iso3 = if_else(adm0_a3 == "KOS", "XKX", iso_a3_eh))

map_df <- world |>
  left_join(f$map$country_map |>
              mutate(gap = if_else(n_scored >= POSTER_MIN_SCORED, median_gap, NA_real_)) |>
              select(iso3, gap), by = "iso3")

names_of <- function(d) paste(d$country[1:3], collapse = ", ")
names_n  <- function(d) paste(sprintf("%s %d", d$country[1:3], d$n_scored[1:3]), collapse = ", ")

pE <- ggplot(map_df) +
  geom_sf(aes(fill = gap), colour = "white", linewidth = .12) +
  scale_fill_gradient2(
    low = PAL_VERDICT[["Below expected"]], mid = "#EEF0F2", high = PAL_VERDICT[["Beats the odds"]],
    midpoint = 0, limits = c(-GAP_CAP, GAP_CAP), oob = squish, na.value = "#D9D9D9",
    name = "Median gap vs
comparable cities
(percentage points)",
    breaks = c(-15, -10, -5, 0, 5, 10, 15),
    labels = c("-15 or less", "-10", "-5", "0", "+5", "+10", "+15 or more"),
    guide = guide_colourbar(barheight = unit(3.2, "cm"), barwidth = unit(.4, "cm"))) +
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  labs(title = "E  Where cities beat the odds, and where they fall short",
       subtitle = sprintf(paste0(
         "Country median gap in 1 km hospital access vs peers of the same income group and density band. ",
         "Grey = fewer than %d scored cities (%d of %d countries shown).
",
         "Furthest ahead: %s.  Furthest behind: %s."),
         POSTER_MIN_SCORED, nrow(poster_map), nrow(f$map$country_map),
         names_of(best_gap), names_of(worst_gap))) +
  labs(caption = sprintf(paste0(
         "Read with care: country medians rest on as few as %d scored cities, so extremes can still be noise. ",
         "Country size is not shown.
Scored cities behind the leaders: %s. Behind the laggards: %s."),
         POSTER_MIN_SCORED, names_n(best_gap), names_n(worst_gap))) +
  theme_reach(11) +
  theme(axis.text = element_blank(), panel.grid = element_blank(),
        legend.position = "right", legend.justification = "center",
        plot.caption.position = "plot",
        plot.caption = element_text(colour = "grey35", size = 9, hjust = 0, margin = margin(t = 6)))

# --- Assemble ----------------------------------------------------------------------
poster <- (pA | pB) / (pC | pD) / pE +
  plot_layout(heights = c(1, 1, 1.15)) +
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
      "missing is not zero. Associations, not causes.  |  Analysis & graphics: Erick K. Yegon (github.com/erickyegon)"),
    theme = theme_reach(12) +
      theme(plot.title = element_text(size = 20, face = "bold"),
            plot.subtitle = element_text(size = 11.5, lineheight = 1.15))
  )

ggsave("output/figures/within_reach_poster.png", poster,
       width = 14, height = 14, dpi = 300, bg = "white")
message("Saved output/figures/within_reach_poster.png")
