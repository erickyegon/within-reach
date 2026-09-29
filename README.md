# Within Reach

**Money buys hospitals; compactness buys access.**
A #TidyTuesday (2026, week 39) analysis of hospital and pharmacy access in 11,422 urban centres, using the GHS Urban Centre Database R2024A (European Commission JRC).

## The story in four findings

1. **Money buys facilities.** High-income cities have about 3x the hospitals per 100,000 residents of low-income cities.
2. **...but not walking access.** Their residents are *less* likely to live within 1 km of a hospital (median 24% vs 30%), because high-income cities are about 2.6x less dense.
3. **Compare like with like and the ranking flips.** At equal density and size, high-income cities reach 32% vs 24% for low-income cities (median regression). Adding UN region reverses it again, because income and region are tightly entangled; this is reported, not hidden.
4. **The map is thinnest where need is likely greatest.** Pharmacy data exist for 8% of low-income cities vs 77% of high-income ones. Hong Kong (4.8M people) has no hospital data at all.

## Project structure

```
within-reach/
├── run_all.R                  # rebuilds everything, in order
├── R/
│   ├── 00_config.R            # every analytic assumption + shared palette/theme
│   ├── 01_import.R            # cached download, explicit schema, integrity checks
│   ├── 02_clean.R             # names, factors, unique labels, rates, data-quality audit
│   ├── 03_process.R           # density bands, confidence grades, peer groups, verdicts
│   ├── 04_analysis.R          # bootstrap CIs, median regressions, coverage, leaderboards
│   ├── 05_static_dashboard.R  # one-page shareable poster (patchwork)
│   └── 06_export_app_data.R   # packages a small .rds for the app
├── app/
│   ├── app.R                  # bslib Shiny app (5 tabs)
│   └── data/app_data.rds      # built by run_all.R
└── output/figures/within_reach_poster.png
```

Each script sources its predecessor, so you can run any step on its own.

## Run it

```r
install.packages(c("readr", "dplyr", "tidyr", "stringr", "forcats", "purrr",
                   "ggplot2", "scales", "patchwork", "quantreg",
                   "shiny", "bslib", "bsicons", "plotly", "reactable"))

source("run_all.R")      # from the project root; about 15 seconds
shiny::runApp("app")
```

## Deploy (free) to Posit Connect Cloud

1. Push this folder to a public GitHub repo.
2. In `app/`, run `rsconnect::writeManifest()` and commit the `manifest.json`.
3. At connect.posit.cloud, choose **Publish → Shiny**, pick the repo, and set the primary file to `app/app.R`.

## Methods in brief

- **Frame:** urban centres with 100,000+ residents and a World Bank income group (5,940 cities; 3,927 with hospital access data).
- **Outcome:** share of residents within a 1 km straight-line buffer of a mapped hospital.
- **Peers:** same income group and same density quintile; whole income group if that cell has fewer than 30 cities.
- **Verdicts:** top 20% of peers = *beats the odds*; bottom 20% = *below expected*.
- **Robustness:** a median regression on logit access (income, log density, log population) ranks cities almost identically (Spearman 0.96).

## Data-quality findings worth knowing

- **Censored counts:** the smallest recorded count is 2, so a missing value can mean 0, 1, or unmapped. NA is never treated as zero.
- **Even-number heaping:** 95% of hospital counts and 97% of pharmacy counts are even, consistent with facilities being recorded twice. Absolute counts are indicative only.
- **Access without counts:** 199 centres have an access share but no count (facilities just outside the boundary still reach residents inside).

## Caveats

A 1 km buffer is not travel time. A mapped facility says nothing about quality, capacity, cost, or whether it is currently functioning, which matters in conflict-affected cities. All results are associations, not causes.

---
Analysis: Erick K. Yegon · github.com/erickyegon · linkedin.com/in/erickyegon
