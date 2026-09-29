# =============================================================================
# Within Reach — Shiny app (bslib)
# -----------------------------------------------------------------------------
# Run locally:   shiny::runApp("app")
# Deploy:        Posit Connect Cloud (publish the app/ folder; data included)
#
# Build the data first by running run_all.R from the project root.
# =============================================================================

library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(reactable)
library(scales)

# ---- Data -----------------------------------------------------------------------
app_data <- readRDS("data/app_data.rds")
cities   <- app_data$cities
F        <- app_data$findings
PAL_INC  <- app_data$palettes$income
PAL_VER  <- app_data$palettes$verdict
PAL_CONF <- app_data$palettes$conf
INC_LEV  <- app_data$levels$income
MIN_POP  <- app_data$settings$min_pop

frame_all <- cities |> filter(in_frame)
regions   <- sort(unique(frame_all$region))

city_choices <- setNames(cities$id, cities$label)
city_choices <- city_choices[order(-cities$pop)]    # big cities first in search

pct  <- function(x, acc = 1) ifelse(is.na(x), "–", percent(x, accuracy = acc))
num1 <- function(x) ifelse(is.na(x), "–", sprintf("%.1f", x))
fmt_pop <- function(x) ifelse(x >= 1e6, sprintf("%.1fM", x / 1e6), sprintf("%.0fk", x / 1e3))
ordinal <- function(p) {
  n <- round(p * 100)
  suf <- ifelse(n %% 100 %in% 11:13, "th",
                c("th", "st", "nd", "rd", rep("th", 6))[n %% 10 + 1])
  paste0(n, suf)
}

# Consistent plotly styling
style_plotly <- function(p, ...) {
  p |>
    layout(font = list(family = "Inter, system-ui, sans-serif", size = 12, color = "#2b2b2b"),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
           legend = list(orientation = "h", x = 0, y = 1.12),
           margin = list(l = 10, r = 10, t = 10, b = 10), ...) |>
    config(displaylogo = FALSE,
           modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"))
}

caveat <- function(...) {
  div(class = "small text-muted mt-2", bs_icon("info-circle"), " ", ...)
}

# ---- Theme ------------------------------------------------------------------------
theme <- bs_theme(
  version = 5,
  primary = "#2E4A6B", secondary = "#6c757d", success = "#1F9E89",
  danger  = "#D1495B", warning = "#E9A03B",
  # Inter from Google Fonts, with system fallbacks if it cannot load
  base_font    = font_collection(font_google("Inter", local = FALSE),
                                 "system-ui", "-apple-system", "Segoe UI", "sans-serif"),
  heading_font = font_collection(font_google("Inter", local = FALSE),
                                 "system-ui", "-apple-system", "Segoe UI", "sans-serif")
) |>
  bs_add_rules("
    .verdict-badge { font-size: 1.35rem; font-weight: 700; padding: .45rem 1rem;
                     border-radius: 999px; color: white; display: inline-block; }
    .big-sentence  { font-size: 1.1rem; line-height: 1.55; }
    .card-header   { font-weight: 600; }
    .lede          { font-size: 1.05rem; color: #444; max-width: 70ch; }
    .footer-note   { font-size: .8rem; color: #777; }
  ")

# ---- UI ---------------------------------------------------------------------------
sidebar_filters <- sidebar(
  title = "Filter cities",
  width = 290,
  checkboxGroupInput("income", "World Bank income group",
                     choices = INC_LEV, selected = INC_LEV),
  selectizeInput("region", "UN SDG region", choices = regions, selected = regions,
                 multiple = TRUE, options = list(plugins = list("remove_button"))),
  selectInput("min_pop", "Minimum city population",
              choices = c("100,000" = 1e5, "250,000" = 2.5e5, "500,000" = 5e5,
                          "1 million" = 1e6, "5 million" = 5e6),
              selected = 1e5),
  hr(),
  div(class = "footer-note",
      p("Filters apply to the Story, Beating the Odds, and Hospitals vs Pharmacies tabs."),
      p("Data: GHS Urban Centre Database R2024A (EC JRC), via #TidyTuesday 2026 week 39."))
)

ui <- page_navbar(
  id = "main",
  title = tags$span(bs_icon("hospital"), " Within Reach"),
  theme = theme,
  bg = "#2E4A6B",              # dark navbar; bslib switches text to white
  sidebar = sidebar_filters,
  fillable = FALSE,
  # Small client-side helper for the "Copy link" button
  header = tags$script(HTML("
    Shiny.addCustomMessageHandler('copy_url', function(x) {
      navigator.clipboard && navigator.clipboard.writeText(window.location.href);
    });")),

  # ---------------- Tab 1: The Story ----------------
  nav_panel(
    "The Story", icon = bs_icon("book"),
    p(class = "lede mt-2",
      "Richer cities have far more hospitals per person. Yet their residents are ",
      tags$b("less"), " likely to live within walking distance of one, because richer cities ",
      "sprawl. Compare cities of similar compactness and the advantage of wealth reappears."),
    layout_column_wrap(
      width = 1/4, fill = FALSE,
      uiOutput("vb_h100"), uiOutput("vb_raw"), uiOutput("vb_adj"), uiOutput("vb_cov")
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(full_screen = TRUE,
           card_header("1. Money buys facilities"),
           plotlyOutput("p_h100", height = 300),
           caveat("Median hospitals per 100,000 residents. Hover for the number of cities.")),
      card(full_screen = TRUE,
           card_header("2. ...but not walking access"),
           plotlyOutput("p_raw", height = 300),
           caveat("Median share of residents within 1 km (straight line) of a hospital."))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(full_screen = TRUE,
           card_header("3. Compare like with like: access rises with income within each density band"),
           plotlyOutput("p_band", height = 340),
           caveat("Bands are density quintiles. Groups with fewer than 15 cities are hidden.")),
      card(full_screen = TRUE,
           card_header("4. Compactness is the hidden variable"),
           plotlyOutput("p_density", height = 340),
           caveat("Each dot is a city. Low-income cities are about 2.6 times as dense as high-income cities."))
    )
  ),

  # ---------------- Tab 2: Beating the Odds ----------------
  nav_panel(
    "Beating the Odds", icon = bs_icon("trophy"),
    p(class = "lede mt-2",
      "Each city is compared with its peers: cities in the same income group and the same ",
      "density band. Cities above the diagonal have better walking access than cities like them. ",
      tags$b("Click any dot to open that city's profile.")),
    layout_columns(
      col_widths = c(7, 5),
      card(full_screen = TRUE,
           card_header("Observed vs expected access"),
           plotlyOutput("p_odds", height = 470),
           caveat(sprintf(paste("Expected = median-regression prediction from income, density and size.",
                                "Verdicts use peer percentiles; the two methods agree closely (Spearman %.2f)."),
                          F$headlines$model_agreement))),
      navset_card_tab(
        full_screen = TRUE,
        title = "Standouts (pop. 500k+)",
        nav_panel("Beating the odds", reactableOutput("t_top")),
        nav_panel("Falling short",   reactableOutput("t_bottom"))
      )
    )
  ),

  # ---------------- Tab 3: Your City ----------------
  nav_panel(
    "Your City", icon = bs_icon("geo-alt"), value = "city_tab",
    layout_columns(
      col_widths = c(8, 4),
      selectizeInput("city", NULL, choices = NULL, width = "100%",
                     options = list(placeholder = "Search any of 11,422 urban centres...")),
      div(class = "d-flex gap-2 justify-content-end",
          downloadButton("dl_card", "Download card", class = "btn-primary"),
          actionButton("copy_link", "Copy link", icon = icon("link"),
                       class = "btn-outline-secondary"))
    ),
    uiOutput("city_panel")
  ),

  # ---------------- Tab 4: Hospitals vs Pharmacies ----------------
  nav_panel(
    "Hospitals vs Pharmacies", icon = bs_icon("capsule"),
    p(class = "lede mt-2",
      "Where both services are mapped, which cities are close to a pharmacy but far from a hospital, ",
      "or the reverse? Read with care: pharmacy data exist for only a small share of lower-income cities."),
    layout_columns(
      col_widths = c(7, 5),
      card(full_screen = TRUE,
           card_header("Four kinds of city"),
           plotlyOutput("p_quad", height = 480),
           caveat("Lines mark the medians among cities where both services are observed.")),
      card(card_header("How many cities in each quadrant?"),
           reactableOutput("t_quad"),
           caveat("Counts and hospital-pharmacy rates barely correlate: they are both ",
                  "driven by city size, not by each other."))
    )
  ),

  # ---------------- Tab 5: Data Gaps & Methods ----------------
  nav_panel(
    "Data Gaps & Methods", icon = bs_icon("clipboard-data"),
    layout_columns(
      col_widths = c(7, 5),
      card(full_screen = TRUE,
           card_header("Where is the map incomplete?"),
           plotlyOutput("p_cov", height = 380),
           caveat("Share of cities (pop. 100k+) with any recorded hospital access data.")),
      card(full_screen = TRUE,
           card_header("Big cities with no hospital data: likely mapping gaps"),
           reactableOutput("t_gaps"),
           caveat("A city of a million people with no recorded hospital is almost certainly ",
                  "a gap in the open map, not a city without hospitals."))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Data-quality audit"), reactableOutput("t_audit")),
      card(card_header("Robustness: how the income effect changes with adjustment"),
           reactableOutput("t_models"),
           caveat("Odds ratios for the median share within 1 km, vs low-income cities. ",
                  "M1 = income only; M2 = + density & size; M3 = + UN region. Adding region reverses the effect again: income and region are tightly ",
                  "entangled, so the like-with-like comparison is the more interpretable one."))
    ),
    card(
      card_header("Methods and caveats"),
      markdown(paste0(
"**Frame.** Urban centres with 100,000+ residents and a World Bank income classification. ",
"Per-capita rates are unstable for smaller centres.

**Outcome.** The share of residents living within a 1 km straight-line buffer of a mapped hospital. ",
"This is the most complete and least distorted access field in the data.

**Peers and verdicts.** Peers share a city's income group and density quintile (whole income group ",
"if that cell has fewer than 30 cities). A city in the top 20% of its peers *beats the odds*; bottom 20% is *below expected*.

**Robustness.** A median regression on logit access with income, log density and log population gives ",
"near-identical rankings (Spearman ", sprintf("%.2f", F$headlines$model_agreement), ").

**Caveats.** A 1 km buffer is not travel time. A mapped facility says nothing about quality, capacity, cost, ",
"or whether it is currently functioning (a real concern in conflict-affected cities). Missing is not zero: ",
"the smallest recorded count is 2, so a gap may mean 0, 1, or simply unmapped. Facility counts are ",
"overwhelmingly even numbers, which suggests double-recording, so absolute counts are indicative only. ",
"All results are associations, not causes."))
    )
  ),

  nav_spacer(),
  nav_item(tags$a(bs_icon("github"), href = "https://github.com/erickyegon", target = "_blank")),
  nav_item(tags$a(bs_icon("linkedin"), href = "https://linkedin.com/in/erickyegon", target = "_blank"))
)

# ---- Server -----------------------------------------------------------------------
server <- function(input, output, session) {

  # ---- Filtered frame (shared by several tabs) ----
  filt <- reactive({
    req(input$income, input$region)
    frame_all |>
      filter(income_label %in% input$income,
             region %in% input$region,
             pop >= as.numeric(input$min_pop))
  })
  scored <- reactive(filt() |> filter(has_hosp))

  inc_summary <- reactive({
    scored() |>
      group_by(income_label) |>
      summarise(n = n(),
                h100  = median(hosp_per_100k, na.rm = TRUE),
                share = median(hosp_share_1km),
                .groups = "drop") |>
      filter(income_label %in% INC_LEV) |>
      mutate(income_label = factor(income_label, levels = INC_LEV)) |>
      arrange(income_label)
  })

  # ---- Value boxes ----
  pair_text <- function(tbl, col, fmt) {
    if (nrow(tbl) == 0) return(list(value = "–", sub = "No cities match the filters"))
    lo <- tbl[1, ]; hi <- tbl[nrow(tbl), ]
    if (nrow(tbl) == 1) return(list(value = fmt(lo[[col]]), sub = as.character(lo$income_label)))
    list(value = paste(fmt(hi[[col]]), "vs", fmt(lo[[col]])),
         sub   = paste(hi$income_label, "vs", lo$income_label))
  }

  output$vb_h100 <- renderUI({
    x <- pair_text(inc_summary(), "h100", num1)
    value_box("Hospitals per 100k (median)", x$value, x$sub,
              showcase = bs_icon("building-add"), theme = "primary")
  })
  output$vb_raw <- renderUI({
    x <- pair_text(inc_summary(), "share", pct)
    value_box("Live within 1 km of a hospital", x$value, x$sub,
              showcase = bs_icon("person-walking"), theme = "light")
  })
  output$vb_adj <- renderUI({
    bi <- F$by_income
    value_box("...at equal density and size",
              paste(pct(bi$adj_share[bi$income_label == "High income"]), "vs",
                    pct(bi$adj_share[bi$income_label == "Low income"])),
              "High vs Low income (all cities, modelled)",
              showcase = bs_icon("arrow-left-right"), theme = "success")
  })
  output$vb_cov <- renderUI({
    cv <- filt() |> summarise(p = mean(has_pharm)) |> pull(p)
    value_box("Cities with pharmacy data", pct(cv),
              "in current selection; the map is thinnest in poorer cities",
              showcase = bs_icon("exclamation-triangle"), theme = "warning")
  })

  # ---- Story charts ----
  output$p_h100 <- renderPlotly({
    d <- inc_summary(); validate(need(nrow(d) > 0, "No cities match these filters."))
    plot_ly(d, x = ~income_label, y = ~h100, type = "bar",
            marker = list(color = unname(PAL_INC[as.character(d$income_label)])),
            text = ~sprintf("%.1f", h100), textposition = "outside",
            hovertemplate = "%{x}<br>%{y:.1f} per 100k<br>n = %{customdata} cities<extra></extra>",
            customdata = ~n) |>
      style_plotly(xaxis = list(title = ""), yaxis = list(title = "Hospitals per 100k"))
  })

  output$p_raw <- renderPlotly({
    d <- inc_summary(); validate(need(nrow(d) > 0, "No cities match these filters."))
    plot_ly(d, x = ~income_label, y = ~share, type = "bar",
            marker = list(color = unname(PAL_INC[as.character(d$income_label)])),
            text = ~pct(share), textposition = "outside",
            hovertemplate = "%{x}<br>%{y:.0%} within 1 km<br>n = %{customdata} cities<extra></extra>",
            customdata = ~n) |>
      style_plotly(xaxis = list(title = ""),
                   yaxis = list(title = "Share within 1 km", tickformat = ".0%",
                                range = c(0, max(d$share) * 1.2)))
  })

  output$p_band <- renderPlotly({
    d <- scored() |>
      filter(income_label %in% INC_LEV) |>
      group_by(density_band, income_label) |>
      summarise(n = n(), share = median(hosp_share_1km), .groups = "drop") |>
      filter(n >= 15)
    validate(need(nrow(d) > 0, "Too few cities for this view."))
    p <- plot_ly()
    for (lv in intersect(INC_LEV, as.character(unique(d$income_label)))) {
      di <- d |> filter(income_label == lv)
      p <- p |> add_trace(data = di, x = ~density_band, y = ~share, type = "scatter",
                          mode = "lines+markers", name = lv,
                          line = list(color = PAL_INC[[lv]], width = 3),
                          marker = list(color = PAL_INC[[lv]], size = 9),
                          customdata = ~n,
                          hovertemplate = paste0(lv, "<br>%{x}: %{y:.0%}<br>n = %{customdata}<extra></extra>"))
    }
    p |> style_plotly(xaxis = list(title = "Density band (sprawling → compact)",
                                   categoryorder = "array",
                                   categoryarray = levels(d$density_band)),
                      yaxis = list(title = "Median share within 1 km", tickformat = ".0%"))
  })

  output$p_density <- renderPlotly({
    d <- scored() |> filter(income_label %in% INC_LEV)
    validate(need(nrow(d) > 0, "No cities match these filters."))
    plot_ly(d, x = ~density, y = ~hosp_share_1km, color = ~income_label,
            colors = PAL_INC, type = "scatter", mode = "markers",
            marker = list(size = 6, opacity = .55),
            text = ~label,
            hovertemplate = "%{text}<br>%{x:,.0f} people/km²<br>%{y:.0%} within 1 km<extra></extra>") |>
      style_plotly(xaxis = list(title = "People per km² (log scale)", type = "log"),
                   yaxis = list(title = "Share within 1 km", tickformat = ".0%"))
  })

  # ---- Beating the odds ----
  output$p_odds <- renderPlotly({
    d <- scored()
    validate(need(nrow(d) > 0, "No cities match these filters."))
    p <- plot_ly(source = "odds")
    for (v in c("As expected", "Beats the odds", "Below expected")) {
      dv <- d |> filter(verdict == v)
      if (nrow(dv) == 0) next
      p <- p |> add_trace(
        data = dv, x = ~expected_model, y = ~hosp_share_1km,
        type = "scatter", mode = "markers", name = v,
        marker = list(color = PAL_VER[[v]], size = ~pmin(4 + sqrt(pop) / 250, 22),
                      opacity = .7, line = list(width = .5, color = "white")),
        customdata = ~id, text = ~label,
        hovertemplate = paste0("<b>%{text}</b><br>Observed %{y:.0%} vs expected %{x:.0%}",
                               "<br>", v, "<extra></extra>"))
    }
    p |>
      add_segments(x = 0, xend = 1, y = 0, yend = 1, inherit = FALSE,
                   line = list(color = "grey", dash = "dash", width = 1),
                   showlegend = FALSE, hoverinfo = "skip") |>
      style_plotly(xaxis = list(title = "Expected share (cities like this one)",
                                tickformat = ".0%", range = c(0, 0.85)),
                   yaxis = list(title = "Observed share within 1 km",
                                tickformat = ".0%", range = c(0, 1))) |>
      event_register("plotly_click")
  })

  # Render the odds plot even while its tab is hidden, so its click event is
  # registered at startup
  outputOptions(output, "p_odds", suspendWhenHidden = FALSE)

  deviant_table <- function(d) {
    reactable(
      d |> transmute(City = label, Observed = hosp_share_1km, Peers = peer_median, Gap = gap_pp),
      compact = TRUE, striped = TRUE, highlight = TRUE, defaultPageSize = 10,
      columns = list(
        City = colDef(minWidth = 170),
        Observed = colDef(format = colFormat(percent = TRUE, digits = 0)),
        Peers    = colDef(name = "Peer median", format = colFormat(percent = TRUE, digits = 0)),
        Gap      = colDef(name = "Gap (pts)", format = colFormat(digits = 0),
                          style = function(v) list(color = if (v >= 0) PAL_VER[["Beats the odds"]]
                                                          else PAL_VER[["Below expected"]],
                                                   fontWeight = 600))
      )
    )
  }
  output$t_top    <- renderReactable(deviant_table(scored() |> filter(pop >= 5e5) |>
                                                     arrange(desc(gap_pp)) |> head(25)))
  output$t_bottom <- renderReactable(deviant_table(scored() |> filter(pop >= 5e5) |>
                                                     arrange(gap_pp) |> head(25)))

  # Click a dot -> open that city
  # (warnings suppressed: the plot may not exist yet while its tab is hidden)
  observeEvent(suppressWarnings(event_data("plotly_click", source = "odds")), {
    ev <- suppressWarnings(event_data("plotly_click", source = "odds"))
    if (!is.null(ev$customdata)) {
      updateSelectizeInput(session, "city", choices = city_choices,
                           selected = ev$customdata, server = TRUE)
      nav_select("main", "city_tab")
    }
  })

  # ---- Your City ----
  # Deep links: ?city=<id> preselects a city, and the URL updates as you search.
  initial_city <- isolate({
    q <- parseQueryString(session$clientData$url_search)
    if (!is.null(q$city) && q$city %in% cities$id) q$city
    else as.character(cities$id[cities$label == "Nairobi, Kenya"][1])
  })
  updateSelectizeInput(session, "city", choices = city_choices,
                       selected = initial_city, server = TRUE)
  if (!is.null(parseQueryString(isolate(session$clientData$url_search))$city)) {
    nav_select("main", "city_tab")
  }

  observeEvent(input$city, {
    req(input$city)
    updateQueryString(paste0("?city=", input$city), mode = "replace")
  })

  observeEvent(input$copy_link, {
    session$sendCustomMessage("copy_url", list())
    showNotification("Link copied to clipboard.", type = "message", duration = 2)
  })

  city <- reactive({
    req(input$city)
    cities |> filter(id == as.integer(input$city))
  })

  peers <- reactive({
    c1 <- city()
    req(!is.na(c1$peer_group))
    frame_all |> filter(has_hosp, !is.na(peer_group), peer_group == c1$peer_group)
  })

  verdict_sentence <- function(c1) {
    nm <- c1$city
    if (!c1$in_frame) {
      return(sprintf("%s has %s residents. Verdicts are only given for centres of %s+ people with an income classification, where rates are stable.",
                     nm, comma(round(c1$pop)), comma(MIN_POP)))
    }
    if (!c1$has_hosp) {
      extra <- if (isTRUE(c1$likely_map_gap))
        " For a city this large, that almost certainly reflects a gap in the open map rather than an absence of hospitals."
      else " It may have no mapped hospitals, one hospital, or simply be unmapped."
      return(paste0("No hospital access data are recorded for ", nm, ".", extra))
    }
    sprintf(
      "%s of %s's residents live within 1 km of a hospital. Among its %s peers (%s income, %s density), the median is %s. That puts %s in the %s percentile: <b>%s</b>.",
      pct(c1$hosp_share_1km), nm, comma(c1$peer_n), tolower(as.character(c1$income_label)),
      tolower(as.character(c1$density_band)), pct(c1$peer_median), nm,
      ordinal(c1$peer_pct), tolower(as.character(c1$verdict))
    )
  }

  output$city_panel <- renderUI({
    c1 <- city()
    vcol <- PAL_VER[[as.character(c1$verdict)]]
    conf_txt <- c(High = "Hospitals and pharmacies both mapped",
                  Medium = "Hospitals mapped; pharmacies missing",
                  Low = "No hospital access data")[[as.character(c1$confidence)]]

    tagList(
      card(
        card_body(
          div(class = "d-flex flex-wrap align-items-center gap-3",
              h3(class = "m-0", c1$city),
              span(class = "text-muted", sprintf("%s · %s · %s people",
                                                 c1$country, c1$region, fmt_pop(c1$pop))),
              span(class = "verdict-badge ms-auto", style = paste0("background:", vcol),
                   as.character(c1$verdict))),
          p(class = "big-sentence mt-3", HTML(verdict_sentence(c1))),
          div(class = "d-flex align-items-center gap-2",
              span(class = "badge", style = paste0("background:", PAL_CONF[[as.character(c1$confidence)]]),
                   paste("Data confidence:", c1$confidence)),
              span(class = "small text-muted", conf_txt))
        )
      ),
      layout_column_wrap(
        width = 1/4, fill = FALSE,
        value_box("Within 1 km of a hospital", pct(c1$hosp_share_1km),
                  showcase = bs_icon("person-walking")),
        value_box("Peer median", pct(c1$peer_median),
                  if (!is.na(c1$peer_n)) paste(c1$peer_n, "similar cities") else "",
                  showcase = bs_icon("people")),
        value_box("Hospitals per 100k", num1(c1$hosp_per_100k),
                  if (!is.na(c1$hosp_n)) paste(c1$hosp_n, "hospitals recorded") else "count not recorded",
                  showcase = bs_icon("building-add")),
        value_box("Within 1 km of a pharmacy", pct(c1$pharm_share_1km),
                  if (!is.na(c1$quadrant)) c1$quadrant else "pharmacy data missing",
                  showcase = bs_icon("capsule"))
      ),
      if (!is.na(c1$peer_group)) card(
        full_screen = TRUE,
        card_header(paste("Where", c1$city, "sits among its peers")),
        plotOutput("p_peers", height = 240),
        caveat("Each tick is one peer city. Shaded band = middle 60% (as expected).")
      )
    )
  })

  peer_plot <- function(c1, pr, base = 13) {
    q <- quantile(pr$hosp_share_1km, c(.2, .5, .8))
    ggplot(pr, aes(x = hosp_share_1km)) +
      annotate("rect", xmin = q[1], xmax = q[3], ymin = -Inf, ymax = Inf,
               fill = "grey90") +
      geom_rug(aes(y = 0), sides = "b", length = unit(1, "npc"), alpha = .35,
               colour = "grey40") +
      geom_vline(xintercept = q[2], colour = "grey30", linetype = "dashed") +
      annotate("label", x = q[2], y = .85, label = "peer median", size = base / 4,
               label.size = 0, fill = "white", colour = "grey30") +
      geom_vline(xintercept = c1$hosp_share_1km, linewidth = 2,
                 colour = PAL_VER[[as.character(c1$verdict)]]) +
      annotate("label", x = c1$hosp_share_1km, y = .5, label = c1$city,
               size = base / 3.5, fontface = "bold", label.size = 0, fill = "white",
               colour = PAL_VER[[as.character(c1$verdict)]]) +
      scale_x_continuous(labels = percent_format(1), limits = c(0, 1)) +
      scale_y_continuous(limits = c(0, 1)) +
      labs(x = "Share of residents within 1 km of a hospital", y = NULL) +
      theme_minimal(base_size = base) +
      theme(axis.text.y = element_blank(), panel.grid = element_blank(),
            axis.line.x = element_line(colour = "grey60"))
  }

  output$p_peers <- renderPlot({
    c1 <- city(); req(!is.na(c1$peer_group))
    peer_plot(c1, peers())
  }, res = 96)

  output$dl_card <- downloadHandler(
    filename = function() paste0("within-reach-", gsub("[^A-Za-z0-9]+", "-", tolower(city()$city)), ".png"),
    content = function(file) {
      c1 <- city()
      body <- gsub("<[^>]+>", "", verdict_sentence(c1))
      if (!is.na(c1$peer_group)) {
        g <- peer_plot(c1, peers(), base = 14)
      } else {
        g <- ggplot() + theme_void()
      }
      g <- g + labs(
        title = paste0(c1$city, ", ", c1$country, ":  ", c1$verdict),
        subtitle = paste(strwrap(body, 95), collapse = "\n"),
        caption = "Within Reach · GHS Urban Centre Database R2024A (EC JRC) · #TidyTuesday · github.com/erickyegon"
      ) + theme(plot.title = element_text(face = "bold", size = 20,
                                          colour = PAL_VER[[as.character(c1$verdict)]]),
                plot.subtitle = element_text(size = 12, lineheight = 1.2, colour = "grey25"),
                plot.caption = element_text(colour = "grey50", size = 9),
                plot.margin = margin(20, 24, 14, 24))
      ggsave(file, g, width = 10, height = 5, dpi = 200, bg = "white")
    }
  )

  # ---- Hospitals vs Pharmacies ----
  output$p_quad <- renderPlotly({
    d <- scored() |> filter(!is.na(quadrant))
    validate(need(nrow(d) > 0, "No cities in this selection have pharmacy data."))
    qm <- app_data$quad_med
    plot_ly(d, x = ~hosp_share_1km, y = ~pharm_share_1km, color = ~income_label,
            colors = PAL_INC, type = "scatter", mode = "markers",
            marker = list(size = 7, opacity = .6), text = ~label, customdata = ~quadrant,
            hovertemplate = "<b>%{text}</b><br>Hospital %{x:.0%} · Pharmacy %{y:.0%}<br>%{customdata}<extra></extra>") |>
      add_segments(x = qm[["hosp"]], xend = qm[["hosp"]], y = 0, yend = 1, inherit = FALSE,
                   line = list(color = "grey", dash = "dot"), showlegend = FALSE, hoverinfo = "skip") |>
      add_segments(x = 0, xend = 1, y = qm[["pharm"]], yend = qm[["pharm"]], inherit = FALSE,
                   line = list(color = "grey", dash = "dot"), showlegend = FALSE, hoverinfo = "skip") |>
      add_annotations(x = c(.02, .98, .02, .98), y = c(.98, .98, .02, .02),
                      text = c("Pharmacy-led", "Well served", "Thin on both", "Hospital-led"),
                      xanchor = c("left", "right", "left", "right"), showarrow = FALSE,
                      font = list(size = 13, color = "#333"),
                      bgcolor = "rgba(255,255,255,0.85)") |>
      style_plotly(xaxis = list(title = "Share within 1 km of a hospital", tickformat = ".0%", range = c(0, 1)),
                   yaxis = list(title = "Share within 1 km of a pharmacy", tickformat = ".0%", range = c(0, 1)))
  })

  output$t_quad <- renderReactable({
    d <- scored() |> filter(!is.na(quadrant)) |>
      count(quadrant, name = "cities") |>
      mutate(share = cities / sum(cities),
             top_income = sapply(quadrant, \(q) {
               x <- scored() |> filter(quadrant == q) |> count(income_label, sort = TRUE)
               as.character(x$income_label[1])
             }))
    reactable(d, compact = TRUE,
              columns = list(quadrant   = colDef(name = "Quadrant", minWidth = 110),
                             cities     = colDef(name = "Cities"),
                             share      = colDef(name = "Share", format = colFormat(percent = TRUE, digits = 0)),
                             top_income = colDef(name = "Most common group", minWidth = 110)))
  })

  # ---- Data gaps & methods ----
  output$p_cov <- renderPlotly({
    d <- F$coverage |> filter(income_label %in% INC_LEV, n >= 5) |>
      mutate(income_label = factor(income_label, levels = INC_LEV))
    plot_ly(d, x = ~income_label, y = ~region, z = ~hosp_cov, type = "heatmap",
            colors = c("#FBEAEA", "#2E4A6B"), zmin = 0, zmax = 1,
            text = ~sprintf("%s cities", n), texttemplate = "%{z:.0%}",
            textfont = list(color = "white"),
            hovertemplate = "%{y}<br>%{x}: %{z:.0%} covered<br>%{text}<extra></extra>",
            colorbar = list(title = "", tickformat = ".0%")) |>
      style_plotly(xaxis = list(title = ""), yaxis = list(title = ""))
  })

  output$t_gaps <- renderReactable({
    reactable(F$map_gaps |> transmute(City = label, Population = pop),
              compact = TRUE, striped = TRUE, defaultPageSize = 8,
              columns = list(City = colDef(minWidth = 170),
                             Population = colDef(format = colFormat(separators = TRUE, digits = 0))))
  })

  output$t_audit <- renderReactable({
    reactable(F$quality_audit, compact = TRUE, wrap = TRUE,
              columns = list(issue = colDef(name = "Issue", minWidth = 90),
                             finding = colDef(name = "Finding", minWidth = 180),
                             implication = colDef(name = "What we did", minWidth = 200)))
  })

  output$t_models <- renderReactable({
    d <- F$model_table |>
      filter(term %in% c("Lower Middle", "Upper Middle", "High income")) |>
      mutate(cell = sprintf("%.2f (%.2f–%.2f)", or, or_lo, or_hi),
             model = sub(":.*", "", model)) |>
      select(Term = term, model, cell) |>
      pivot_wider(names_from = model, values_from = cell)
    reactable(d, compact = TRUE, columns = list(Term = colDef(minWidth = 110)))
  })
}

shinyApp(ui, server)
