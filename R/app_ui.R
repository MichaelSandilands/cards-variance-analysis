app_theme <- function() {
  bslib::bs_theme(
    version = 5,
    bg = "#fcfcfb",
    fg = "#0b0b0b",
    primary = "#2a78d6",
    secondary = "#52514e",
    success = "#0ca30c",
    warning = "#fab219",
    danger = "#d03b3b",
    base_font = bslib::font_collection(
      "system-ui", "-apple-system", "Segoe UI", "sans-serif"
    ),
    "border-radius" = "0.5rem"
  ) |>
    bslib::bs_add_rules(c(
      ".dataTables_wrapper { font-size: 0.85rem; }",
      ".dataTables_wrapper .dt-buttons .btn { padding: 0.15rem 0.6rem; }",
      ".value-box-value { font-variant-numeric: tabular-nums; }",
      ".methodology { max-width: 980px; }",
      ".methodology h2 { font-size: 1.4rem; margin-top: 1.5rem; }",
      ".methodology table { margin-bottom: 1rem; width: 100%; }",
      ".methodology th, .methodology td { padding: 0.35rem 0.75rem; border-bottom: 1px solid #e1e0d9; vertical-align: top; }"
    ))
}

app_sidebar <- function() {
  pct_slider <- function(id, label, value, max, step = 0.5) {
    shiny::sliderInput(id, label, min = 0, max = max, value = value,
                       step = step, post = "%")
  }

  bslib::sidebar(
    width = 320,
    shiny::selectInput("period", "Reporting period", choices = NULL),
    bslib::accordion(
      open = "Variance tolerances",
      bslib::accordion_panel(
        "Variance tolerances",
        pct_slider("mom_pct", "Raw MoM (context only)", 10, 50),
        pct_slider("yoy_shift", "YoY growth shift (pp vs 3m trend)", 10, 50),
        pct_slider("sa_pct", "Seasonally adjusted MoM", 5, 30),
        shiny::sliderInput("z", "Rolling z-score", min = 1, max = 6,
                           value = 3, step = 0.5),
        pct_slider("min_pct", "Min. move for z / anomaly tests", 2, 10, 0.5),
        shiny::numericInput("mat_value", "Materiality: value (EUR m)",
                            value = 1, min = 0, step = 0.5),
        shiny::numericInput("mat_volume", "Materiality: volume (thousand)",
                            value = 100, min = 0, step = 10),
        shiny::numericInput("mat_cards", "Materiality: cards (thousand)",
                            value = 10, min = 0, step = 5)
      ),
      bslib::accordion_panel(
        "Model settings",
        shiny::sliderInput(
          "iqr_alpha", "Anomaly sensitivity (IQR alpha)",
          min = 0.01, max = 0.20, value = 0.05, step = 0.01
        ),
        shiny::helpText(
          "Higher alpha narrows the timetk::anomalize() band and flags more",
          "points. Changing it re-runs the STL decomposition."
        )
      ),
      bslib::accordion_panel(
        "Validation tolerances",
        shiny::sliderInput(
          "recon_pct", "Cross-return tolerance", min = 0, max = 0.5,
          value = 0.05, step = 0.01, post = "%"
        ),
        pct_slider("coverage_pct", "Min. county coverage", 98, 100, 0.5)
      ),
      bslib::accordion_panel(
        "Data source",
        shiny::fileInput(
          "upload", "Upload draft extract(s)", multiple = TRUE,
          accept = ".csv"
        ),
        shiny::helpText(
          "MPCI, MPCP or MPCY CSVs in the CBI layout. The return type is",
          "detected from the header. Uploaded files replace the bundled",
          "data for their return."
        ),
        shiny::actionButton("reset_data", "Use bundled data",
                            class = "btn-sm btn-outline-secondary")
      )
    )
  )
}

app_ui <- function() {
  bslib::page_navbar(
    title = "Card Payments Variance Analysis",
    theme = app_theme(),
    sidebar = app_sidebar(),
    fillable = FALSE,
    bslib::nav_panel("Overview", mod_overview_ui("overview")),
    bslib::nav_panel("Variance analysis", mod_variance_ui("variance")),
    bslib::nav_panel("Time-series diagnostics", mod_diagnostics_ui("diagnostics")),
    bslib::nav_panel("Validation rules", mod_validation_ui("validation")),
    bslib::nav_panel("County breakdown", mod_county_ui("county")),
    bslib::nav_panel("Cards in issue", mod_cards_ui("cards")),
    bslib::nav_panel("Methodology", mod_methodology_ui())
  )
}
