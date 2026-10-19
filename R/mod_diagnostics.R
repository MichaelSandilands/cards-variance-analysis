mod_diagnostics_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(
      bslib::layout_columns(
        col_widths = c(8, 4), fill = FALSE,
        shiny::selectizeInput(ns("series"), "Series", choices = NULL,
                              width = "100%"),
        shiny::radioButtons(
          ns("scale"), "Decomposition scale", inline = TRUE,
          choices = c("Log (multiplicative)" = "log", "Level (additive)" = "level")
        )
      ),
      shiny::p(
        class = "text-muted small mb-0",
        "Native timetk diagnostics for the selected series. Confidential",
        "gaps are imputed with ts_impute_vec() before decomposition."
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(full_screen = TRUE,
                  bslib::card_header("Anomalies: timetk::plot_anomalies()"),
                  plotly::plotlyOutput(ns("anomalies"), height = "460px", fill = FALSE)),
      bslib::card(full_screen = TRUE,
                  bslib::card_header("STL decomposition: plot_stl_diagnostics()"),
                  plotly::plotlyOutput(ns("stl"), height = "460px", fill = FALSE))
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(full_screen = TRUE,
                  bslib::card_header("Seasonality: plot_seasonal_diagnostics()"),
                  plotly::plotlyOutput(ns("seasonal"), height = "460px", fill = FALSE)),
      bslib::card(full_screen = TRUE,
                  bslib::card_header("Autocorrelation: plot_acf_diagnostics()"),
                  plotly::plotlyOutput(ns("acf"), height = "460px", fill = FALSE))
    )
  )
}

mod_diagnostics_server <- function(id, data, period, alpha) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(data(), {
      catalogue <- dplyr::distinct(data(), .data$series_key, .data$series_label)
      default <- "PCP.M.IE._Z.W0.CP0.1._T._T.PCS_ALL._Z._X._Z.N.EUR"
      shiny::updateSelectizeInput(
        session, "series", server = TRUE,
        choices = stats::setNames(catalogue$series_key, catalogue$series_label),
        selected = if (default %in% catalogue$series_key) default else catalogue$series_key[1]
      )
    })

    series <- shiny::reactive({
      shiny::req(input$series)
      df <- data() |>
        dplyr::filter(.data$series_key == input$series) |>
        dplyr::filter(.data$date <= period()) |>
        dplyr::arrange(.data$date) |>
        dplyr::select("date", "value")
      shiny::validate(shiny::need(
        sum(!is.na(df$value)) >= 24,
        "Not enough unsuppressed history (24 months) to decompose this series."
      ))
      df$value <- timetk::ts_impute_vec(df$value, period = 12)
      if (input$scale == "log" && all(df$value > 0)) df$value <- log(df$value)
      df
    })

    y_title <- shiny::reactive(if (input$scale == "log") "log(value)" else "value")

    output$anomalies <- plotly::renderPlotly({
      series() |>
        timetk::anomalize(date, value, .frequency = 12, .trend = 12,
                          .iqr_alpha = alpha(), .message = FALSE) |>
        timetk::plot_anomalies(date, .interactive = TRUE, .title = "",
                               .y_lab = y_title(), .legend_show = TRUE)
    })

    output$stl <- plotly::renderPlotly({
      timetk::plot_stl_diagnostics(
        series(), date, value, .frequency = 12, .trend = 12,
        .feature_set = c("observed", "season", "trend", "remainder"),
        .interactive = TRUE, .title = "", .message = FALSE
      )
    })

    output$seasonal <- plotly::renderPlotly({
      timetk::plot_seasonal_diagnostics(
        series(), date, value, .feature_set = c("month.lbl", "quarter", "year"),
        .interactive = TRUE, .title = ""
      )
    })

    output$acf <- plotly::renderPlotly({
      timetk::plot_acf_diagnostics(series(), date, value, .lags = 24,
                                   .interactive = TRUE, .title = "")
    })
  })
}
