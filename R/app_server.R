app_server <- function(bundled) {
  function(input, output, session) {
    source_data <- shiny::reactiveVal(bundled)

    shiny::observeEvent(input$upload, {
      uploaded <- tryCatch(
        load_cards_data(input$upload$datapath),
        error = function(e) {
          shiny::showNotification(
            paste("Upload rejected:", conditionMessage(e)),
            type = "error", duration = NULL
          )
          NULL
        }
      )
      if (is.null(uploaded)) return()
      replaced <- unique(uploaded$dataset)
      merged <- dplyr::bind_rows(
        dplyr::filter(source_data(), !.data$dataset %in% replaced),
        uploaded
      )
      source_data(merged)
      shiny::showNotification(
        paste("Loaded", paste(toupper(replaced), collapse = ", "),
              "from upload."),
        type = "message"
      )
    })
    shiny::observeEvent(input$reset_data, source_data(bundled))

    shiny::observeEvent(source_data(), {
      periods <- sort(unique(source_data()$date), decreasing = TRUE)
      shiny::updateSelectInput(
        session, "period",
        choices = stats::setNames(as.character(periods), format(periods, "%b %Y")),
        selected = as.character(periods[1])
      )
    })

    period <- shiny::reactive({
      shiny::req(input$period)
      as.Date(input$period)
    })

    alpha <- shiny::debounce(shiny::reactive(input$iqr_alpha), 750)
    metrics <- shiny::reactive({
      shiny::withProgress(message = "Decomposing series (STL)", {
        compute_variance_metrics(source_data(), iqr_alpha = alpha())
      })
    })

    thresholds <- shiny::reactive({
      variance_thresholds(
        mom_pct = input$mom_pct / 100,
        yoy_shift = input$yoy_shift / 100,
        sa_pct = input$sa_pct / 100,
        z = input$z,
        min_pct = input$min_pct / 100,
        materiality = c(
          Value = (input$mat_value %||% 1) * 1e6,
          Volume = (input$mat_volume %||% 100) * 1e3,
          Cards = (input$mat_cards %||% 10) * 1e3
        )
      )
    })

    flagged <- shiny::reactive(flag_variances(metrics(), thresholds()))

    tolerances <- shiny::reactive({
      validation_tolerances(
        reconciliation_pct = input$recon_pct / 100,
        coverage_pct = input$coverage_pct / 100
      )
    })
    validation <- shiny::reactive(run_validation(source_data(), tolerances()))

    mod_overview_server("overview", flagged, validation, period)
    mod_variance_server("variance", flagged, source_data, period)
    mod_diagnostics_server("diagnostics", source_data, period, alpha)
    mod_validation_server("validation", validation, period)
    mod_county_server("county", flagged, validation, period)
    mod_cards_server("cards", source_data, flagged, period)
  }
}
