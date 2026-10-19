mod_variance_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(
      bslib::card_header("Period variance results"),
      bslib::layout_columns(
        col_widths = c(3, 9), fill = FALSE,
        shiny::selectInput(
          ns("dataset"), "Return",
          choices = c("All" = "", MPCI = "mpci", MPCP = "mpcp", MPCY = "mpcy")
        ),
        shiny::checkboxGroupInput(
          ns("status"), "Status", inline = TRUE,
          choices = status_levels(), selected = c("Breach", "Review", "Missing")
        )
      ),
      shiny::p(class = "text-muted small mb-2",
               "Select a row to chart the series and bridge its movement."),
      DT::DTOutput(ns("table"), fill = FALSE)
    ),
    bslib::layout_columns(
      col_widths = c(8, 4),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Reported vs seasonally adjusted"),
        plotly::plotlyOutput(ns("series"), height = "380px", fill = FALSE)
      ),
      bslib::card(
        bslib::card_header("Test results"),
        shiny::tableOutput(ns("tests"))
      )
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        "Value bridge: volume vs average-ticket effect",
        shiny::radioButtons(
          ns("compare"), NULL, inline = TRUE,
          choices = c("vs prior month" = "mom", "vs same month last year" = "yoy")
        )
      ),
      shiny::uiOutput(ns("bridge_note")),
      plotly::plotlyOutput(ns("bridge"), height = "340px", fill = FALSE)
    )
  )
}

mod_variance_server <- function(id, flagged, data, period) {
  shiny::moduleServer(id, function(input, output, session) {
    rows <- shiny::reactive({
      df <- period_variances(flagged(), period())
      if (nzchar(input$dataset)) df <- dplyr::filter(df, .data$dataset == input$dataset)
      if (length(input$status)) df <- dplyr::filter(df, .data$status %in% input$status)
      df
    })

    output$table <- DT::renderDT({
      rows() |>
        dplyr::transmute(
          Series = .data$series_label,
          Reported = format_measure(.data$value, .data$measure),
          MoM = format_change(.data$mom_pct),
          YoY = format_change(.data$yoy_pct),
          `YoY shift (pp)` = round(100 * .data$yoy_shift, 1),
          `SA MoM` = format_change(.data$sa_mom_pct),
          z = round(.data$z_mom, 1),
          Status = .data$status,
          Reason = .data$reason
        ) |>
        status_table("Status", selection = list(mode = "single", selected = 1))
    })

    selected_key <- shiny::reactive({
      i <- input$table_rows_selected
      df <- rows()
      shiny::validate(shiny::need(nrow(df) > 0, "No series match the filters."))
      if (length(i) == 0 || i > nrow(df)) i <- 1
      df$series_key[i]
    })

    output$series <- plotly::renderPlotly(
      plot_series_variance(flagged(), selected_key(), period())
    )

    output$tests <- shiny::renderTable({
      r <- dplyr::filter(flagged(), .data$series_key == selected_key(),
                         .data$date == period())
      tibble::tibble(
        Test = c("Raw MoM (context)", "YoY growth shift", "Seasonally adjusted MoM",
                 "Rolling z-score", "STL anomaly", "Material movement"),
        Result = c(
          format_change(r$mom_pct),
          paste0(sprintf("%+.1f", 100 * r$yoy_shift), "pp"),
          format_change(r$sa_mom_pct),
          sprintf("%+.2f", r$z_mom),
          dplyr::coalesce(r$anomaly, "n/a"),
          ifelse(r$material, "Yes", "No")
        ),
        Breached = c(
          ifelse(
            c(r$breach_mom, r$breach_yoy, r$breach_sa, r$breach_z,
              r$breach_anomaly),
            "\u2716 Yes", "\u2714 No"
          ),
          ""
        )
      )
    }, striped = TRUE, width = "100%")

    bridge <- shiny::reactive({
      key <- dplyr::filter(flagged(), .data$series_key == selected_key())$pair_key[1]
      value_volume_bridge(data(), key, period(), input$compare)
    })

    output$bridge_note <- shiny::renderUI({
      b <- bridge()
      if (is.null(b)) {
        return(shiny::p(class = "text-muted",
          "Bridge needs both value and volume for the series in both periods",
          "(not available for cards in issue or suppressed cells)."))
      }
      shiny::p(
        class = "small",
        sprintf(
          "Value moved %s (%s). Volume contributed %s and average ticket (\u20ac%.2f \u2192 \u20ac%.2f) contributed %s.",
          format_measure(b$total_change, "Value"),
          format_change(b$total_change / b$base_value),
          format_measure(b$volume_effect, "Value"),
          b$base_atv, b$current_atv,
          format_measure(b$atv_effect, "Value")
        )
      )
    })

    output$bridge <- plotly::renderPlotly({
      shiny::req(bridge())
      plot_bridge(bridge())
    })
  })
}
