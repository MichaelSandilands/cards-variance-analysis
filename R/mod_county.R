mod_county_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(
      bslib::layout_columns(
        col_widths = c(6, 6), fill = FALSE,
        shiny::selectInput(
          ns("category"), "Transaction type",
          choices = c("Card payments", "Contactless (POS)",
                      "NFC / mobile wallet (POS)", "ATM cash withdrawals")
        ),
        shiny::radioButtons(ns("measure"), "Measure", inline = TRUE,
                            choices = c("Value", "Volume"))
      ),
      shiny::uiOutput(ns("coverage"))
    ),
    bslib::layout_columns(
      col_widths = c(5, 7),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Year-on-year change by county"),
        plotly::plotlyOutput(ns("growth"), height = "620px", fill = FALSE)
      ),
      bslib::card(
        bslib::card_header("County detail"),
        DT::DTOutput(ns("table"), fill = FALSE)
      )
    )
  )
}

mod_county_server <- function(id, flagged, validation, period) {
  shiny::moduleServer(id, function(input, output, session) {
    rows <- shiny::reactive({
      period_variances(flagged(), period()) |>
        dplyr::filter(
          .data$dataset == "mpcy",
          .data$category == input$category,
          .data$measure == input$measure
        )
    })

    output$coverage <- shiny::renderUI({
      check <- dplyr::filter(
        validation(), .data$rule_id == "VR03", .data$date == period(),
        grepl(input$category, .data$check, fixed = TRUE),
        grepl(paste0("| ", input$measure, "$"), .data$check)
      )
      if (nrow(check) == 0) return(NULL)
      shiny::p(
        class = "mb-0",
        shiny::strong("County allocation (VR03): "),
        shiny::span(as.character(check$status[1]),
                    style = paste0("color:", viz_palette()$status[[as.character(check$status[1])]],
                                   ";font-weight:600")),
        " \u2013 ", check$detail[1]
      )
    })

    output$growth <- plotly::renderPlotly({
      df <- dplyr::filter(rows(), .data$area_code != "_T")
      shiny::validate(shiny::need(nrow(df) > 0, "No county data for this period."))
      plot_county_growth(df)
    })

    output$table <- DT::renderDT({
      df <- rows()
      national <- df$value[df$area_code == "_T"]
      df |>
        dplyr::mutate(share = if (length(national)) .data$value / national else NA_real_) |>
        dplyr::arrange(dplyr::desc(.data$area_code == "_T"), dplyr::desc(.data$value)) |>
        dplyr::transmute(
          County = .data$area,
          Reported = format_measure(.data$value, .data$measure),
          Share = .data$share,
          MoM = format_change(.data$mom_pct),
          YoY = format_change(.data$yoy_pct),
          `SA MoM` = format_change(.data$sa_mom_pct),
          Status = .data$status,
          Reason = .data$reason
        ) |>
        status_table("Status", page_length = 30, dom = "ft") |>
        DT::formatPercentage("Share", digits = 1)
    })
  })
}
