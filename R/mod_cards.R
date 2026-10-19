mod_cards_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::p(
      class = "text-muted",
      "Ratios across returns are a strong plausibility check: a step change in",
      "reported volumes that coincides with a proportionate step in cards in",
      "issue (as in March 2023) points to a change in the reporting population",
      "rather than a data error."
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(full_screen = TRUE,
                  plotly::plotlyOutput(ns("per_card"), height = "360px", fill = FALSE)),
      bslib::card(full_screen = TRUE,
                  plotly::plotlyOutput(ns("cards"), height = "360px", fill = FALSE))
    ),
    bslib::card(
      bslib::card_header("Cards in issue: variance results"),
      DT::DTOutput(ns("table"), fill = FALSE)
    )
  )
}

mod_cards_server <- function(id, data, flagged, period) {
  shiny::moduleServer(id, function(input, output, session) {
    output$per_card <- plotly::renderPlotly({
      ppc <- payments_per_card(data())
      shiny::validate(shiny::need(nrow(ppc) > 0, "Needs both MPCI and MPCP data."))
      plot_per_card(ppc)
    })

    output$cards <- plotly::renderPlotly({
      keys <- unique(dplyr::filter(data(), .data$dataset == "mpci")$series_key)
      shiny::validate(shiny::need(length(keys) > 0, "No MPCI data loaded."))
      plot_series_variance(flagged(), keys[1], period())
    })

    output$table <- DT::renderDT({
      flagged() |>
        dplyr::filter(.data$dataset == "mpci", .data$date <= period()) |>
        dplyr::arrange(dplyr::desc(.data$date)) |>
        dplyr::transmute(
          Period = format(.data$date, "%b %Y"),
          Series = .data$category,
          Cards = format_measure(.data$value, .data$measure),
          MoM = format_change(.data$mom_pct),
          YoY = format_change(.data$yoy_pct),
          z = round(.data$z_mom, 1),
          Status = .data$status,
          Reason = .data$reason
        ) |>
        status_table("Status", page_length = 10)
    })
  })
}
