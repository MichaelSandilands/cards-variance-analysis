mod_validation_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(
      bslib::card_header(shiny::textOutput(ns("summary_title"), inline = TRUE)),
      DT::DTOutput(ns("summary"), fill = FALSE)
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Exceptions by rule over time"),
      plotly::plotlyOutput(ns("heatmap"), height = "340px", fill = FALSE)
    ),
    bslib::card(
      bslib::card_header("Rule results"),
      bslib::layout_columns(
        col_widths = c(4, 4, 4), fill = FALSE,
        shiny::selectInput(ns("rule"), "Rule", choices = NULL),
        shiny::checkboxGroupInput(
          ns("status"), "Status", inline = TRUE,
          choices = validation_status_levels(), selected = c("Fail", "Warn")
        ),
        shiny::checkboxInput(ns("all_periods"), "Show all periods", FALSE)
      ),
      DT::DTOutput(ns("detail"), fill = FALSE)
    )
  )
}

mod_validation_server <- function(id, validation, period) {
  shiny::moduleServer(id, function(input, output, session) {
    rules <- validation_rules()
    shiny::updateSelectInput(
      session, "rule",
      choices = c("All rules" = "", stats::setNames(rules$rule_id,
                                                    paste(rules$rule_id, rules$rule)))
    )

    output$summary_title <- shiny::renderText(
      paste("Rule summary:", format(period(), "%B %Y"))
    )

    output$summary <- DT::renderDT({
      s <- summarise_validation(validation(), period())
      s$Result <- dplyr::case_when(
        s$Fail > 0 ~ "Fail",
        s$Warn > 0 ~ "Warn",
        s$Pass > 0 ~ "Pass",
        TRUE ~ "Not assessed"
      )
      s |>
        dplyr::select("rule_id", "rule", "description", "Result",
                      dplyr::any_of(validation_status_levels())) |>
        dplyr::rename(ID = "rule_id", Rule = "rule", Description = "description") |>
        status_table("Result", page_length = 10, filter = "none", dom = "t")
    })

    output$heatmap <- plotly::renderPlotly(plot_validation_heatmap(validation()))

    output$detail <- DT::renderDT({
      df <- validation()
      if (!isTRUE(input$all_periods)) df <- dplyr::filter(df, .data$date == period())
      if (nzchar(input$rule %||% "")) df <- dplyr::filter(df, .data$rule_id == input$rule)
      if (length(input$status)) df <- dplyr::filter(df, .data$status %in% input$status)
      df |>
        dplyr::transmute(
          Period = format(.data$date, "%b %Y"),
          ID = .data$rule_id,
          Check = .data$check,
          Expected = .data$expected,
          Actual = .data$actual,
          Difference = .data$difference,
          `Diff %` = .data$difference_pct,
          Status = .data$status,
          Detail = .data$detail
        ) |>
        status_table("Status", page_length = 15) |>
        DT::formatRound(c("Expected", "Actual", "Difference"), digits = 2) |>
        DT::formatPercentage("Diff %", digits = 3)
    })
  })
}
