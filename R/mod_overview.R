status_table <- function(df, status_col = "status", page_length = 10,
                         selection = "none", filter = "top", ...) {
  pal <- viz_palette()$status
  DT::datatable(
    df,
    rownames = FALSE,
    style = "bootstrap5",
    class = "table table-sm table-hover",
    selection = selection,
    filter = filter,
    extensions = "Buttons",
    options = utils::modifyList(
      list(
        pageLength = page_length, scrollX = TRUE, dom = "Bfrtip",
        buttons = c("copy", "csv"), autoWidth = FALSE
      ),
      list(...)
    )
  ) |>
    DT::formatStyle(
      status_col,
      backgroundColor = DT::styleEqual(names(pal), paste0(pal, "33")),
      borderLeft = DT::styleEqual(names(pal), paste0("4px solid ", pal)),
      fontWeight = "600"
    )
}

mod_overview_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::layout_column_wrap(
      width = 1 / 4, fill = FALSE,
      bslib::value_box("Series assessed", shiny::textOutput(ns("n_series")),
                       shiny::textOutput(ns("n_suppressed"), inline = TRUE)),
      bslib::value_box("Variance breaches", shiny::textOutput(ns("n_breach")),
                       "two or more independent tests failed",
                       theme = bslib::value_box_theme(bg = "#d03b3b", fg = "#fff")),
      bslib::value_box("For review", shiny::textOutput(ns("n_review")),
                       "one independent test failed",
                       theme = bslib::value_box_theme(bg = "#fab219", fg = "#0b0b0b")),
      bslib::value_box("Validation exceptions", shiny::textOutput(ns("n_rules")),
                       shiny::textOutput(ns("n_rules_detail"), inline = TRUE),
                       theme = bslib::value_box_theme(bg = "#2a78d6", fg = "#fff"))
    ),
    bslib::card(
      bslib::card_header("Variance exceptions by reporting period"),
      plotly::plotlyOutput(ns("history"), height = "280px", fill = FALSE)
    ),
    bslib::card(
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        shiny::textOutput(ns("log_title"), inline = TRUE),
        shiny::downloadButton(ns("download"), "Download exceptions log",
                              class = "btn-sm btn-primary")
      ),
      shiny::p(
        class = "text-muted small mb-2",
        "Every variance flag and rule exception for the period, with a blank",
        "commentary column to complete before sign-off."
      ),
      DT::DTOutput(ns("log"), fill = FALSE)
    )
  )
}

mod_overview_server <- function(id, flagged, validation, period) {
  shiny::moduleServer(id, function(input, output, session) {
    current <- shiny::reactive(period_variances(flagged(), period()))
    log <- shiny::reactive(exceptions_log(flagged(), validation(), period()))
    rule_exc <- shiny::reactive({
      dplyr::filter(validation(), .data$date == period(),
                    .data$status %in% c("Fail", "Warn"))
    })

    output$n_series <- shiny::renderText(sum(!is.na(current()$value)))
    output$n_suppressed <- shiny::renderText(
      paste(sum(current()$status == "Not assessed"), "suppressed as confidential")
    )
    output$n_breach <- shiny::renderText(sum(current()$status == "Breach"))
    output$n_review <- shiny::renderText(sum(current()$status == "Review"))
    output$n_rules <- shiny::renderText(nrow(rule_exc()))
    output$n_rules_detail <- shiny::renderText(paste(
      sum(rule_exc()$status == "Fail"), "fail /",
      sum(rule_exc()$status == "Warn"), "warn across",
      nrow(validation_rules()), "rules"
    ))
    output$history <- plotly::renderPlotly(plot_status_history(flagged()))
    output$log_title <- shiny::renderText(
      paste("Exceptions log:", format(period(), "%B %Y"))
    )
    output$log <- DT::renderDT(
      log() |>
        dplyr::select(-"period", -"commentary") |>
        dplyr::rename_with(tools::toTitleCase) |>
        status_table("Status")
    )
    output$download <- shiny::downloadHandler(
      filename = function() {
        paste0("exceptions_log_", format(period(), "%Y%m"), ".csv")
      },
      content = function(file) readr::write_csv(log(), file, na = "")
    )
  })
}
