#' Launch the variance analysis app
#'
#' @param data Optional tidy data (see [tidy_schema()]). Defaults to the
#'   bundled [cards_data]. Users can also upload draft extracts in the app.
#' @param ... Passed to [shiny::shinyApp()] `options`, e.g. `port`.
#' @return A `shiny.appobj`.
#' @export
#' @examples
#' if (interactive()) run_app()
run_app <- function(data = NULL, ...) {
  if (is.null(data)) data <- cardsvariance::cards_data
  shiny::shinyApp(
    ui = app_ui(),
    server = app_server(data),
    options = list(...)
  )
}
