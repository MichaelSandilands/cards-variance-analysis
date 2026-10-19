mod_methodology_ui <- function() {
  bslib::card(
    class = "p-3 methodology",
    shiny::HTML(markdown::mark_html(
      system.file("app", "methodology.md", package = "cardsvariance"),
      template = FALSE
    ))
  )
}
