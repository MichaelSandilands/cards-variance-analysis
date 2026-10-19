# Display formatting helpers.

#' Format observations for display
#'
#' @param x Numeric vector.
#' @param measure `Value` (euro), `Volume` or `Cards`.
#' @return Character vector of short-scale numbers (`"264.7m"`), with a
#'   euro sign prefixed for values.
#' @export
#' @examples
#' format_measure(c(9.716e9, 2.647e8), c("Value", "Volume"))
format_measure <- function(x, measure) {
  short <- scales::label_number(
    accuracy = 0.01, scale_cut = c(0, k = 1e3, m = 1e6, bn = 1e9)
  )
  out <- short(x)
  euro <- measure == "Value"
  out[euro] <- paste0("\u20ac", out[euro])
  out[is.na(x)] <- NA_character_
  out
}

#' Format proportions as signed percentages
#'
#' @param x Numeric vector of proportions.
#' @return Character vector such as `"+4.2%"`.
#' @export
format_change <- function(x) {
  out <- scales::percent(x, accuracy = 0.1, style_positive = "plus")
  out[is.na(x)] <- NA_character_
  out
}
