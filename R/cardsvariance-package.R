#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data %||%
## usethis namespace: end
NULL

# Bare column names used inside timetk calls, which evaluate them as data
# masks rather than through the .data pronoun.
utils::globalVariables(c("date", "value"))
