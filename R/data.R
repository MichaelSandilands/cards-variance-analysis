#' Card statistics for Ireland, October 2022 to August 2026
#'
#' The three Central Bank of Ireland card returns combined into one tidy
#' table by [load_cards_data()]. Rebuild with `data-raw/cards_data.R`.
#'
#' @format A tibble with one row per series and reporting period; columns
#'   are described in [tidy_schema()].
#' @source Central Bank of Ireland, card payments statistics: cards in issue
#'   (MPCI), card payments (MPCP) and card payments by county (MPCY).
"cards_data"
