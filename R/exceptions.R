#' Build the exceptions log for a reporting period
#'
#' Combines variance breaches/reviews and validation failures/warnings into
#' one table with an empty `commentary` column, ready to be completed by the
#' reporting team and returned with the submission sign-off.
#'
#' @param flagged Output of [flag_variances()].
#' @param validation Output of [run_validation()].
#' @param period Reporting period.
#' @return A tibble, most severe first.
#' @export
exceptions_log <- function(flagged, validation, period) {
  period <- as.Date(period)

  variance <- flagged |>
    dplyr::filter(
      .data$date == period,
      .data$status %in% c("Breach", "Review", "Missing")
    ) |>
    dplyr::transmute(
      source = "Variance",
      severity = dplyr::if_else(.data$status == "Review", "Medium", "High"),
      reference = .data$series_key,
      item = .data$series_label,
      reported = format_measure(.data$value, .data$measure),
      movement = paste0(
        "MoM ", format_change(.data$mom_pct),
        " / YoY ", format_change(.data$yoy_pct)
      ),
      status = as.character(.data$status),
      finding = .data$reason
    )

  rules <- validation |>
    dplyr::filter(.data$date == period, .data$status %in% c("Fail", "Warn")) |>
    dplyr::transmute(
      source = "Validation",
      severity = dplyr::if_else(.data$status == "Fail", "High", "Medium"),
      reference = .data$rule_id,
      item = .data$check,
      reported = dplyr::if_else(
        is.na(.data$actual), NA_character_,
        scales::label_comma(accuracy = 0.01)(.data$actual)
      ),
      movement = format_change(.data$difference_pct),
      status = as.character(.data$status),
      finding = .data$detail
    )

  dplyr::bind_rows(variance, rules) |>
    dplyr::mutate(
      period = period,
      commentary = ""
    ) |>
    dplyr::arrange(
      factor(.data$severity, levels = c("High", "Medium")),
      .data$source, .data$reference
    ) |>
    dplyr::relocate("period")
}
