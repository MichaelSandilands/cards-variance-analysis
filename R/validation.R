# Pre-submission validation rules.
#
# Every rule returns the same columns (see `validation_columns()`) so results
# can be stacked, summarised and exported as one exceptions log.

#' Default validation tolerances
#'
#' @param hierarchy_abs Absolute tolerance for additive identities (rounding).
#' @param reconciliation_pct Relative tolerance between returns that should
#'   report the same population.
#' @param coverage_pct Minimum share of a national total that must be
#'   allocated to counties before an unexplained gap is flagged.
#' @param atv_bands Plausible average transaction value (EUR) by category.
#' @param atv_move_pct Tolerated year-on-year change in average value.
#'   Compared year on year because average ticket is seasonal (January dips).
#' @param per_card_band Plausible monthly card payments per card in issue.
#' @param per_card_yoy_pct Tolerated year-on-year change in payments per
#'   card. Compared year on year because card usage is seasonal.
#' @return A named list.
#' @export
validation_tolerances <- function(hierarchy_abs = 1,
                                  reconciliation_pct = 0.0005,
                                  coverage_pct = 0.98,
                                  atv_bands = list(
                                    `Card payments` = c(10, 150),
                                    `Contactless (POS)` = c(5, 50),
                                    `NFC / mobile wallet (POS)` = c(5, 60),
                                    `ATM cash withdrawals` = c(50, 300)
                                  ),
                                  atv_move_pct = 0.10,
                                  per_card_band = c(10, 60),
                                  per_card_yoy_pct = 0.15) {
  list(
    hierarchy_abs = hierarchy_abs,
    reconciliation_pct = reconciliation_pct,
    coverage_pct = coverage_pct,
    atv_bands = atv_bands,
    atv_move_pct = atv_move_pct,
    per_card_band = per_card_band,
    per_card_yoy_pct = per_card_yoy_pct
  )
}

#' Catalogue of validation rules
#' @return A tibble describing each rule.
#' @export
validation_rules <- function() {
  tibble::tribble(
    ~rule_id, ~rule, ~description,
    "VR01", "Geographic hierarchy",
    "MPCP: all locations (W0) equals domestic (W2) plus abroad (W1).",
    "VR02", "Channel hierarchy",
    "MPCP: all channels equals remote plus non-remote.",
    "VR03", "County allocation",
    "MPCY: counties sum to no more than the national total; unexplained gaps flagged.",
    "VR04", "Sub-category bounds",
    "MPCY: NFC <= contactless <= all card payments, per county.",
    "VR05", "Cross-return reconciliation",
    "MPCY national total equals MPCP domestic (W2) for the same transactions.",
    "VR06", "Average transaction value",
    "Value / volume within a plausible band and stable year on year.",
    "VR07", "Completeness & confidentiality",
    "Every series reports every period; NA only where flagged confidential.",
    "VR08", "Sign check",
    "No negative values or volumes.",
    "VR09", "Payments per card",
    "MPCP payments by Irish cards / MPCI cards in issue within a plausible band."
  )
}

validation_columns <- function() {
  c(
    "rule_id", "rule", "dataset", "date", "check", "expected", "actual",
    "difference", "difference_pct", "status", "detail"
  )
}

#' Run every validation rule
#'
#' @param data Tidy data from [load_cards_data()].
#' @param tolerances Output of [validation_tolerances()].
#' @return A tibble of rule outcomes with `status` one of `Fail`, `Warn`,
#'   `Pass` or `Not assessed`.
#' @export
run_validation <- function(data, tolerances = validation_tolerances()) {
  rules <- list(
    check_geographic_hierarchy,
    check_channel_hierarchy,
    check_county_allocation,
    check_subcategory_bounds,
    check_cross_return,
    check_average_value,
    check_completeness,
    check_signs,
    check_payments_per_card
  )
  purrr::map(rules, ~ .x(data, tolerances)) |>
    purrr::list_rbind() |>
    dplyr::mutate(
      status = factor(.data$status, levels = validation_status_levels())
    ) |>
    dplyr::arrange(.data$status, .data$rule_id, dplyr::desc(.data$date))
}

#' Ordered validation status levels
#' @return Character vector.
#' @export
validation_status_levels <- function() {
  c("Fail", "Warn", "Pass", "Not assessed")
}

# Generic additive identity: parent == sum(children) within `by` groups.
check_additive <- function(df, dim, parent, children, by, rule_id, rule,
                           label_fn, tol) {
  wide <- df |>
    dplyr::filter(.data[[dim]] %in% c(parent, children)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::summarise(
      expected = sum(.data$value[.data[[dim]] == parent]),
      actual = sum(.data$value[.data[[dim]] %in% children]),
      n_children = sum(.data[[dim]] %in% children),
      .groups = "drop"
    )

  wide |>
    dplyr::mutate(
      rule_id = rule_id,
      rule = rule,
      check = label_fn(wide),
      difference = .data$actual - .data$expected,
      difference_pct = safe_pct(.data$difference, .data$expected),
      status = dplyr::case_when(
        is.na(.data$difference) ~ "Not assessed",
        .data$n_children < length(children) ~ "Fail",
        abs(.data$difference) <= tol ~ "Pass",
        TRUE ~ "Fail"
      ),
      detail = dplyr::case_when(
        .data$status == "Not assessed" ~ "Component suppressed (confidential)",
        .data$n_children < length(children) ~ "Component series missing",
        .data$status == "Fail" ~ "Components do not sum to total",
        TRUE ~ "Identity holds"
      )
    ) |>
    finalise_rule()
}

finalise_rule <- function(df) {
  df |>
    dplyr::mutate(date = as.Date(.data$date)) |>
    dplyr::select(dplyr::all_of(validation_columns()))
}

#' @rdname run_validation
#' @export
check_geographic_hierarchy <- function(data, tolerances = validation_tolerances()) {
  data |>
    dplyr::filter(.data$dataset == "mpcp") |>
    check_additive(
      dim = "area_code", parent = "W0", children = c("W1", "W2"),
      by = c("dataset", "date", "category", "channel", "measure"),
      rule_id = "VR01", rule = "Geographic hierarchy",
      label_fn = function(w) {
        paste("W0 = W1 + W2", w$category, w$channel, w$measure, sep = " | ")
      },
      tol = tolerances$hierarchy_abs
    )
}

#' @rdname run_validation
#' @export
check_channel_hierarchy <- function(data, tolerances = validation_tolerances()) {
  data |>
    dplyr::filter(.data$dataset == "mpcp", .data$category_code == "CP0") |>
    check_additive(
      dim = "channel_code", parent = "_T", children = c("NR", "R"),
      by = c("dataset", "date", "area", "category", "measure"),
      rule_id = "VR02", rule = "Channel hierarchy",
      label_fn = function(w) {
        paste("Total = Remote + Non-remote", w$area, w$measure, sep = " | ")
      },
      tol = tolerances$hierarchy_abs
    )
}

#' @rdname run_validation
#' @export
check_county_allocation <- function(data, tolerances = validation_tolerances()) {
  data |>
    dplyr::filter(.data$dataset == "mpcy") |>
    dplyr::group_by(.data$dataset, .data$date, .data$category, .data$measure) |>
    dplyr::summarise(
      expected = sum(.data$value[.data$area_code == "_T"]),
      actual = sum(.data$value[.data$area_code != "_T"], na.rm = TRUE),
      suppressed = sum(.data$confidential & .data$area_code != "_T"),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      rule_id = "VR03",
      rule = "County allocation",
      check = paste("Sum of counties <= national", .data$category,
        .data$measure,
        sep = " | "
      ),
      difference = .data$actual - .data$expected,
      difference_pct = safe_pct(.data$difference, .data$expected),
      coverage = .data$actual / .data$expected,
      status = dplyr::case_when(
        is.na(.data$expected) ~ "Not assessed",
        .data$difference > tolerances$hierarchy_abs ~ "Fail",
        abs(.data$difference) <= tolerances$hierarchy_abs ~ "Pass",
        .data$suppressed > 0 ~ "Pass",
        .data$coverage < tolerances$coverage_pct ~ "Fail",
        TRUE ~ "Warn"
      ),
      detail = dplyr::case_when(
        .data$status == "Not assessed" ~ "National total suppressed",
        .data$difference > tolerances$hierarchy_abs ~
          "Counties exceed national total",
        abs(.data$difference) <= tolerances$hierarchy_abs ~ "Fully allocated",
        .data$suppressed > 0 ~ paste0(
          scales::percent(.data$coverage, 0.01), " allocated; gap explained by ",
          .data$suppressed, " suppressed count",
          ifelse(.data$suppressed == 1, "y", "ies")
        ),
        TRUE ~ paste0(
          scales::percent(.data$coverage, 0.01),
          " allocated with no suppressed counties: unallocated residual"
        )
      )
    ) |>
    finalise_rule()
}

#' @rdname run_validation
#' @export
check_subcategory_bounds <- function(data, tolerances = validation_tolerances()) {
  wide <- data |>
    dplyr::filter(.data$dataset == "mpcy") |>
    dplyr::select("date", "area", "measure", "category_code", "value") |>
    tidyr::pivot_wider(names_from = "category_code", values_from = "value")

  pairs <- list(c(child = "CP11", parent = "CP1"), c(child = "CP1", parent = "CP0"))
  purrr::map(pairs, function(p) {
    wide |>
      dplyr::transmute(
        dataset = "mpcy",
        date = .data$date,
        rule_id = "VR04",
        rule = "Sub-category bounds",
        check = paste(
          label_transaction(p[["child"]]), "<=", label_transaction(p[["parent"]]),
          .data$area, .data$measure,
          sep = " | "
        ),
        expected = .data[[p[["parent"]]]],
        actual = .data[[p[["child"]]]],
        difference = .data$actual - .data$expected,
        difference_pct = safe_pct(.data$difference, .data$expected),
        status = dplyr::case_when(
          is.na(.data$difference) ~ "Not assessed",
          .data$difference > tolerances$hierarchy_abs ~ "Fail",
          TRUE ~ "Pass"
        ),
        detail = dplyr::case_when(
          .data$status == "Not assessed" ~ "Suppressed (confidential)",
          .data$status == "Fail" ~ "Sub-category exceeds its parent",
          TRUE ~ "Within parent"
        )
      )
  }) |>
    purrr::list_rbind() |>
    finalise_rule()
}

#' @rdname run_validation
#' @export
check_cross_return <- function(data, tolerances = validation_tolerances()) {
  mpcy <- data |>
    dplyr::filter(
      .data$dataset == "mpcy", .data$area_code == "_T",
      .data$category_code %in% c("CP0", "CW1")
    ) |>
    dplyr::select("date", "category", "measure", county_total = "value")
  mpcp <- data |>
    dplyr::filter(
      .data$dataset == "mpcp", .data$area_code == "W2",
      .data$channel_code == "_T", .data$category_code %in% c("CP0", "CW1")
    ) |>
    dplyr::select("date", "category", "measure", domestic = "value")

  dplyr::inner_join(mpcp, mpcy, by = c("date", "category", "measure")) |>
    dplyr::mutate(
      dataset = "mpcp/mpcy",
      rule_id = "VR05",
      rule = "Cross-return reconciliation",
      check = paste("MPCY national = MPCP domestic", .data$category,
        .data$measure,
        sep = " | "
      ),
      expected = .data$domestic,
      actual = .data$county_total,
      difference = .data$actual - .data$expected,
      difference_pct = safe_pct(.data$difference, .data$expected),
      status = dplyr::case_when(
        is.na(.data$difference) ~ "Not assessed",
        abs(.data$difference) <= tolerances$hierarchy_abs ~ "Pass",
        abs(.data$difference_pct) <= tolerances$reconciliation_pct ~ "Warn",
        TRUE ~ "Fail"
      ),
      detail = dplyr::case_when(
        .data$status == "Not assessed" ~ "Suppressed (confidential)",
        .data$status == "Pass" ~ "Returns agree",
        .data$status == "Warn" ~ paste0(
          "Within ", scales::percent(tolerances$reconciliation_pct, 0.01),
          " tolerance; confirm revision timing between returns"
        ),
        TRUE ~ paste0(
          "Outside ", scales::percent(tolerances$reconciliation_pct, 0.01),
          " tolerance; investigate"
        )
      )
    ) |>
    finalise_rule()
}

#' Average transaction value for every value/volume pair
#'
#' @param data Tidy data from [load_cards_data()].
#' @return A tibble with `atv`, `atv_mom_pct` and `atv_yoy_pct` per pair and
#'   period.
#' @export
average_values <- function(data) {
  data |>
    dplyr::filter(.data$measure %in% c("Value", "Volume")) |>
    dplyr::select(
      "dataset", "pair_key", "date", "area", "category", "channel",
      "measure", "value"
    ) |>
    tidyr::pivot_wider(names_from = "measure", values_from = "value") |>
    dplyr::arrange(.data$pair_key, .data$date) |>
    dplyr::mutate(atv = safe_pct(.data$Value, .data$Volume)) |>
    dplyr::group_by(.data$pair_key) |>
    dplyr::mutate(
      atv_mom_pct = safe_pct(.data$atv - dplyr::lag(.data$atv), dplyr::lag(.data$atv)),
      atv_yoy_pct = safe_pct(
        .data$atv - dplyr::lag(.data$atv, 12), dplyr::lag(.data$atv, 12)
      )
    ) |>
    dplyr::ungroup()
}

#' @rdname run_validation
#' @export
check_average_value <- function(data, tolerances = validation_tolerances()) {
  bands <- tolerances$atv_bands
  average_values(data) |>
    dplyr::mutate(
      lower = purrr::map_dbl(.data$category, ~ bands[[.x]][1] %||% NA_real_),
      upper = purrr::map_dbl(.data$category, ~ bands[[.x]][2] %||% NA_real_),
      rule_id = "VR06",
      rule = "Average transaction value",
      check = paste("ATV", .data$area, .data$category,
        dplyr::coalesce(.data$channel, ""),
        sep = " | "
      ),
      expected = (.data$lower + .data$upper) / 2,
      actual = .data$atv,
      difference = NA_real_,
      difference_pct = .data$atv_yoy_pct,
      out_of_band = .data$atv < .data$lower | .data$atv > .data$upper,
      moved = abs(.data$atv_yoy_pct) > tolerances$atv_move_pct,
      status = dplyr::case_when(
        is.na(.data$atv) ~ "Not assessed",
        .data$out_of_band %in% TRUE ~ "Fail",
        .data$moved %in% TRUE ~ "Warn",
        TRUE ~ "Pass"
      ),
      detail = dplyr::case_when(
        .data$status == "Not assessed" ~ "Value or volume suppressed",
        .data$status == "Fail" ~ sprintf(
          "ATV EUR %.2f outside EUR %.0f-%.0f band", .data$atv, .data$lower,
          .data$upper
        ),
        .data$status == "Warn" ~ sprintf(
          "ATV EUR %.2f moved %s YoY", .data$atv,
          scales::percent(.data$atv_yoy_pct, 0.1, style_positive = "plus")
        ),
        TRUE ~ sprintf("ATV EUR %.2f", .data$atv)
      )
    ) |>
    finalise_rule()
}

#' @rdname run_validation
#' @export
check_completeness <- function(data, tolerances = validation_tolerances()) {
  padded <- complete_periods(data)
  duplicates <- data |>
    dplyr::count(.data$dataset, .data$series_key, .data$date) |>
    dplyr::filter(.data$n > 1)

  issues <- padded |>
    dplyr::mutate(
      problem = dplyr::case_when(
        .data$missing_row ~ "Period not reported",
        is.na(.data$value) & !.data$confidential ~
          "Blank value without confidentiality flag",
        !is.na(.data$value) & .data$confidential ~
          "Value populated on a confidential cell",
        is.na(.data$value) & .data$confidential ~ "Suppressed (confidential)",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(!is.na(.data$problem)) |>
    dplyr::transmute(
      rule_id = "VR07",
      rule = "Completeness & confidentiality",
      dataset = .data$dataset,
      date = .data$date,
      check = .data$series_label,
      expected = NA_real_,
      actual = .data$value,
      difference = NA_real_,
      difference_pct = NA_real_,
      status = dplyr::if_else(
        .data$problem == "Suppressed (confidential)", "Not assessed", "Fail"
      ),
      detail = .data$problem
    )

  dupes <- duplicates |>
    dplyr::transmute(
      rule_id = "VR07", rule = "Completeness & confidentiality",
      dataset = .data$dataset, date = .data$date, check = .data$series_key,
      expected = 1, actual = .data$n, difference = .data$n - 1,
      difference_pct = NA_real_, status = "Fail",
      detail = "Duplicate observation for series and period"
    )

  summary <- padded |>
    dplyr::group_by(.data$dataset, .data$date) |>
    dplyr::summarise(
      expected = dplyr::n(),
      actual = sum(!.data$missing_row),
      .groups = "drop"
    ) |>
    dplyr::transmute(
      rule_id = "VR07", rule = "Completeness & confidentiality",
      dataset = .data$dataset, date = .data$date,
      check = paste(toupper(.data$dataset), "series reported"),
      expected = .data$expected, actual = .data$actual,
      difference = .data$actual - .data$expected,
      difference_pct = safe_pct(.data$difference, .data$expected),
      status = dplyr::if_else(.data$difference == 0, "Pass", "Fail"),
      detail = paste(.data$actual, "of", .data$expected, "series present")
    )

  dplyr::bind_rows(summary, issues, dupes) |> finalise_rule()
}

#' @rdname run_validation
#' @export
check_signs <- function(data, tolerances = validation_tolerances()) {
  data |>
    dplyr::group_by(.data$dataset, .data$date) |>
    dplyr::summarise(
      actual = sum(.data$value < 0, na.rm = TRUE),
      worst = suppressWarnings(min(.data$value, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::transmute(
      rule_id = "VR08", rule = "Sign check",
      dataset = .data$dataset, date = .data$date,
      check = paste(toupper(.data$dataset), "non-negative observations"),
      expected = 0, actual = .data$actual,
      difference = .data$actual, difference_pct = NA_real_,
      status = dplyr::if_else(.data$actual == 0, "Pass", "Fail"),
      detail = dplyr::if_else(
        .data$actual == 0, "No negative values",
        paste(.data$actual, "negative value(s); minimum", .data$worst)
      )
    ) |>
    finalise_rule()
}

#' Monthly payments and withdrawals per card in issue
#'
#' Uses MPCP transactions by Irish-issued cards at all locations (W0) and the
#' total of debit and credit cards in issue from MPCI.
#'
#' @param data Tidy data from [load_cards_data()].
#' @return A tibble with `cards`, `transactions` and `per_card` by period.
#' @export
payments_per_card <- function(data) {
  cards <- data |>
    dplyr::filter(.data$dataset == "mpci") |>
    dplyr::group_by(.data$date) |>
    dplyr::summarise(cards = sum(.data$value), .groups = "drop")

  data |>
    dplyr::filter(
      .data$dataset == "mpcp", .data$area_code == "W0",
      .data$channel_code == "_T", .data$measure == "Volume"
    ) |>
    dplyr::select("date", "category", transactions = "value") |>
    dplyr::inner_join(cards, by = "date") |>
    dplyr::mutate(per_card = .data$transactions / .data$cards) |>
    dplyr::arrange(.data$category, .data$date) |>
    dplyr::group_by(.data$category) |>
    dplyr::mutate(
      per_card_mom_pct = safe_pct(
        .data$per_card - dplyr::lag(.data$per_card), dplyr::lag(.data$per_card)
      ),
      per_card_yoy_pct = safe_pct(
        .data$per_card - dplyr::lag(.data$per_card, 12),
        dplyr::lag(.data$per_card, 12)
      )
    ) |>
    dplyr::ungroup()
}

#' @rdname run_validation
#' @export
check_payments_per_card <- function(data, tolerances = validation_tolerances()) {
  band <- tolerances$per_card_band
  payments_per_card(data) |>
    dplyr::filter(.data$category == "Card payments") |>
    dplyr::transmute(
      rule_id = "VR09", rule = "Payments per card",
      dataset = "mpcp/mpci", date = .data$date,
      check = "Card payments per card in issue (monthly)",
      expected = mean(band), actual = .data$per_card,
      difference = NA_real_, difference_pct = .data$per_card_yoy_pct,
      status = dplyr::case_when(
        is.na(.data$per_card) ~ "Not assessed",
        .data$per_card < band[1] | .data$per_card > band[2] ~ "Fail",
        abs(dplyr::coalesce(.data$per_card_yoy_pct, 0)) >
          tolerances$per_card_yoy_pct ~ "Warn",
        TRUE ~ "Pass"
      ),
      detail = sprintf(
        "%.1f payments per card (band %s-%s); YoY %s", .data$per_card,
        band[1], band[2],
        scales::percent(.data$per_card_yoy_pct, 0.1, style_positive = "plus")
      )
    ) |>
    finalise_rule()
}

#' Summarise validation outcomes by rule
#'
#' @param results Output of [run_validation()].
#' @param period Optional reporting period to restrict to.
#' @return One row per rule with counts by status.
#' @export
summarise_validation <- function(results, period = NULL) {
  if (!is.null(period)) {
    results <- dplyr::filter(results, .data$date == as.Date(period))
  }
  counts <- results |>
    dplyr::count(.data$rule_id, .data$status, .drop = FALSE) |>
    tidyr::pivot_wider(names_from = "status", values_from = "n", values_fill = 0)

  validation_rules() |>
    dplyr::left_join(counts, by = "rule_id") |>
    dplyr::mutate(dplyr::across(
      dplyr::any_of(validation_status_levels()), ~ dplyr::coalesce(.x, 0L)
    ))
}
