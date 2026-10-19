# Variance engine.
#
# Analysis is split into two stages so the app stays responsive:
#   1. compute_variance_metrics(): lags, rolling statistics, STL seasonal
#      adjustment and anomaly detection. Depends only on the data.
#   2. flag_variances(): applies tolerances and materiality to the metrics.
#      Cheap, so it re-runs whenever a threshold slider moves.

#' Default variance tolerances
#'
#' Percentages are expressed as proportions (`0.10` is 10%).
#'
#' @param mom_pct Raw month-on-month tolerance.
#' @param yoy_shift Tolerated shift in year-on-year growth, in percentage
#'   points, relative to the average YoY growth of the previous three months.
#'   Testing the shift rather than the level stops series with steady
#'   structural growth (e.g. mobile wallets) breaching every month, while
#'   still catching breaks and base effects.
#' @param sa_pct Month-on-month tolerance on the seasonally adjusted series.
#' @param z Tolerance on the z-score of the month-on-month change relative to
#'   the trailing window.
#' @param min_pct Minimum raw month-on-month movement for the z-score and
#'   anomaly tests to count. Very smooth series (e.g. cards in issue) have
#'   tiny historical volatility, so a 1% move can be "unusual" without
#'   being worth a query.
#' @param materiality Named vector of absolute floors by measure. A movement
#'   smaller than this is never flagged, however large in percentage terms.
#' @return A named list.
#' @export
variance_thresholds <- function(mom_pct = 0.10,
                                yoy_shift = 0.10,
                                sa_pct = 0.05,
                                z = 3,
                                min_pct = 0.02,
                                materiality = c(
                                  Value = 1e6, Volume = 1e5, Cards = 1e4
                                )) {
  list(
    mom_pct = mom_pct,
    yoy_shift = yoy_shift,
    sa_pct = sa_pct,
    z = z,
    min_pct = min_pct,
    materiality = materiality
  )
}

#' Pad every series to a complete monthly grid
#'
#' Rows that had to be added are marked with `missing_row = TRUE` so the
#' completeness rule can report them; lag-based metrics then line up by
#' period rather than by row.
#'
#' @param data Tidy data from [load_cards_data()].
#' @return `data` with any absent periods added.
#' @export
complete_periods <- function(data) {
  grid <- month_grid(min(data$date), max(data$date))
  keys <- setdiff(tidy_schema(), c("date", "value", "confidential"))

  data |>
    dplyr::mutate(missing_row = FALSE) |>
    tidyr::complete(
      tidyr::nesting(!!!rlang::syms(keys)),
      date = grid,
      fill = list(missing_row = TRUE, confidential = FALSE)
    ) |>
    dplyr::arrange(.data$series_key, .data$date)
}

month_grid <- function(from, to) {
  starts <- timetk::tk_make_timeseries(
    lubridate::floor_date(from, "month"),
    lubridate::floor_date(to, "month"),
    by = "month"
  )
  month_end(starts)
}

#' Compute variance metrics for every series
#'
#' For each series this adds:
#' * `value_lag1`, `value_lag12` via [timetk::tk_augment_lags()];
#' * `mom_abs`, `mom_pct`, `yoy_abs`, `yoy_pct`;
#' * `yoy_trend`: mean YoY growth over the previous three months, and
#'   `yoy_shift`, the current YoY growth less that trend;
#' * `z_mom`: the month-on-month change standardised against the trailing
#'   `window` months (via [timetk::slidify_vec()]), excluding the current
#'   month so a break cannot hide itself;
#' * `season`, `trend`, `remainder`, `seasadj` from an STL decomposition
#'   and `sa_mom_pct`, the month-on-month change of the seasonally adjusted
#'   series. Strictly positive series are decomposed on the log scale
#'   (multiplicative seasonality), since the December peak grows with the
#'   level of spending; components are returned on the original scale;
#' * `anomaly`, `anomaly_direction`, `recomposed_l1`, `recomposed_l2` from
#'   [timetk::anomalize()].
#'
#' Suppressed (confidential) values are imputed with
#' [timetk::ts_impute_vec()] for the decomposition only; their own metrics
#' stay `NA`.
#'
#' @param data Tidy data from [load_cards_data()].
#' @param window Trailing window length, in months, for the z-score.
#' @param iqr_alpha Anomaly sensitivity passed to [timetk::anomalize()];
#'   larger values flag more points.
#' @return A tibble, one row per series and period.
#' @export
compute_variance_metrics <- function(data, window = 12, iqr_alpha = 0.05) {
  data <- complete_periods(data)

  base <- data |>
    dplyr::group_by(.data$series_key) |>
    timetk::tk_augment_lags(.value = "value", .lags = c(1, 12)) |>
    dplyr::mutate(
      mom_abs = .data$value - .data$value_lag1,
      mom_pct = safe_pct(.data$mom_abs, .data$value_lag1),
      yoy_abs = .data$value - .data$value_lag12,
      yoy_pct = safe_pct(.data$yoy_abs, .data$value_lag12),
      yoy_trend = timetk::slidify_vec(
        dplyr::lag(.data$yoy_pct), ~ mean(.x, na.rm = TRUE),
        .period = 3, .align = "right", .partial = TRUE
      ),
      yoy_trend = dplyr::if_else(is.nan(.data$yoy_trend), NA_real_, .data$yoy_trend),
      yoy_shift = .data$yoy_pct - .data$yoy_trend,
      mom_prev = dplyr::lag(.data$mom_pct),
      roll_mean = timetk::slidify_vec(
        .data$mom_prev, ~ mean(.x, na.rm = TRUE),
        .period = window, .align = "right", .partial = TRUE
      ),
      roll_sd = timetk::slidify_vec(
        .data$mom_prev, ~ stats::sd(.x, na.rm = TRUE),
        .period = window, .align = "right", .partial = TRUE
      ),
      z_mom = (.data$mom_pct - .data$roll_mean) / .data$roll_sd,
      z_mom = dplyr::if_else(is.finite(.data$z_mom), .data$z_mom, NA_real_)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-"mom_prev", -"roll_mean", -"roll_sd")

  decomposed <- base |>
    dplyr::group_by(.data$series_key) |>
    dplyr::group_modify(~ decompose_series(.x, iqr_alpha)) |>
    dplyr::ungroup()

  base |>
    dplyr::left_join(decomposed, by = c("series_key", "date")) |>
    dplyr::group_by(.data$series_key) |>
    dplyr::mutate(
      sa_mom_pct = safe_pct(
        .data$seasadj - dplyr::lag(.data$seasadj), dplyr::lag(.data$seasadj)
      ),
      sa_mom_pct = dplyr::if_else(is.na(.data$value), NA_real_, .data$sa_mom_pct)
    ) |>
    dplyr::ungroup()
}

safe_pct <- function(num, den) {
  dplyr::if_else(!is.na(den) & den != 0, num / abs(den), NA_real_)
}

# STL decomposition + anomaly detection for a single series. Returns an empty
# frame (all metrics NA after the join) when there is not enough history.
decompose_series <- function(df, iqr_alpha) {
  empty <- tibble::tibble(
    date = df$date, season = NA_real_, trend = NA_real_,
    remainder = NA_real_, seasadj = NA_real_, anomaly = NA_character_,
    anomaly_direction = NA_real_, recomposed_l1 = NA_real_,
    recomposed_l2 = NA_real_
  )
  observed <- sum(!is.na(df$value))
  if (observed < 24 || observed < 0.75 * nrow(df)) {
    return(empty)
  }

  filled <- df |>
    dplyr::transmute(
      date = .data$date,
      value = timetk::ts_impute_vec(.data$value, period = 12)
    )
  multiplicative <- all(filled$value > 0)
  if (multiplicative) filled$value <- log(filled$value)

  result <- tryCatch(
    timetk::anomalize(
      filled, .date_var = date, .value = value,
      .frequency = 12, .trend = 12,
      .iqr_alpha = iqr_alpha, .max_anomalies = 0.2, .message = FALSE
    ),
    error = function(e) NULL
  )
  if (is.null(result)) {
    return(empty)
  }

  # Back-transform: on the log scale components combine multiplicatively,
  # so season/remainder become factors and trend/seasadj/bands are levels.
  back <- if (multiplicative) exp else identity
  result |>
    dplyr::transmute(
      date = .data$date,
      season = back(.data$season),
      trend = back(.data$trend),
      remainder = back(.data$remainder),
      seasadj = back(.data$seasadj),
      anomaly = dplyr::if_else(is.na(df$value), NA_character_, .data$anomaly),
      anomaly_direction = .data$anomaly_direction,
      recomposed_l1 = back(.data$recomposed_l1),
      recomposed_l2 = back(.data$recomposed_l2)
    )
}

#' Apply tolerances and assign a RAG status
#'
#' A raw month-on-month breach on its own is treated as seasonal (card spend
#' always jumps in December), so it never escalates by itself. Escalation is
#' driven by the independent tests: year-on-year, seasonally adjusted
#' month-on-month, the shift in year-on-year growth, the rolling z-score and
#' the STL anomaly flag.
#'
#' * **Breach**: two or more independent tests fail.
#' * **Review**: exactly one independent test fails.
#' * **Pass**: none fail (a seasonal-only movement is noted in `reason`).
#' * **Not assessed**: value suppressed as confidential.
#' * **Missing**: no value and not flagged confidential.
#'
#' @param metrics Output of [compute_variance_metrics()].
#' @param thresholds Output of [variance_thresholds()].
#' @return `metrics` with breach flags, `status` and `reason` added.
#' @export
flag_variances <- function(metrics, thresholds = variance_thresholds()) {
  t <- thresholds
  mat <- unname(t$materiality[metrics$measure])
  mat[is.na(mat)] <- 0

  out <- metrics |>
    dplyr::mutate(
      materiality = mat,
      material = !is.na(.data$mom_abs) & abs(.data$mom_abs) >= .data$materiality,
      material_yoy = !is.na(.data$yoy_abs) &
        abs(.data$yoy_abs) >= .data$materiality,
      breach_mom = .data$material & abs(.data$mom_pct) > t$mom_pct,
      breach_yoy = .data$material_yoy & !is.na(.data$yoy_shift) &
        abs(.data$yoy_shift) > t$yoy_shift,
      breach_sa = .data$material & !is.na(.data$sa_mom_pct) &
        abs(.data$sa_mom_pct) > t$sa_pct,
      notable = !is.na(.data$mom_pct) & abs(.data$mom_pct) >= t$min_pct,
      breach_z = .data$material & .data$notable & !is.na(.data$z_mom) &
        abs(.data$z_mom) > t$z,
      breach_anomaly = .data$material & .data$notable &
        .data$anomaly %in% "Yes",
      dplyr::across(dplyr::starts_with("breach_"), ~ dplyr::coalesce(.x, FALSE)),
      tests_failed = .data$breach_yoy + .data$breach_sa + .data$breach_z +
        .data$breach_anomaly,
      status = dplyr::case_when(
        is.na(.data$value) & .data$confidential ~ "Not assessed",
        is.na(.data$value) ~ "Missing",
        .data$tests_failed >= 2 ~ "Breach",
        .data$tests_failed == 1 ~ "Review",
        TRUE ~ "Pass"
      ),
      status = factor(.data$status, levels = status_levels())
    )

  out$reason <- build_reasons(out, t)
  out
}

#' Ordered status levels used throughout the app
#' @return Character vector.
#' @export
status_levels <- function() {
  c("Breach", "Review", "Missing", "Pass", "Not assessed")
}

build_reasons <- function(df, t) {
  pct <- function(x) scales::percent(x, accuracy = 0.1, style_positive = "plus")
  tol <- function(x) scales::percent(x, accuracy = 1)

  parts <- list(
    ifelse(df$breach_yoy,
      paste0(
        "YoY ", pct(df$yoy_pct), " vs ", pct(df$yoy_trend), " trend (",
        sprintf("%+.1f", 100 * df$yoy_shift), "pp, tolerance \u00b1",
        round(100 * t$yoy_shift), "pp)"
      ), NA
    ),
    ifelse(df$breach_sa,
      paste0("SA MoM ", pct(df$sa_mom_pct), " beyond \u00b1", tol(t$sa_pct)), NA
    ),
    ifelse(df$breach_z,
      paste0("z-score ", sprintf("%+.1f", df$z_mom), " beyond \u00b1", t$z), NA
    ),
    ifelse(df$breach_anomaly,
      paste0(
        "STL anomaly (",
        ifelse(df$anomaly_direction > 0, "above", "below"),
        " expected range)"
      ), NA
    )
  )
  reasons <- do.call(paste, c(parts, sep = "; "))
  reasons <- gsub("(NA; )|(; NA)", "", reasons)
  reasons[reasons == "NA"] <- NA

  seasonal <- df$breach_mom & df$tests_failed == 0
  reasons[seasonal] <- paste0(
    "Seasonal: raw MoM ", pct(df$mom_pct[seasonal]),
    " but SA MoM ", pct(df$sa_mom_pct[seasonal]),
    " and YoY ", pct(df$yoy_pct[seasonal]), " in line with trend"
  )
  immaterial <- is.na(reasons) & !df$material & !is.na(df$mom_abs) &
    abs(dplyr::coalesce(df$mom_pct, 0)) > t$mom_pct
  reasons[immaterial] <- "Below materiality threshold"
  reasons[df$status == "Not assessed"] <- "Suppressed (confidential)"
  reasons[df$status == "Missing"] <- "No observation reported"
  reasons[is.na(reasons)] <- "Within tolerance"
  reasons
}

#' Variance results for one reporting period
#'
#' @param flagged Output of [flag_variances()].
#' @param period Reporting period (Date). Defaults to the latest.
#' @return One row per series for `period`, worst status first.
#' @export
period_variances <- function(flagged, period = max(flagged$date)) {
  flagged |>
    dplyr::filter(.data$date == as.Date(period)) |>
    dplyr::arrange(.data$status, dplyr::desc(abs(.data$yoy_pct)))
}

#' Decompose a value movement into volume and average-ticket effects
#'
#' For a value series \eqn{V = N \times ATV}, the change between a base
#' period 0 and current period 1 splits exactly into
#' \deqn{\Delta V = (N_1 - N_0) ATV_0 + (ATV_1 - ATV_0) N_1}
#' i.e. a volume effect (more or fewer transactions at the old average
#' ticket) and a price/mix effect (change in average transaction value).
#'
#' @param data Tidy data from [load_cards_data()].
#' @param pair_key Series key without unit suffix (see [tidy_schema()]).
#' @param period Current period.
#' @param compare `"mom"` (prior month) or `"yoy"` (same month last year).
#' @return A one-row tibble, or `NULL` if either period is unavailable.
#' @export
value_volume_bridge <- function(data, pair_key, period,
                                compare = c("mom", "yoy")) {
  compare <- match.arg(compare)
  period <- as.Date(period)
  base_period <- month_end(
    lubridate::floor_date(period, "month") -
      months(if (compare == "mom") 1 else 12)
  )

  wide <- data |>
    dplyr::filter(
      .data$pair_key == !!pair_key,
      .data$date %in% c(base_period, period),
      .data$measure %in% c("Value", "Volume")
    ) |>
    dplyr::select("date", "measure", "value") |>
    tidyr::pivot_wider(names_from = "measure", values_from = "value")

  if (nrow(wide) < 2 || !all(c("Value", "Volume") %in% names(wide)) ||
    anyNA(wide$Value) || anyNA(wide$Volume)) {
    return(NULL)
  }

  b <- wide[wide$date == base_period, ]
  c1 <- wide[wide$date == period, ]
  atv0 <- b$Value / b$Volume
  atv1 <- c1$Value / c1$Volume

  tibble::tibble(
    pair_key = pair_key,
    base_period = base_period,
    period = period,
    base_value = b$Value,
    current_value = c1$Value,
    base_volume = b$Volume,
    current_volume = c1$Volume,
    base_atv = atv0,
    current_atv = atv1,
    volume_effect = (c1$Volume - b$Volume) * atv0,
    atv_effect = (atv1 - atv0) * c1$Volume,
    total_change = c1$Value - b$Value
  )
}
