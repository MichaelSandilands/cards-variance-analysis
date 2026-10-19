# Small synthetic series for deterministic tests.
make_series <- function(values, key = "TEST.M.EUR", measure = "Value",
                        start = as.Date("2022-01-01"), confidential = NULL) {
  n <- length(values)
  dates <- lubridate::ceiling_date(
    seq(start, by = "month", length.out = n), "month"
  ) - 1
  tibble::tibble(
    dataset = "mpcp",
    series_key = key,
    pair_key = sub("\\.[A-Z]+$", "", key),
    date = dates,
    value = values,
    confidential = confidential %||% rep(FALSE, n),
    unit = if (measure == "Value") "EUR" else "PN",
    measure = measure,
    area_code = "W0", area = "All locations",
    category_code = "CP0", category = "Card payments",
    channel_code = "_T", channel = "All channels",
    series_label = paste("Test", measure)
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# Seasonal series with steady growth: 4 years of monthly data.
seasonal_values <- function(n = 48, level = 1e9, growth = 0.005) {
  m <- (seq_len(n) - 1) %% 12 + 1
  season <- c(0.92, 0.9, 1, 0.98, 1.01, 1, 1.02, 1.01, 0.98, 1.0, 1.03, 1.15)
  level * (1 + growth)^(seq_len(n) - 1) * season[m]
}
