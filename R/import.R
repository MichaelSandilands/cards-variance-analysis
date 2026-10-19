# Readers for the Central Bank of Ireland card statistics extracts.
#
# Three returns are supported, each published as an SDMX-style flat CSV:
#   * mpci - cards in issue (by card function)
#   * mpcp - card payments and ATM withdrawals (by terminal location/channel)
#   * mpcy - card payments by county
#
# All readers return the same tidy schema so that one variance engine and one
# set of validation rules can run across every return. See `tidy_schema()`.

#' Path to a bundled source file
#'
#' @param dataset One of `"mpci"`, `"mpcp"` or `"mpcy"`.
#' @return Path to the CSV shipped in `inst/extdata`.
#' @export
#' @examples
#' cbi_file("mpcp")
cbi_file <- function(dataset = c("mpci", "mpcp", "mpcy")) {
  dataset <- match.arg(dataset)
  file <- switch(dataset,
    mpci = "mpci_cards_in_issue.csv",
    mpcp = "mpcp_card_payments.csv",
    mpcy = "mpcy_card_payments_by_county.csv"
  )
  system.file("extdata", file, package = "cardsvariance", mustWork = TRUE)
}

#' Columns shared by every tidy return
#'
#' * `dataset`: return code (`mpci`, `mpcp`, `mpcy`).
#' * `series_key`: SDMX series key, unique per series.
#' * `pair_key`: series key without the unit suffix; joins value to volume.
#' * `date`: reporting period (month end).
#' * `value`: observation; `NA` when suppressed.
#' * `confidential`: `TRUE` when the confidentiality flag is `C`.
#' * `unit`: `EUR` or `PN` (pure number).
#' * `measure`: `Value`, `Volume` or `Cards`.
#' * `area_code`/`area`: terminal location (mpcp), county (mpcy) or holder
#'   area (mpci).
#' * `category_code`/`category`: transaction type or card function.
#' * `channel_code`/`channel`: remote / non-remote initiation.
#' * `series_label`: readable label built from the dimensions.
#'
#' @return Character vector of column names, in order.
#' @export
tidy_schema <- function() {
  c(
    "dataset", "series_key", "pair_key", "date", "value", "confidential",
    "unit", "measure", "area_code", "area", "category_code", "category",
    "channel_code", "channel", "series_label"
  )
}

#' Read a CBI card statistics extract
#'
#' The return type is detected from the header, so draft submissions can be
#' dropped in without saying which return they belong to.
#'
#' @param path Path to a CSV extract.
#' @return A tibble following [tidy_schema()].
#' @export
#' @examples
#' read_cbi_file(cbi_file("mpci"))
read_cbi_file <- function(path) {
  raw <- readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    progress = FALSE
  )
  dataset <- detect_dataset(names(raw))
  switch(dataset,
    mpci = tidy_mpci(raw),
    mpcp = tidy_mpcp(raw),
    mpcy = tidy_mpcy(raw)
  )
}

#' Load all bundled returns as one tidy tibble
#'
#' @param paths Optional character vector of CSV paths. Defaults to the
#'   bundled extracts.
#' @return A tibble following [tidy_schema()].
#' @export
load_cards_data <- function(paths = NULL) {
  if (is.null(paths)) {
    paths <- vapply(c("mpci", "mpcp", "mpcy"), cbi_file, character(1))
  }
  purrr::map(paths, read_cbi_file) |>
    purrr::list_rbind() |>
    dplyr::arrange(.data$dataset, .data$series_key, .data$date)
}

detect_dataset <- function(cols) {
  if ("OBSERVATION_FREE" %in% cols && "CRD_FNCTN" %in% cols) {
    return("mpci")
  }
  if ("COUNTY" %in% cols) {
    return("mpcy")
  }
  if ("TRMNL_LCTN" %in% cols) {
    return("mpcp")
  }
  rlang::abort(c(
    "Could not identify the return from the file header.",
    i = "Expected an mpci, mpcp or mpcy extract."
  ))
}

#' Parse a reporting period to its month-end date
#'
#' Accepts ISO (`2026-08-31`) and day-first (`31/08/2026`) formats.
#'
#' @param x Character vector of periods.
#' @return A Date vector, always the last day of the month.
#' @export
#' @examples
#' parse_period(c("2026-08-31", "31/08/2026", "2026-08-15"))
parse_period <- function(x) {
  d <- lubridate::parse_date_time(x, orders = c("Ymd", "dmY"), quiet = TRUE)
  if (anyNA(d[!is.na(x)])) {
    bad <- unique(x[is.na(d) & !is.na(x)])
    rlang::abort(c(
      "Unparseable reporting period(s).",
      x = paste(utils::head(bad, 5), collapse = ", ")
    ))
  }
  month_end(as.Date(d))
}

month_end <- function(d) {
  lubridate::ceiling_date(d, "month") - 1
}

unit_measure <- function(unit, cards = FALSE) {
  dplyr::case_when(
    cards ~ "Cards",
    unit == "EUR" ~ "Value",
    TRUE ~ "Volume"
  )
}

strip_unit <- function(series_key) {
  sub("\\.[A-Z]+$", "", series_key)
}

finish_tidy <- function(df) {
  df |>
    dplyr::mutate(
      pair_key = strip_unit(.data$series_key),
      series_label = build_label(
        .data$dataset, .data$area, .data$category, .data$channel,
        .data$measure
      )
    ) |>
    dplyr::select(dplyr::all_of(tidy_schema()))
}

build_label <- function(dataset, area, category, channel, measure) {
  parts <- ifelse(
    is.na(channel) | channel == "All channels",
    paste(area, category, sep = " | "),
    paste(area, category, channel, sep = " | ")
  )
  paste0(toupper(dataset), " | ", parts, " | ", measure)
}

tidy_mpci <- function(raw) {
  raw |>
    dplyr::transmute(
      dataset = "mpci",
      series_key = .data$SERIES_KEY,
      date = parse_period(.data$REPORTING_PERIOD),
      value = readr::parse_double(.data$OBSERVATION_FREE),
      confidential = .data$CONFIDENTIALITY_FLAG %in% "C",
      unit = .data$UNIT_MEASURE,
      measure = unit_measure(.data$UNIT_MEASURE, cards = TRUE),
      area_code = .data$HOLDER_AREA,
      area = label_area(.data$HOLDER_AREA, "holder"),
      category_code = .data$CRD_FNCTN,
      category = label_card_function(.data$CRD_FNCTN),
      channel_code = NA_character_,
      channel = NA_character_
    ) |>
    finish_tidy()
}

tidy_mpcp <- function(raw) {
  raw |>
    dplyr::transmute(
      dataset = "mpcp",
      series_key = .data$SERIES_KEY,
      date = parse_period(.data$REPORTINGPERIOD),
      value = readr::parse_double(.data$OBSERVATION),
      confidential = .data$CONFIDENTIALITY_FLAG %in% "C",
      unit = .data$UNIT_MEASURE,
      measure = unit_measure(.data$UNIT_MEASURE),
      area_code = .data$TRMNL_LCTN,
      area = label_area(.data$TRMNL_LCTN, "terminal"),
      category_code = .data$TYP_TRNSCTN,
      category = label_transaction(.data$TYP_TRNSCTN),
      channel_code = .data$RMT_INTTN,
      channel = label_channel(.data$RMT_INTTN)
    ) |>
    finish_tidy()
}

tidy_mpcy <- function(raw) {
  raw |>
    dplyr::transmute(
      dataset = "mpcy",
      series_key = .data$SERIES_KEY,
      date = parse_period(.data$REPORTINGPERIOD),
      value = readr::parse_double(.data$OBSERVATION),
      confidential = .data$CONFIDENTIALITY_FLAG %in% "C",
      unit = .data$UNIT_MEASURE,
      measure = unit_measure(.data$UNIT_MEASURE),
      area_code = .data$COUNTY,
      area = dplyr::if_else(
        .data$COUNTY == "_T", "All counties", .data$COUNTY_DESC
      ),
      category_code = .data$TYP_TRNSCTN,
      category = label_transaction(.data$TYP_TRNSCTN),
      channel_code = .data$RMT_INTTN,
      channel = label_channel(.data$RMT_INTTN)
    ) |>
    finish_tidy()
}

# Short labels for the SDMX code lists ---------------------------------------

label_area <- function(code, type = c("terminal", "holder")) {
  type <- match.arg(type)
  lookup <- c(W0 = "All locations", W1 = "Abroad", W2 = "Domestic")
  if (type == "holder") lookup["W2"] <- "Irish-resident holders"
  unname(dplyr::coalesce(lookup[code], code))
}

label_transaction <- function(code) {
  lookup <- c(
    CP0 = "Card payments",
    CP1 = "Contactless (POS)",
    CP11 = "NFC / mobile wallet (POS)",
    CW1 = "ATM cash withdrawals"
  )
  unname(dplyr::coalesce(lookup[code], code))
}

label_card_function <- function(code) {
  lookup <- c(`11` = "Debit cards", `13` = "Credit cards")
  unname(dplyr::coalesce(lookup[code], code))
}

label_channel <- function(code) {
  lookup <- c(`_T` = "All channels", NR = "Non-remote", R = "Remote")
  unname(dplyr::coalesce(lookup[code], code))
}
