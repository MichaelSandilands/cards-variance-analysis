## Build the bundled `cards_data` dataset from the raw CSV extracts in
## inst/extdata. Run with the package loaded: devtools::load_all()

devtools::load_all()

cards_data <- load_cards_data(c(
  "inst/extdata/mpci_cards_in_issue.csv",
  "inst/extdata/mpcp_card_payments.csv",
  "inst/extdata/mpcy_card_payments_by_county.csv"
))

stopifnot(
  !anyDuplicated(cards_data[c("series_key", "date")]),
  all(is.na(cards_data$value) == cards_data$confidential)
)

usethis::use_data(cards_data, overwrite = TRUE, compress = "xz")
