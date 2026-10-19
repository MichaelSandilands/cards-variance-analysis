test_that("parse_period handles both CBI date formats and returns month end", {
  expect_equal(
    parse_period(c("2026-08-31", "31/08/2026", "2024-02-10")),
    as.Date(c("2026-08-31", "2026-08-31", "2024-02-29"))
  )
  expect_error(parse_period("August 2026"), "Unparseable")
})

test_that("bundled extracts load into the tidy schema", {
  d <- load_cards_data()
  expect_named(d, tidy_schema())
  expect_setequal(unique(d$dataset), c("mpci", "mpcp", "mpcy"))
  expect_equal(nrow(d), 94 + 1128 + 10152)
  expect_false(anyDuplicated(d[c("series_key", "date")]) > 0)
  # Confidential cells are exactly the blank observations
  expect_identical(is.na(d$value), d$confidential)
})

test_that("return type is detected from the header", {
  expect_equal(unique(read_cbi_file(cbi_file("mpci"))$dataset), "mpci")
  expect_equal(unique(read_cbi_file(cbi_file("mpcy"))$dataset), "mpcy")
  bad <- withr::local_tempfile(fileext = ".csv")
  writeLines("a,b\n1,2", bad)
  expect_error(read_cbi_file(bad), "Could not identify")
})

test_that("value and volume series share a pair key", {
  d <- dplyr::filter(cards_data, dataset == "mpcp")
  pairs <- dplyr::count(d, pair_key, measure) |> dplyr::count(pair_key)
  expect_true(all(pairs$n == 2))
})
