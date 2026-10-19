test_that("hierarchy rules hold on the published data", {
  v <- run_validation(cards_data)
  for (id in c("VR01", "VR02", "VR03", "VR04", "VR08")) {
    expect_false(any(v$status[v$rule_id == id] == "Fail"), info = id)
  }
  expect_setequal(unique(v$rule_id), validation_rules()$rule_id)
  expect_named(v, cardsvariance:::validation_columns())
})

test_that("a broken geographic identity is caught", {
  bad <- cards_data
  i <- which(bad$series_key == "PCP.M.IE._Z.W1.CP0.1._T._T.PCS_ALL._Z._X._Z.N.EUR" &
    bad$date == as.Date("2026-08-31"))
  bad$value[i] <- bad$value[i] + 5e6
  v <- check_geographic_hierarchy(bad)
  fails <- dplyr::filter(v, status == "Fail")
  expect_equal(nrow(fails), 1)
  expect_equal(fails$date, as.Date("2026-08-31"))
  expect_equal(fails$difference, 5e6)
})

test_that("counties exceeding the national total fail allocation", {
  bad <- cards_data
  i <- which(bad$dataset == "mpcy" & bad$area == "Dublin" &
    bad$category_code == "CW1" & bad$measure == "Volume" &
    bad$date == as.Date("2026-08-31"))
  bad$value[i] <- bad$value[i] * 2
  v <- check_county_allocation(bad)
  hit <- dplyr::filter(v, date == as.Date("2026-08-31"), grepl("ATM.*Volume", check))
  expect_equal(as.character(hit$status), "Fail")
})

test_that("NFC above contactless breaks the sub-category bound", {
  bad <- cards_data
  i <- which(bad$dataset == "mpcy" & bad$area == "Cork" &
    bad$category_code == "CP11" & bad$measure == "Volume" &
    bad$date == as.Date("2026-08-31"))
  bad$value[i] <- bad$value[i] * 10
  v <- check_subcategory_bounds(bad)
  expect_true(any(v$status == "Fail" & grepl("Cork", v$check)))
})

test_that("cross-return reconciliation grades by tolerance", {
  v <- check_cross_return(cards_data)
  expect_true(all(v$status %in% c("Pass", "Warn", "Fail")))
  oct22 <- dplyr::filter(v, date == as.Date("2022-10-31"),
                         grepl("Card payments \\| Value", check))
  expect_equal(as.character(oct22$status), "Fail")
  loose <- check_cross_return(
    cards_data, validation_tolerances(reconciliation_pct = 0.01)
  )
  expect_false(any(loose$status == "Fail"))
})

test_that("completeness flags missing rows, unflagged blanks and duplicates", {
  d <- dplyr::filter(cards_data, dataset == "mpci")
  d <- d[-1, ] # drop a period for one series
  d$value[5] <- NA # blank without confidentiality flag
  d <- dplyr::bind_rows(d, d[10, ]) # duplicate
  v <- check_completeness(d)
  details <- v$detail[v$status == "Fail"]
  expect_true(any(details == "Period not reported"))
  expect_true(any(details == "Blank value without confidentiality flag"))
  expect_true(any(details == "Duplicate observation for series and period"))
})

test_that("negative values fail the sign check", {
  d <- dplyr::filter(cards_data, dataset == "mpci")
  d$value[1] <- -1
  expect_true(any(check_signs(d)$status == "Fail"))
})

test_that("payments per card is plausible on the bundled data", {
  ppc <- payments_per_card(cards_data)
  card <- dplyr::filter(ppc, category == "Card payments")
  expect_true(all(card$per_card > 10 & card$per_card < 60))
})

test_that("exceptions log combines both sources with a commentary column", {
  f <- flag_variances(compute_variance_metrics(cards_data))
  v <- run_validation(cards_data)
  log <- exceptions_log(f, v, "2026-08-31")
  expect_true(all(c("Variance", "Validation") %in% log$source))
  expect_true(all(log$commentary == ""))
  expect_equal(unique(log$period), as.Date("2026-08-31"))
})
