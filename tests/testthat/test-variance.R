test_that("lags and percentage changes are computed per period", {
  d <- make_series(c(100, 110, 99, rep(100, 9), 120) * 1e6)
  m <- compute_variance_metrics(d)
  expect_equal(m$mom_pct[2], 0.10)
  expect_equal(m$mom_pct[3], -0.10)
  expect_equal(m$yoy_pct[13], 0.20)
  expect_true(is.na(m$yoy_pct[12]))
})

test_that("missing periods are padded so lags stay aligned", {
  d <- make_series(seq(100, 112) * 1e6)[-5, ]
  m <- compute_variance_metrics(d)
  expect_equal(nrow(m), 13)
  expect_true(m$missing_row[5])
  expect_true(is.na(m$mom_pct[6]))
  expect_equal(m$value_lag12[13], 100e6)
})

test_that("STL seasonal adjustment removes a regular December peak", {
  d <- make_series(seasonal_values())
  f <- flag_variances(compute_variance_metrics(d))
  dec <- dplyr::filter(f, lubridate::month(date) == 12, !is.na(yoy_pct))
  # Raw MoM is large every December...
  expect_true(all(dec$mom_pct > 0.10))
  # ...but the seasonally adjusted move is small and nothing escalates.
  expect_true(all(abs(dec$sa_mom_pct) < 0.02))
  expect_true(all(dec$status == "Pass"))
  expect_true(all(grepl("^Seasonal", dec$reason)))
})

test_that("a genuine level shift is escalated to a breach", {
  v <- seasonal_values()
  v[47] <- v[47] * 1.4
  f <- flag_variances(compute_variance_metrics(make_series(v)))
  spike <- f[47, ]
  expect_equal(as.character(spike$status), "Breach")
  expect_true(spike$breach_sa)
  expect_true(spike$breach_yoy)
  expect_match(spike$reason, "SA MoM")
})

test_that("materiality floor suppresses large percentage moves on small series", {
  v <- seasonal_values(level = 1e5)
  v[47] <- v[47] * 1.4
  f <- flag_variances(compute_variance_metrics(make_series(v)))
  expect_equal(as.character(f$status[47]), "Pass")
  expect_false(f$material[47])
})

test_that("confidential cells are not assessed and not imputed into results", {
  v <- seasonal_values()
  v[30] <- NA
  conf <- is.na(v)
  f <- flag_variances(compute_variance_metrics(make_series(v, confidential = conf)))
  expect_equal(as.character(f$status[30]), "Not assessed")
  expect_true(is.na(f$sa_mom_pct[30]))
  # The decomposition still runs for the rest of the series
  expect_false(is.na(f$seasadj[40]))
})

test_that("blank cells without a confidentiality flag are reported missing", {
  v <- seasonal_values()
  v[30] <- NA
  f <- flag_variances(compute_variance_metrics(make_series(v)))
  expect_equal(as.character(f$status[30]), "Missing")
})

test_that("value bridge decomposes the change exactly", {
  val <- make_series(c(1000, 1320), key = "X.EUR", measure = "Value")
  vol <- make_series(c(100, 120), key = "X.PN", measure = "Volume")
  b <- value_volume_bridge(dplyr::bind_rows(val, vol), "X", val$date[2], "mom")
  expect_equal(b$volume_effect, 200) # 20 extra transactions at EUR 10
  expect_equal(b$atv_effect, 120) # EUR 1 higher ticket on 120 transactions
  expect_equal(b$volume_effect + b$atv_effect, b$total_change)
  expect_null(value_volume_bridge(val, "X", val$date[2]))
})

test_that("bundled data flags the known March 2023 break and Laois spike", {
  f <- flag_variances(compute_variance_metrics(cards_data))
  mar23 <- period_variances(f, "2023-03-31")
  debit <- dplyr::filter(mar23, series_key == "PCI.M.IE.W2.PCS_ALL.11.PN")
  expect_equal(as.character(debit$status), "Breach")

  aug26 <- period_variances(f, "2026-08-31")
  laois <- dplyr::filter(aug26, area == "Laois", category == "Card payments")
  expect_true(all(laois$status == "Breach"))
})
