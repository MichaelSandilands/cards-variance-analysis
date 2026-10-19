test_that("app object builds", {
  app <- run_app()
  expect_s3_class(app, "shiny.appobj")
})

test_that("server computes flags and responds to tolerance changes", {
  shiny::testServer(app_server(cards_data), {
    session$setInputs(
      period = "2026-08-31", iqr_alpha = 0.05, mom_pct = 10, yoy_shift = 10,
      sa_pct = 5, z = 3, min_pct = 2, mat_value = 1, mat_volume = 100,
      mat_cards = 10, recon_pct = 0.05, coverage_pct = 98
    )
    strict <- sum(flagged()$status %in% c("Breach", "Review"))
    session$setInputs(sa_pct = 30, yoy_shift = 50, z = 6)
    loose <- sum(flagged()$status %in% c("Breach", "Review"))
    expect_lt(loose, strict)
    expect_gt(nrow(validation()), 0)
  })
})

test_that("variance module renders for the selected period", {
  f <- shiny::reactive(flag_variances(compute_variance_metrics(cards_data)))
  shiny::testServer(
    mod_variance_server,
    args = list(
      flagged = f, data = shiny::reactive(cards_data),
      period = shiny::reactive(as.Date("2026-08-31"))
    ),
    {
      session$setInputs(dataset = "", status = c("Breach", "Review"),
                        compare = "yoy")
      expect_true(all(rows()$status %in% c("Breach", "Review")))
      expect_false(is.null(bridge()))
      expect_equal(bridge()$volume_effect + bridge()$atv_effect,
                   bridge()$total_change)
    }
  )
})

test_that("an uploaded draft replaces only its own return", {
  draft <- withr::local_tempfile(fileext = ".csv")
  raw <- readr::read_csv(cbi_file("mpci"), col_types = readr::cols(.default = "c"))
  raw$OBSERVATION_FREE[1] <- "1" # implausible drop in credit cards for Aug 2026
  readr::write_csv(raw, draft)

  shiny::testServer(app_server(cards_data), {
    session$setInputs(
      period = "2026-08-31", iqr_alpha = 0.05, mom_pct = 10, yoy_shift = 10,
      sa_pct = 5, z = 3, min_pct = 2, mat_value = 1, mat_volume = 100,
      mat_cards = 10, recon_pct = 0.05, coverage_pct = 98
    )
    session$setInputs(upload = data.frame(
      name = "draft.csv", size = file.size(draft), type = "text/csv",
      datapath = draft
    ))
    expect_equal(nrow(source_data()), nrow(cards_data))
    credit <- dplyr::filter(
      flagged(), series_key == "PCI.M.IE.W2.PCS_ALL.13.PN",
      date == as.Date("2026-08-31")
    )
    expect_equal(as.character(credit$status), "Breach")
  })
})
