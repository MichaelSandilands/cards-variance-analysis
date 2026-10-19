# Entry point for Posit Connect / shinyapps.io and for "Run App" in RStudio.
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
run_app()
