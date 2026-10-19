# One-off project scaffolding, kept for reproducibility.
# Run from the project root. Each step is idempotent enough to re-run.

# The repo already exists as a git checkout, so allow creating the package in it.
options(usethis.allow_nested_project = TRUE)

usethis::create_package(
  ".",
  fields = list(
    Package = "cardsvariance",
    Title = "Variance Analysis and Validation of Card Payments Regulatory Data",
    Description = paste(
      "A Shiny application and supporting functions for pre-submission",
      "validation of card payments statistics: period-on-period variance",
      "analysis, seasonal adjustment and anomaly detection with 'timetk',",
      "hierarchy and cross-return reconciliation rules, and value/volume",
      "bridge analysis."
    ),
    `Authors@R` = 'person("Michael", "Sandilands", email = "msand91@gmail.com", role = c("aut", "cre"))',
    Language = "en-GB"
  ),
  rstudio = TRUE,
  check_name = FALSE, # folder name differs from the package name
  open = FALSE
)

usethis::use_mit_license("Michael Sandilands")
usethis::use_roxygen_md()
usethis::use_testthat(3)
usethis::use_data_raw("cards_data", open = FALSE)
usethis::use_readme_md(open = FALSE)
usethis::use_news_md(open = FALSE)
usethis::use_pipe(export = TRUE)

# Runtime dependencies
for (pkg in c(
  "shiny", "bslib", "DT", "plotly", "timetk", "dplyr", "tidyr",
  "purrr", "readr", "lubridate", "tibble", "rlang", "scales",
  "markdown", "stats", "tools", "utils"
)) {
  usethis::use_package(pkg)
}
usethis::use_package("R", type = "Depends", min_version = "4.1")

# Development-only dependencies
usethis::use_package("pkgload", type = "Suggests")
usethis::use_package("withr", type = "Suggests")

usethis::use_build_ignore(c("dev", "app.R", "renv", "renv.lock", "data-raw"))
usethis::use_git_ignore(c(".Rproj.user", "renv/library/", "renv/staging/", "renv/local/"))

# Reproducible environment: copy installed packages into the project library
# and record them. On a fresh machine use renv::restore() instead.
renv::init(bare = TRUE, restart = FALSE)
renv::hydrate()
renv::snapshot(prompt = FALSE)
