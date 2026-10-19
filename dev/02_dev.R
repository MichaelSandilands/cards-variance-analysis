# Day-to-day development loop.

devtools::load_all()       # load package code into the session
devtools::document()       # regenerate NAMESPACE and man/ from roxygen
devtools::test()           # run testthat suite
devtools::check()          # full R CMD check

# Rebuild the bundled dataset after changing the raw extracts or the readers
source("data-raw/cards_data.R")

# Run the app
run_app()

# After adding a dependency with usethis::use_package(), record it:
renv::snapshot()
