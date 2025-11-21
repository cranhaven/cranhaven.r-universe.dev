library(shinytest2)

test_that("ReliaShiny app works", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()

  appdir <- system.file(package = "ReliaShiny", "app")
  test_app(appdir)
})
