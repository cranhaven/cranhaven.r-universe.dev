testthat::skip_on_cran()
testthat::skip_if_offline(host = "r-project.org")

try(initialization_sgat(), silent = TRUE)

test_that("gives correct data frame if information is found", {
  skip_if(!exists("remDr"), "Server not initialized")
  skip_on_cran()
  skip_if_offline(host = "r-project.org")
  coutume <- sgat("coutume, 47 rue de babylone, 75007 paris, france", tiempo.espera = 20)
  expect_s3_class(coutume, "data.frame")
  expect_identical(unique(coutume$lugar), "coutume, 47 rue de babylone, 75007 paris, france")
  expect_lt(max(coutume$hora), 24)
  expect_gt(min(coutume$hora), 0)
  expect_type(coutume$latitud, "double")
  expect_type(coutume$longitud, "double")
})

test_that("gives NULL if information is not found", {
  skip_if(!exists("remDr"), "Server not initialized")
  skip_on_cran()
  skip_if_offline(host = "r-project.org")
  expect_null(moustacchio <- sgat("Moustacchio, San Martín 298, Ushuaia, Argentina"))
})

test_that("error if no lugar.a.buscar and or dia.seman typed", {
  if(!exists("remDr")){
    remDr <- 1
  }
  expect_error(sgat())
})
