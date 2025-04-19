testthat::skip_on_cran()
testthat::skip_if_offline(host = "r-project.org")

try(initialization_sgat(), silent = TRUE)

test_that("gives correct data frame if information is found", {
  skip("prueba")
  skip_on_cran()
  skip_if_offline(host = "r-project.org")
  coutume.martes <- sgat_day("coutume, 47 rue de babylone, 75007 paris, france", "martes", tiempo.espera = 20)
  expect_s3_class(coutume.martes, "data.frame")
  expect_identical(unique(coutume.martes$lugar), "coutume, 47 rue de babylone, 75007 paris, france")
  expect_identical(unique(coutume.martes$dia), "martes")
  expect_lt(max(coutume.martes$hora), 24)
  expect_gt(min(coutume.martes$hora), 0)
  expect_type(coutume.martes$latitud, "double")
  expect_type(coutume.martes$longitud, "double")
})

test_that("gives message if information is not found", {
  skip("prueba")
  skip_on_cran()
  skip_if_offline(host = "r-project.org")
  expect_identical(sgat_day("Moustacchio, San Martín 298, Ushuaia, Argentina", "jueves"), "Sin datos de concurrencia")
})

test_that("error if lugar.a.buscar and or dia.semana is not typed", {
  if(!exists("remDr")){
    remDr <- 1
  }
  expect_error(sgat_day(dia.semana = "viernes"))
  expect_error(sgat_day(lugar.a.buscar = "coutume, 47 rue de babylone, 75007 paris, france"))
})
