setDTthreads(1)

fn1 <- system.file("extdata", "test1.nc", package = "eurocordexr")

test_that("get_varnames works", {
  expect_identical(get_varnames(fn1), "tasmin")
})

