setDTthreads(1)

fn3 <- system.file("extdata", "test3.nc", package = "eurocordexr")

test_that("normal version does not work", {
  expect_error(nc_grid_to_dt(fn3))
})


test_that("basic functionality", {
  expect_error(nc_grid_to_dt_raw(fn3), "variable", fixed = T)
  expect_equal(nrow(nc_grid_to_dt_raw(fn3, "tasmax")), 51660)
  expect_named(nc_grid_to_dt_raw(fn3, "tasmax", var_t = "Times"),
               c("icell", "Times", "tasmax"))
  expect_named(nc_grid_to_dt_raw(fn3, "tasmax", var_t = "Times", var_x = "XLONG", var_y = "XLAT"),
               c("icell", "Times", "tasmax", "XLONG", "XLAT"))
})

fn1 <- system.file("extdata", "test1.nc", package = "eurocordexr")

test_that("works with standard files too", {
  expect_equal(nrow(nc_grid_to_dt_raw(fn1, "tasmin", var_t = "time")), 25550)
  expect_named(nc_grid_to_dt_raw(fn1, "tasmin", var_t = "time"), c("icell", "time", "tasmin"))
  expect_named(nc_grid_to_dt_raw(fn1, "tasmin", var_t = "time", var_x = "lon", var_y = "lat"),
               c("icell", "time", "tasmin", "lon", "lat"))
})
