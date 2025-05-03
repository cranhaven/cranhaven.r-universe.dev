setDTthreads(1)

fn1 <- system.file("extdata", "test1.nc", package = "eurocordexr")
fn2 <- system.file("extdata", "test2.nc", package = "eurocordexr")


test_that("basic functionality", {
  expect_equal(nrow(rotpole_nc_point_to_dt(fn1, point_lon = 11.31, point_lat = 46.5)), 365)
  expect_named(rotpole_nc_point_to_dt(fn1, point_lon = 11.31, point_lat = 46.5, add_grid_coord = T),
               c("date", "tasmin", "grid_lon", "grid_lat"))
  expect_equal(nrow(rotpole_nc_point_to_dt(fn2, point_lon = 11.31, point_lat = 46.5)), 390)
  expect_equal(nrow(rotpole_nc_point_to_dt(fn2, point_lon = 11.31, point_lat = 46.5,
                                           interpolate_to_standard_calendar = T)), 395)
})
