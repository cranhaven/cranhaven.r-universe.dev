setDTthreads(1)

fn1 <- system.file("extdata", "test1.nc", package = "eurocordexr")

test_that("basic functionality", {
  expect_equal(nrow(nc_grid_to_dt(fn1)), 25550)
  expect_named(nc_grid_to_dt(fn1), c("icell", "date", "tasmin"))
  expect_s3_class(nc_grid_to_dt(fn1)$date, "Date")
})


test_that("coordinates argument", {
  expect_named(nc_grid_to_dt(fn1, add_xy = TRUE), c("icell", "date", "tasmin", "rlon", "rlat"))
  expect_type(nc_grid_to_dt(fn1, add_xy = TRUE)$rlon, "double")
  expect_type(nc_grid_to_dt(fn1, add_xy = TRUE)$rlat, "double")
})

test_that("date_range argument", {
  expect_equal(nrow(nc_grid_to_dt(fn1, date_range = c("1950-04-01", "1950-04-30"))), 2100)
  expect_equal(nrow(nc_grid_to_dt(fn1, date_range = c("1950-12-31", "1970-04-30"))), 70)
  expect_error(nc_grid_to_dt(fn1, date_range = c("1950-04-01", "1950-03-30")),
               "date_range[1] <= date_range[2]", fixed = T)
})

# 360 calendar
fn2 <- system.file("extdata", "test2.nc", package = "eurocordexr")

test_that("basic non-standard calendar functionality", {
  expect_type(nc_grid_to_dt(fn2)$date, "character")
  expect_s3_class(nc_grid_to_dt(fn2, interpolate_to_standard_calendar = T)$date, "Date")
  expect_equal(any(is.na(nc_grid_to_dt(fn2, interpolate_to_standard_calendar = T)$date)), FALSE)
})

test_that("non-standard calendar with other arguments", {
  expect_type(nc_grid_to_dt(fn2, date_range = c("1950-10-01", "1950-10-31"))$date, "character")
  expect_s3_class(nc_grid_to_dt(fn2, date_range = c("1950-10-01", "1950-10-31"),
                                interpolate_to_standard_calendar = T)$date, "Date")
  expect_equal(any(is.na(nc_grid_to_dt(fn2, date_range = c("1950-10-01", "1950-10-31"),
                                       interpolate_to_standard_calendar = T)$date)), FALSE)
  expect_equal(any(is.na(nc_grid_to_dt(fn2, date_range = c("1950-10-01", "1950-10-31"), add_xy = T,
                                       interpolate_to_standard_calendar = T))), FALSE)
})

test_that("consistency of calendar interpolation", {
  expect_identical(nc_grid_to_dt(fn1, interpolate_to_standard_calendar = T),
                   nc_grid_to_dt(fn1, interpolate_to_standard_calendar = F))
  expect_gte(nrow(nc_grid_to_dt(fn2, interpolate_to_standard_calendar = T)),
             nrow(nc_grid_to_dt(fn2, interpolate_to_standard_calendar = F)))
})

