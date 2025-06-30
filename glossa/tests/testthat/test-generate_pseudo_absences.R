library(testthat)
library(terra)
library(sf)
library(glossa)  # or source manually if not installed

# Simulated time-indexed raster stacks
raster_stack <- lapply(1:3, function(i) {
  r1 <- rast(nrows = 10, ncols = 10, xmin = 0, xmax = 10, ymin = 0, ymax = 10)
  values(r1) <- i

  r2 <- rast(nrows = 10, ncols = 10, xmin = 0, xmax = 10, ymin = 0, ymax = 10)
  values(r2) <- i

  s <- c(r1, r2)
  names(s) <- c("var1", "var2")
  return(s)
})

presences <- data.frame(
  decimalLongitude = runif(5, 2, 8),
  decimalLatitude = runif(5, 2, 8),
  timestamp = sample(1:3, 5, replace = TRUE),
  pa = 1
)
presences[, c("timestamp_original", "var1", "var2")] <- presences$timestamp

target_group <- data.frame(
  decimalLongitude = runif(30, 1, 9),
  decimalLatitude = runif(30, 1, 9),
  timestamp = sample(1:3, 30, replace = TRUE)
)

study_area <- st_as_sfc(st_bbox(c(xmin=0, xmax=10, ymin=0, ymax=10), crs=4326))

test_that("generate_pseudo_absences() random returns correct results", {
  res <- suppressMessages(generate_pseudo_absences(
    method = "random",
    presences = presences,
    raster_stack = raster_stack,
    predictor_variables = c("var1", "var2"),
    study_area = study_area,
    ratio = 1,
    seed = 123
  ))

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 10)
  expect_true(all(c("var1", "var2", "timestamp", "pa") %in% colnames(res)))
  expect_equal(sum(res$pa == 0), 5)
  expect_equal(sum(res$pa == 1), 5)

  #plot(raster_stack[[1]][[1]], main = "Random")
  #points(res[res$pa == 1, c("decimalLongitude", "decimalLatitude")], col = "blue", pch = 20)
  #points(res[res$pa == 0, c("decimalLongitude", "decimalLatitude")], col = "red", pch = 3)
})

test_that("generate_pseudo_absences() target_group returns valid output", {
  res <- suppressMessages(generate_pseudo_absences(
    method = "target_group",
    presences = presences,
    raster_stack = raster_stack,
    predictor_variables = c("var1", "var2"),
    target_group_points = target_group,
    study_area = study_area,
    ratio = 1,
    seed = 123
  ))

  expect_equal(nrow(res), 10)
  expect_true(all(res$pa %in% c(0, 1)))
  expect_true(all(complete.cases(res[, c("var1", "var2")])))

  #plot(raster_stack[[1]][[1]], main = "Target-group")
  #points(res[res$pa == 1, c("decimalLongitude", "decimalLatitude")], col = "blue", pch = 20)
  #points(res[res$pa == 0, c("decimalLongitude", "decimalLatitude")], col = "red", pch = 3)
})

test_that("generate_pseudo_absences() buffer_out enforces buffer", {
  res <- suppressWarnings(suppressMessages(generate_pseudo_absences(
    method = "buffer_out",
    presences = presences,
    raster_stack = raster_stack,
    predictor_variables = c("var1", "var2"),
    pa_buffer_distance = 0.3,
    ratio = 1,
    seed = 123
  )))

  expect_equal(nrow(res), 10)
  expect_true(all(res$pa %in% c(0, 1)))

  #plot(raster_stack[[1]][[1]], main = "Buffer")
  #points(res[res$pa == 1, c("decimalLongitude", "decimalLatitude")], col = "blue", pch = 20)
  #points(res[res$pa == 0, c("decimalLongitude", "decimalLatitude")], col = "red", pch = 3)
})

test_that("Wrapper fails with invalid method", {
  expect_error(
    generate_pseudo_absences(
      method = "not_a_method",
      presences = presences,
      raster_stack = raster_stack,
      predictor_variables = c("var1", "var2"),
    ),
    "should be one of"
  )
})

test_that("Target-group method fails without target_group_points", {
  expect_error(
    generate_pseudo_absences(
      method = "target_group",
      presences = presences,
      raster_stack = raster_stack,
      predictor_variables = c("var1", "var2"),
    ),
    "Target group occurrence data must be provided"
  )
})
