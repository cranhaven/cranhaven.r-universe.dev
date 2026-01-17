test_that("plot_direction returns ggplot object for all plot types", {
  expect_s3_class(plot_direction(MountTom, plot_type = "boxplot"), "gg")
  expect_s3_class(plot_direction(MountTom, plot_type = "polar_steps"), "gg")
  expect_s3_class(plot_direction(MountTom, plot_type = "polar_average"), "gg")
  expect_s3_class(plot_direction(MountTom, plot_type = "faceted"), "gg")
})

test_that("plot_direction handles custom parameters correctly", {
  expect_s3_class(plot_direction(MountTom, plot_type = "polar_steps", angle_range = 45), "gg")
  expect_s3_class(plot_direction(MountTom, plot_type = "polar_steps", y_labels_position = -45), "gg")
  expect_s3_class(plot_direction(MountTom, plot_type = "polar_steps", y_breaks_manual = c(0, 10, 20)), "gg")
})

test_that("plot_direction warns when extra parameters are passed to boxplot", {
  expect_warning(plot_direction(MountTom, plot_type = "boxplot", angle_range = 60), "not used")
})

test_that("plot_direction throws errors on invalid inputs", {
  bad_data <- list(Trajectories = list(), Footprints = list())
  expect_error(plot_direction(NULL), "must be a 'track' R object")
  expect_error(plot_direction(list(1, 2)), "must be lists")
  expect_error(plot_direction(bad_data), "must not be empty")
  expect_error(plot_direction(MountTom, plot_type = "invalid_type"), "must be one of")
  expect_error(plot_direction(MountTom, angle_range = -10), "between 1 and 180")
  expect_error(plot_direction(MountTom, y_labels_position = 200), "between -180 and 180")
  expect_error(plot_direction(MountTom, y_breaks_manual = c(-1, -2)), "positive values")
})
