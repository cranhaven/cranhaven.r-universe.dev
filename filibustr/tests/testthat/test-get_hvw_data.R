test_that("HVW house data", {
  house_data <- get_hvw_data("house")

  # data checks
  expect_s3_class(house_data, "tbl_df")
  expect_length(house_data, 109)
  expect_equal(nrow(house_data), 9825)
  expect_equal(unique(house_data$congress), 93:114)
  expect_equal(unique(house_data$year), c(seq(1973, 2015, 2), NA))

  # check chamber argument
  expect_equal(house_data, get_hvw_data("h"))
  expect_equal(house_data, get_hvw_data("hr"))
})

test_that("HVW senate data", {
  senate_data <- get_hvw_data("senate")

  # data checks
  expect_s3_class(senate_data, "tbl_df")
  expect_length(senate_data, 104)
  expect_equal(nrow(senate_data), 2228)
  expect_equal(unique(senate_data$congress), 93:114)
  expect_equal(unique(senate_data$year), seq(1972, 2014, 2))

  # check chamber argument
  expect_equal(senate_data, get_hvw_data("s"))
  expect_equal(senate_data, get_hvw_data("sen"))
})

test_that("HVW chamber errors", {
  expect_error(get_hvw_data("all"))
  expect_error(get_hvw_data("congress"))
  expect_error(get_hvw_data(), "argument \"chamber\" is missing, with no default")
})
