# create test data frame
df <- data.frame(AICc = c(10, 12, 15, 20), max_vif = c(2, 4, 5, 6))

test_that("select_models returns a data frame", {
  expect_s3_class(select_models(df), "data.frame")
})

test_that("select_models returns correct number of rows with delta_aicc = 2", {
  expect_equal(nrow(select_models(df, delta_aicc = 2)), 2)
})

test_that("select_models returns correct number of rows with delta_aicc = 5", {
  expect_equal(nrow(select_models(df, delta_aicc = 5)), 3)
})


test_that("select_models returns empty data frame when no models meet criteria", {
  df2 <- data.frame(AICc = c(-10, -5, -8, -20), max_vif = c(10, 11, 12, 13))
  capture_warnings(Rows <- nrow(select_models(df2)))
  expect_equal(Rows, 0)
})
