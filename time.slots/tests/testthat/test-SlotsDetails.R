test_that("SlotsDetails Null Case Works", {
  expect_s3_class(SlotsDetails(), "data.frame")
  expect_setequal(colnames(SlotsDetails()), c("start_datetime", "end_datetime", "title", "body"))
  expect_equal(nrow(SlotsDetails()), 0)
})

test_that("SlotsDetails works with valid data", {
  expect_s3_class(SlotsDetails(
    start_datetime = mock_data$start_datetime, end_datetime = mock_data$end_datetime,
    title = mock_data$title, body = mock_data$body
  ), "data.frame")
})
