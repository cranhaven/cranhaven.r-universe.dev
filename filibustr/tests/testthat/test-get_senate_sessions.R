test_that("senate sessions", {
  skip_if_offline()

  session_dates <- get_senate_sessions()
  expect_s3_class(session_dates, "tbl_df")
  expect_length(session_dates, 4)

  # length: grows by 2 in every Congress, so this will be -1 or 0
  # (may be -2 in first few days of odd years before Congress starts)
  expect_equal(nrow(dplyr::filter(session_dates, congress <= 117)), 306)
  expect_gte(nrow(session_dates) - 306 - 2*(current_congress() - 117), -2)
  expect_lte(nrow(session_dates) - 306 - 2*(current_congress() - 117), 0)

  # check column types/values
  expect_equal(unique(session_dates$congress), current_congress():1)
  expect_equal(levels(session_dates$session), c("1", "2", "3", "4", "S"))
  expect_s3_class(session_dates$begin_date, "Date")
  expect_s3_class(session_dates$adjourn_date, "Date")
  # dates of first session of the Senate
  expect_equal(min(session_dates$begin_date), as.Date("1789-03-04"))
  expect_equal(min(session_dates$adjourn_date, na.rm = TRUE), as.Date("1789-09-29"))
})
