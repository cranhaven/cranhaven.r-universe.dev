test_that("testing football_drive_summaries()", {
  drive_summaries_2016 <- football_drive_summaries(2016)
  expect_type(drive_summaries_2016, "list")
  expect_true (nrow(drive_summaries_2016) != 0)
})
