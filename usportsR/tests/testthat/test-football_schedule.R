test_that("football_schedule() test", {
  schedule <- football_schedule()
  expect_type(schedule , "list")
  expect_true(nrow(schedule) != 0)
})
