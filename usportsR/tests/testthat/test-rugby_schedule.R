test_that("rugby_schedule() test", {
  schedule <- rugby_schedule("w")
  expect_type(schedule , "list")
  expect_true(nrow(schedule) != 0)
})
