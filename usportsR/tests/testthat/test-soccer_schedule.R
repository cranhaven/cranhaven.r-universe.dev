test_that("soccer_schedule() test", {
  schedule <- soccer_schedule("m")
  expect_type(schedule, "list")
  expect_true(nrow(schedule) != 0)
})
