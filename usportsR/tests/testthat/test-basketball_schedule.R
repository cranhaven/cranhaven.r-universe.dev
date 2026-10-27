test_that("basketball_schedule() works", {
  schedule <- basketball_schedule("m")
  expect_true(nrow(schedule) != 0)
})
