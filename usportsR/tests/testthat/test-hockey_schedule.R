test_that("hockey_schedule() test", {
  schedule <- hockey_schedule("w")
  expect_type(schedule,"list")
  expect_true(nrow(schedule) != 0)
})
