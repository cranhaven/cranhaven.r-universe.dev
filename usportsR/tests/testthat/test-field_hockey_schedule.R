test_that("fh schedule", {
  schedule <- field_hockey_schedule()
  expect_type(schedule,"list")
  expect_true(nrow(schedule) != 0)
})
