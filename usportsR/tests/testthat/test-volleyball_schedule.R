test_that("volleyball_schedule() test", {
  schedule <- volleyball_schedule("m")
  expect_type(schedule,"list")
  expect_true(nrow(schedule) != 0)

})
