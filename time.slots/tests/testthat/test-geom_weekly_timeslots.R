test_that("null geom_weekly_timeslots works", {
  expect_s3_class(geom_weekly_timeslots(data = SlotsDetails()), "ggplot")
  expect_s3_class(geom_weekly_timeslots(data = mock_data), "ggplot")
  expect_error(geom_weekly_timeslots(data = mtcars))
})
