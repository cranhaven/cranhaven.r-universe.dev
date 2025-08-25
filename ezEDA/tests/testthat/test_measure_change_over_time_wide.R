p <- measure_change_over_time_wide(ggplot2::economics, date, pop, unemploy)
test_that("Plot layers match expectations",{
  expect_is(p$layers[[1]], "ggproto")
})

test_that("Plot returns ggplot object",{
  expect_is(p, "ggplot")
})

test_that("Plot uses correct data",{
  expect_that(names(p$data), equals(c("date", "series_type", "value")))
})

test_that("x axis is labeled 'date'",{
  expect_match(p$labels$x, "date")
})

test_that("y axis is labeled 'value'",{
  expect_match(p$labels$y, "value")
})


