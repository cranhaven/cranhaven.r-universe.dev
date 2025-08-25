p <- measure_change_over_time_long(ggplot2::economics_long, date, variable, value, pop, unemploy)
test_that("Plot layers match expectations",{
  expect_is(p$layers[[1]], "ggproto")
})

test_that("Plot returns ggplot object",{
  expect_is(p, "ggplot")
})

test_that("Plot uses correct data",{
  expect_equal(TRUE, all(c("date", "variable", "value") %in% names(p$data)))
})
test_that("x axis is labeled 'date'",{
  expect_match(p$labels$x, "date")
})

test_that("y axis is labeled 'value'",{
  expect_match(p$labels$y, "value")
})


