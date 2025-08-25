p <- measure_distribution_by_two_categories(ggplot2::mpg, hwy, class, fl)
test_that("Plot layers match expectations",{
  expect_is(p$layers[[1]], "ggproto")
})

test_that("Plot returns ggplot object",{
  expect_is(p, "ggplot")
})

test_that("Plot uses correct data",{
  expect_that(length(names(p$data)), equals(11))
})

test_that("x axis label is hwy",{
  expect_match(p$labels$x, "hwy")
})

test_that("y axis label is correct",{
  expect_match(p$labels$y, "count")
})

test_that("Facet type is correct", {
  expect_match(class(p$facet)[[1]], "FacetGrid")
})



