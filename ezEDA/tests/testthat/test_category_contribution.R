p <- category_contribution(ggplot2::diamonds, cut, price)
test_that("Plot layers match expectations",{
  expect_is(p$layers[[1]], "ggproto")
})

test_that("Plot returns ggplot object",{
  expect_is(p, "ggplot")
})

test_that("Plot uses correct data",{
  expect_that(names(p$data), equals(c("cut", "total_price")))
})

test_that("x axis is labeled 'cut'",{
  expect_match(p$labels$x, "cut")
})


test_that("y axis is labeled 'total_price' -- 2",{
  expect_match(p$labels$y, "total_price")
})


