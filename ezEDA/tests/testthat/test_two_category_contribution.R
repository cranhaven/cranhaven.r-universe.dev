p <- two_category_contribution(ggplot2::diamonds,  clarity, cut, price, separate = TRUE)
test_that("Plot layers match expectations",{
  expect_is(p$layers[[1]], "ggproto")
})

test_that("Plot returns ggplot object",{
  expect_is(p, "ggplot")
})

test_that("Plot uses correct data",{
  expect_equal(TRUE, all(c("clarity", "cut", "total_price") %in% names(p$data)))
})

test_that("x axis is labeled 'clarity'",{
  expect_match(p$labels$x, "clarity")
})

test_that("y axis is labeled 'total_price'",{
  expect_identical(p$labels$y, "total_price")
})


