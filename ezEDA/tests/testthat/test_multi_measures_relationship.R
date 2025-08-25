p <- multi_measures_relationship(ggplot2::mpg, cty, hwy, displ)

test_that("Plot returns gg object",{
  expect_that(class(p)[[1]], equals("gg"))
})

test_that("Plot uses correct data",{
  expect_that(length(names(p$data)), equals(3))
})

test_that("9 plots are produced",{
  expect_that(length(p$plots), equals(9))
})

