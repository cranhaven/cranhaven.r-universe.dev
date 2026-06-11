test_that("blank theme works", {
  library(ggplot2)
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + theme_rw()
  expect_true(inherits(p$theme$plot.background, 'element_blank'))
  expect_true(inherits(p$theme$panel.grid.major, 'element_blank'))
  expect_true(inherits(p$theme$panel.grid.minor, 'element_blank'))
  expect_true(inherits(p$theme$panel.border, 'element_blank'))
})
