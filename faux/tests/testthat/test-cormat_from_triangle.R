context("test-cormat_from_triangle")

test_that("errors", {
  expect_error(
    cormat_from_triangle(c(.8, .8, .8, .8)),
    "you don't have the right number of correlations"
  )
})

test_that("from triangle", {
  mat <- cormat_from_triangle(c(.2, .3, .4, .5, .6, .7))
  
  compmat <- matrix(c(1, .2, .3, .4,
                      .2,  1, .5, .6, 
                      .3, .5,  1, .7,
                      .4, .6, .7,  1), 4)
  expect_equal(mat, compmat)
})

