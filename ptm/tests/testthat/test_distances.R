library(ptm)

## ---------------------------------------------- ##
#            Testing pairwise.dist                 #
## ---------------------------------------------- ##
test_that('pairwise.dist() works properly',{

  a <- matrix(runif(30*3), ncol=3)
  b <- matrix(runif(10*3), ncol=3)
  M <- pairwise.dist(a, b, square = FALSE)
  a <- matrix(c(1,1,1,1,0,0,1,0,1), ncol = 3, byrow = TRUE)
  b <- matrix(c(0,0,0,1,1,1), ncol = 3, byrow = TRUE)
  D <- pairwise.dist(a, b, square = FALSE)

  expect_equal(dim(M), c(30,10))
  expect_equal(dim(D), c(3,2))
  expect_equivalent(D, matrix(c(sqrt(3), 0, 1, sqrt(2), sqrt(2), 1), ncol = 2, byrow = TRUE))
})
