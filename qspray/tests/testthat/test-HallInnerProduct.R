test_that("Hall inner product", {
  t <- gmp::as.bigq(2L)
  poly <- PSFpoly(4, c(4))
  h1 <- HallInnerProduct(poly, poly, alpha = t)
  poly <- PSFpoly(4, c(3,1))
  h2 <- HallInnerProduct(poly, poly, alpha = t)
  poly <- PSFpoly(4, c(2,2))
  h3 <- HallInnerProduct(poly, poly, alpha = t)
  poly <- PSFpoly(4, c(2,1,1))
  h4 <- HallInnerProduct(poly, poly, alpha = t)
  poly <- PSFpoly(4, c(1,1,1,1))
  h5 <- HallInnerProduct(poly, poly, alpha = t)
  expect_identical(
    c(h1, h2, h3, h4, h5), c(4*t, 3*t^2, 8*t^2, 4*t^3, 24*t^4)
  )
})

test_that("Symbolic Hall inner product", {
  poly <- PSFpoly(4, c(4))
  h1 <- HallInnerProduct(poly, poly, alpha = NULL)
  poly <- PSFpoly(4, c(3,1))
  h2 <- HallInnerProduct(poly, poly, alpha = NULL)
  poly <- PSFpoly(4, c(2,2))
  h3 <- HallInnerProduct(poly, poly, alpha = NULL)
  poly <- PSFpoly(4, c(2,1,1))
  h4 <- HallInnerProduct(poly, poly, alpha = NULL)
  poly <- PSFpoly(4, c(1,1,1,1))
  h5 <- HallInnerProduct(poly, poly, alpha = NULL)
  t <- qlone(1)
  expected <- list(4*t, 3*t^2, 8*t^2, 4*t^3, 24*t^4)
  tests <- mapply(
    function(x, y) x == y, 
    list(h1, h2, h3, h4, h5),
    expected,
    SIMPLIFY = TRUE
  )
  expect_true(all(tests))
  # check the show option
  expect_equal(Print(h2), "3*alpha^2 ")
})