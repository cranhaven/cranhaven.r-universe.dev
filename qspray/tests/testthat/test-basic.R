test_that("simple expansion", {
  x <- qlone(1)
  y <- qlone(2)
  expect_true((x - y)^2 == x^2 - 2*x*y + y^2)
})

test_that("power", {
  x <- qlone(1)
  y <- qlone(2)
  P <- 2L*x*y - 3L*x - 4L*y 
  expect_true(P^7 == P*P*P*P*P*P*P)
})

test_that("commutativity", {
  x <- qlone(1)
  y <- qlone(2)
  expect_true(x*y == y*x)  
})

test_that("associativity", {
  x <- qlone(1)
  y <- qlone(2)
  z <- qlone(3)
  expect_true(x*(y*z) == (x*y)*z)  
})

test_that("distributivity", {
  x <- qlone(1)
  y <- qlone(2)
  z <- qlone(3)
  expect_true(x*(y+z) == x*y + x*z)
})