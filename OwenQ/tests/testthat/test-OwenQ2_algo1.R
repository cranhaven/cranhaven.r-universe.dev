context("OwenQ2 algo1")

algo <- 1

test_that("OwenQ2 for R=0 equals ptOwen", {
  skip_on_os("mac")
  owenQ2 <- sapply(1:8, function(nu) OwenQ2(nu=nu, t=2, delta=2, R=0, algo))
  owenS <- sapply(1:8, function(nu) ptOwen(q=2, nu=nu, delta=2))
  expect_equal(owenQ2, owenS, tolerance=1e-15)
})

test_that("OwenQ2 is correctly vectorized", {
  delta <- c(Inf, 0, -Inf); R <- c(1,2,3)
  nu <- 5; t <- 2
  expect_identical(OwenQ2(nu, t, delta, R, algo),
                   c(OwenQ2(nu, t, delta[1], R[1], algo),
                     OwenQ2(nu, t, delta[2], R[2], algo),
                     OwenQ2(nu, t, delta[3], R[3], algo)))
})

test_that("OwenQ1+OwenQ2 equals ptOwen", {
  owenQ2 <- sapply(1:8,
                   function(nu){
                     OwenQ1(nu=nu, t=2, delta=2, R=1, algo) +
                       OwenQ2(nu=nu, t=2, delta=2, R=1, algo)
                     })
  owenS <- sapply(1:8, function(nu) ptOwen(q=2, nu=nu, delta=2))
  expect_equal(owenQ2, owenS, tolerance=1e-15)
})

test_that("OwenQ2 for t=+Inf does not depend on delta", {
  expect_true(OwenQ2(5, Inf, 2, 2) == OwenQ2(5, Inf, 3, 2))
  expect_true(OwenQ2(6, Inf, 2, 2) == OwenQ2(6, Inf, 3, 2))
  # does not depend on t for delta=-Inf and the same result
  expect_equal(OwenQ2(5, Inf, 2, 2), OwenQ2(5, 1, -100, 2, algo), tolerance=1e-15)
  expect_equal(OwenQ2(6, Inf, 2, 2), OwenQ2(6, 1, -100, 2, algo), tolerance=1e-15)
  # now delta=-Inf is implemented
  expect_equal(OwenQ2(5, Inf, 2, 2), OwenQ2(5, 1, -Inf, 2), tolerance=1e-15)
  expect_equal(OwenQ2(6, Inf, 2, 2), OwenQ2(6, 1, -Inf, 2), tolerance=1e-15)
})

test_that("OwenQ2 for delta=Inf", {
  expect_true(OwenQ2(5, 1, Inf, 2) == 0)
  expect_equal(OwenQ2(5, 1, 100, 2, algo), 0, tolerance=1e-15)
})

test_that("OwenQ2 for t=-Inf equals 0", {
  expect_equal(OwenQ2(5, -Inf, 2, 1), 0, tolerance=1e-16)
  expect_true(OwenQ2(6, -Inf, 2, 1) == 0)
})

test_that("OwenQ2 for |t|=Inf and R=0", {
  expect_true(OwenQ2(1, Inf, 2, 0) == 1)
  expect_true(OwenQ2(1, -Inf, 2, 0) == 0)
})


test_that("OwenQ2 - bivariate Student", {
  t1 <- 2; t2 <- 1; delta1 <- 3; delta2 <- 2
  nu <- 6
  R <- sqrt(nu)*(delta1 - delta2)/(t1-t2)
  owen <- - OwenQ2(nu, t2, delta2, R, algo) + OwenQ2(nu, t1, delta1, R, algo)
  wolfram <- 0.03257737810540227
  expect_equal(owen, wolfram, tolerance=1e-10)
  #
  nu <- 5
  R <- sqrt(nu)*(delta1 - delta2)/(t1-t2)
  owen <- - OwenQ2(nu, t2, delta2, R, algo) + OwenQ2(nu, t1, delta1, R, algo)
  wolfram <- 0.0353568969628651
  expect_equal(owen, wolfram, tolerance=1e-9)
})

test_that("OwenQ2 - numerical integration", {
  wolfram <- c(5.7330314375838782335e-7, 3.7266531720786709899e-6, 0.000015440498291040247642, 0.000050309817486816081076, 0.00013933372105156197162, 0.00034145176657217697536, 0.00075875604691111263929, 0.0015541802819728694091, 0.0029690444288361936486, 0.0053365202327988767708)
  owen <- sapply(1:10, function(nu) OwenQ2(nu, 3, 2, 5, algo))
  expect_equal(owen, wolfram, tolerance=1e-14)
})
