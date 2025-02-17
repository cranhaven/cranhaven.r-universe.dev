context("OwenQ1 algo2")

algo <- 2

test_that("OwenQ1 for large R equals ptOwen", {
  expect_equal(OwenQ1(4, 3, 2, 100, algo), ptOwen(3, 4, 2), tolerance=1e-15)
  expect_equal(OwenQ1(5, 3, 2, 100, algo), ptOwen(3, 5, 2), tolerance=1e-10)
  expect_equal(OwenQ1(5, 3, 2, 100, algo), pt(3, 5, 2), tolerance=1e-10)
})

test_that("OwenQ1 is correctly vectorized", {
  delta <- c(Inf, 0, -Inf); R <- c(1,2,3)
  nu <- 5; t <- 2
  expect_identical(OwenQ1(nu, t, delta, R),
                   c(OwenQ1(nu, t, delta[1], R[1]), OwenQ1(nu, t, delta[2], R[2]),
                     OwenQ1(nu, t, delta[3], R[3])))
})

test_that("OwenQ1 for t=+Inf does not depend on delta", {
  expect_true(OwenQ1(5, Inf, 2, 2) == OwenQ1(5, Inf, 3, 2))
  expect_equal(OwenQ1(6, Inf, 2, 2), OwenQ1(6, Inf, 3, 2), tolerance=1e-16)
  # nu=2 => moment truncated normal
  expect_equal(OwenQ1(2, Inf, 1, 2), sqrt(2*pi)*(dnorm(0)-dnorm(2)), tolerance=1e-15)
  # nu >=1 => incomplete Gamma
  R <- 2; nu <- 6
  expect_equal(OwenQ1(nu, Inf, 3, R), pgamma(R^2/2, nu/2),
               tolerance=1e-14)
  # does not depend on t for delta=-Inf and the same result
  expect_equal(OwenQ1(5, Inf, 2, 2), OwenQ1(5, 1, -100, 2, algo), tolerance=1e-15)
  expect_equal(OwenQ1(6, Inf, 2, 2), OwenQ1(6, 1, -100, 2, algo), tolerance=1e-15)
  expect_equal(OwenQ1(1, Inf, 2, 2), OwenQ:::RcppOwenQ1(1, 1, -Inf, 2, algo), tolerance=1e-15)
  expect_equal(OwenQ1(2, Inf, 2, 2), OwenQ:::RcppOwenQ1(2, 1, -Inf, 2, algo), tolerance=1e-15)
  # now delta=-Inf is implemented
  expect_equal(OwenQ1(5, Inf, 2, 2), OwenQ1(5, 1, -Inf, 2), tolerance=1e-15)
  expect_equal(OwenQ1(6, Inf, 2, 2), OwenQ1(6, 1, -Inf, 2), tolerance=1e-15)
})

test_that("OwenQ1 for t=-Inf equals 0", {
  expect_true(OwenQ1(5, -Inf, 2, 100) == 0)
  expect_equal(OwenQ1(6, -Inf, 2, 100), 0, tolerance=1e-17)
})

test_that("OwenQ1 - comparison Wolfram", {
  owen <- OwenQ1(1, 3, 2, 1, algo)
  wolfram <- 0.219018703856082
  expect_equal(owen, wolfram, tolerance=1e-15)
  wolfram <- c(0.52485658843054409291,0.62938193306526904118,0.68001173355723140333,0.71048916647247223585,
               0.73097050978732740047,0.74563903448183001384,0.75650064658430460294,0.76456656856157965989,
               0.77030437685878389173,0.77383547873740988537)
  owen <- sapply(1:10, function(nu) OwenQ1(nu, 3, 2, 5, algo))
  expect_equal(owen, wolfram, tolerance=1e-15)
})

test_that("OwenQ1 - bivariate Student", {
  t1 <- 2; t2 <- 1; delta1 <- 3; delta2 <- 2
  nu <- 6
  R <- sqrt(nu)*(delta1 - delta2)/(t1-t2)
  owen <- OwenQ1(nu, t2, delta2, R, algo) - OwenQ1(nu, t1, delta1, R, algo)
  pmvt <- mvtnorm::pmvt(lower=c(t1,-Inf), upper=c(Inf,t2), delta=c(delta1, delta2),
                df=nu, corr=cbind(c(1,1),c(1,1)))
  expect_equal(owen, pmvt[1], tolerance=1e-4, check.attributes=FALSE)
  wolfram <- 0.01785518085912236
  expect_equal(owen, wolfram, tolerance=1e-11)
  #
  nu <- 5
  R <- sqrt(nu)*(delta1 - delta2)/(t1-t2)
  owen <- OwenQ1(nu, t2, delta2, R, algo) - OwenQ1(nu, t1, delta1, R, algo)
  wolfram <- 0.01868982415809893
  expect_equal(owen, wolfram, tolerance=1e-9)
  # wolfram input:
  # With[{t1 = 2, t2=1,delta1=3, delta2=2, nu=6},
  #      NProbability[
  #        (x+delta1)/Sqrt[y/nu] >= t1 && (x+delta2)/Sqrt[y/nu] <=t2, {x \[Distributed]
  #          NormalDistribution[], y\[Distributed]ChiSquareDistribution[nu]}]]
})

test_that("OwenQ1 - bivariate Student", {
  t1 <- 2; t2 <- 1; delta1 <- 3; delta2 <- 2
  nu <- 6
  R <- sqrt(nu)*(delta1 - delta2)/(t1-t2)
  owen <- - (ptOwen(t2, nu, delta2) - OwenQ1(nu, t2, delta2, R, algo)) +
             (ptOwen(t1, nu, delta1) - OwenQ1(nu, t1, delta1, R, algo))
  wolfram <- 0.03257737810540227
  expect_equal(owen, wolfram, tolerance=1e-10)
  #
  nu <- 5
  R <- sqrt(nu)*(delta1 - delta2)/(t1-t2)
  owen <- - (ptOwen(t2, nu, delta2) - OwenQ1(nu, t2, delta2, R, algo)) +
    (ptOwen(t1, nu, delta1) - OwenQ1(nu, t1, delta1, R, algo))
  wolfram <- 0.0353568969628651
  expect_equal(owen, wolfram, tolerance=1e-9)
})



