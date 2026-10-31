context("Test TRR and TPR with .sim function, bat and square.")

testthat::skip('skip')
RNGkind("L'Ecuyer-CMRG")

test_that("TRR works with .sim function", {
  set.seed(1)
  r <- c(10, 10, 10)
  u <- c(2, 2, 2)
  p <- 5
  n <- 100
  dat <- TRRsim(r = r, p = p, u = u, n = n)
  x <- dat$x
  y <- dat$y
  B <- dat$coefficients
  fit_std <- TRR.fit(x, y, method="standard")
  fit_1D <- TRR.fit(x, y, u, method="1D")
  expect_equal(fit_std$fitted.values@data[1:5,2,2,2], c(-0.35577737,  0.49003598,  0.02182803,  0.10934437,  0.28544817))
  expect_equal(fit_1D$fitted.values@data[1:5,2,2,2], c(-0.047196540, -0.056589661,  0.016332660, -0.014613523,  0.003886516))
  expect_equal(rTensor::fnorm(B-stats::coef(fit_std)), 10.104526002)
  expect_equal(rTensor::fnorm(B-stats::coef(fit_1D)), 0.19323578)
})

test_that("TPR works with .sim function", {
  set.seed(1)
  p <- c(10, 10, 10)
  u <- c(1, 1, 1)
  r <- 5
  n <- 200
  dat <- TPRsim(p = p, r = r, u = u, n = n)
  x <- dat$x
  y <- dat$y
  B <- dat$coefficients
  fit_std <- TPR.fit(x, y, method="standard")
  fit_pls <- TPR.fit(x, y, u, method="PLS")
  expect_equal(fit_std$fitted.values[3, 51:55], c(2.35594144, -7.30311921,  0.89509184,  4.56270439,  5.52483876))
  expect_equal(fit_pls$fitted.values[3, 51:55], c(0.32107521, -4.36888451,  3.42342640, -2.17142435,  3.97609705))
  expect_equal(rTensor::fnorm(B-stats::coef(fit_std)), 2639.8270557)
  expect_equal(rTensor::fnorm(B-stats::coef(fit_pls)), 8.5626061395)
})

test_that("TRR and TPR works with bat and square datasets", {
  ## bat
  set.seed(1)
  data("bat")
  x <- bat$x
  y <- bat$y
  fit_std <- TRR.fit(x, y, method="standard")
  expect_equal(fit_std$fitted.values@data[1:5,3,2], c(0.0059197489885, -0.0048500220339, -0.0011737549541, -0.0022189939226, -0.0038772714662))

  ## square
  data("square")
  x <- square$x
  y <- square$y
  fit_std <- TPR.fit(x, y, method="standard")
  expect_equal(fit_std$fitted.values[1, 51:55], c(15.090983362073, -0.046981801747, 12.441415239454, -51.758700615955, -25.743953633943))
})
