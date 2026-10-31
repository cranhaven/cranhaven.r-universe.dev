context("Test auxiliary functions")

testthat::skip('skip')
RNGkind("L'Ecuyer-CMRG")

test_that("Tenv_Pval function works", {
  set.seed(1)
  data("bat")
  x <- bat$x
  y <- bat$y
  fit_std <- TRR.fit(x, y, method="standard")
  fit <- Tenv_Pval(x, y, fit_std$coefficients)
  expect_equal(fit$se@data[51:55,3,1], c(0.059671626039, 0.067443096107, 0.063656446666, 0.068024423057, 0.057238323458))
  expect_equal(fit$p_val@data[6:10,10,1], c(0.56343181958, 0.51710260368, 0.30713200748, 0.66371425932 , 0.99487291620))
})

test_that("TRRdim works", {
  set.seed(1)
  r <- c(10, 10, 10)
  u <- c(2, 2, 2)
  p <- 5
  n <- 100
  dat <- TRRsim(r = r, p = p, u = u, n = n)
  x <- dat$x
  y <- dat$y
  result <- TRRdim(x, y)
  expect_equal(result$mse, 2255.3026655)
  expect_equal(result$bicval, c(-902.36909312, -742.50254547, -772.98204908), tolerance = 1e-6)
  expect_equal(result$u, c(2,2,2))
})

test_that("PMSE works", {
  set.seed(1)
  p <- c(10, 10, 10)
  u <- c(1, 1, 1)
  r <- 5
  n <- 200
  dat <- TPRsim(p = p, r = r, u = u, n = n)
  x <- dat$x
  y <- dat$y
  fit_std <- TPR.fit(x, y, u, method="standard")
  result <- PMSE(x, y, fit_std$coefficients)
  expect_equal(result$mse, 426.62145357)
  expect_equal(result$pred[3, 51:55], c(2.35594143958, -7.30311921112,  0.89509183803,  4.56270438511,  5.52483875989))
})
