test_that("test not.converged gives correct results", {
  set.seed(111)
  
  ##### Testing R Functions ####
  n <- 100
  p <- 10
  s <- 1000
  
  x <- matrix( rnorm( p * n ), nrow = n, ncol = p )
  x_ <- t(x)
  beta <- (1:10)/10
  y <- x %*% beta + rnorm(n)
  post_beta <- matrix(beta, nrow=p, ncol=s) + rnorm(p*s, 0, 0.1)
  post_mu <- x %*% post_beta
  post_beta_norm <- rowMeans(post_beta^2)
  pseudo.obs <- 1
  wt <- n/(pseudo.obs + n)
  
  xtx <- crossprod(x)/n #* wt + diag(1,p,p) * (1 - wt)
  xty <- crossprod(x, post_mu)/n #* wt + post_beta * (1 - wt)
  otopt <- list(same = TRUE,
                method = "selection.variable",
                transport.method = "exact",
                epsilon = 0.05,
                niter = 100)
  
  suffStat_star <- sufficientStatistics(x, post_mu, t(post_beta), otopt)
  xtx_star <- suffStat_star$XtX #* wt + diag(post_beta_norm) * (1-wt)
  xty_star <- suffStat_star$XtY #* wt + post_beta_norm * (1-wt)
  
  active.idx <- seq(2,10,2)
  fake_coefs <- matrix(1:100, ncol=10, nrow=10)
  fake_coefs2 <- matrix(100:1, ncol=10, nrow=10)
  
  # make sure gives correct results
  testthat::expect_true(not.converged(fake_coefs, fake_coefs2, 1e-10)) # TRUE
  testthat::expect_false(not.converged(fake_coefs, fake_coefs2, 100)) # FALSE
  testthat::expect_false(not.converged(fake_coefs, fake_coefs, 1e-10)) # FALSE
})
