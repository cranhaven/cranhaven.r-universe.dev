test_that("combine.distcompare works", {
  n <- 32
  p <- 10
  s <- 21
  # covariates and coefficients
  x <- matrix( stats::rnorm( p * n ), nrow = n, ncol = p )
  beta <- (1:10)/10
  #outcome
  y <- x %*% beta + stats::rnorm(n)
  # fake posterior
  post_beta <- matrix(beta, nrow=p, ncol=s) + stats::rnorm(p*s, 0, 0.1)
  post_mu <- x %*% post_beta #posterior predictive distributions
  # fit models
  ## L1 model
  fit.p2     <-  WpProj(X=x, eta=post_mu, power = 2.0,
                        method = "L1", #default
                        solver = "lasso" #default
  )
  ## approximate binary program
  fit.p2.bp <-  WpProj(X=x, eta=post_mu, theta = post_beta, power = 2.0,
                       method = "binary program",
                       solver = "lasso" #default because approximate algorithm is faster
  )
  ## compare performance by measuring distance from full model
  dc <- distCompare(models = list("L1" = fit.p2, "BP" = fit.p2.bp))
  
  testthat::expect_silent(cc1 <- combine.distcompare(dc,dc))
  testthat::expect_silent(cc2 <- combine.distcompare(list(dc,dc)))
  testthat::expect_equal(cc1, cc2)
  testthat::expect_true(inherits(cc1, "distcompare"))
  testthat::expect_message(cc <- combine.distcompare(dc))
  testthat::expect_equal(cc, dc)
})
