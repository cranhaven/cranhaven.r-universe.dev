test_that("make sure calc.beta throws error when active idx is ", {
  set.seed(239874)
  
  ##### Testing R Functions ####
  n <- 128
  p <- 10
  s <- 100
  
  x <- matrix( rnorm( p * n ), nrow = n, ncol = p )
  x_ <- t(x)
  beta <- (1:10)/10
  y <- x %*% beta + rnorm(n)
  post_beta <- matrix(beta, nrow=p, ncol=s) + rnorm(p*s, 0, 0.1)
  post_mu <- x %*% post_beta
  
  xtx <- crossprod(x)/n #* wt + diag(1,p,p) * (1 - wt)
  xty <- crossprod(x, post_mu)/n #* wt + post_beta * (1 - wt)
  
  OTopts <- list(same = TRUE,
                 method = "projection",
                 transport.method = "exact",
                 epsilon = 0.05,
                 niter = 100)
  
  testthat::expect_error(calc.beta(xtx, xty, integer(0), OTopts$method, OToptions = OToptions, x, post_beta))
})
