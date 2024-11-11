test_that("model sizes work for W2IP", {
  set.seed(84370158)
  
  n <- 100
  p <- 10
  s <- 1000
  
  x <- matrix( rnorm( p * n ), nrow = n, ncol = p )
  x_ <- t(x)
  beta <- (1:p)/p
  y <- x %*% beta + rnorm(n)
  post_beta <- matrix(beta, nrow=p, ncol=s) + rnorm(p*s, 0, 0.1)
  post_mu <- x %*% post_beta
  transp <- "hilbert"
  nvars <- c(2,4,8)
    
  test <- W2IP(X = x, Y = post_mu, theta = post_beta, transport.method = transp, 
               infimum.maxit = 10, 
               tol = 1e-7, solver = "cone",
               display.progress = FALSE,nvars = nvars)
  testthat::expect_equal(test$nzero, nvars)
  
})


test_that("model sizes work for gurobi", {
  check_gurobi()
  
  set.seed(84370158)
  
  n <- 100
  p <- 10
  s <- 1000
  
  x <- matrix( rnorm( p * n ), nrow = n, ncol = p )
  x_ <- t(x)
  beta <- (1:p)/p
  y <- x %*% beta + rnorm(n)
  post_beta <- matrix(beta, nrow=p, ncol=s) + rnorm(p*s, 0, 0.1)
  post_mu <- x %*% post_beta
  transp <- "hilbert"
  nvars <- c(2,4,8)
  
  # debugonce(W2IP)
  WpProj:::check_gurobi()
  test <- W2IP(X = x, Y = post_mu, theta = post_beta, transport.method = transp, 
               infimum.maxit = 10, 
               tol = 1e-7, solver = "gurobi",
               display.progress = FALSE,nvars = nvars)
  testthat::expect_equal(test$nzero, nvars)
  
})

test_that("model sizes work for mosek", {
  check_mosek()
  set.seed(84370158)
  
  n <- 100
  p <- 10
  s <- 1000
  
  x <- matrix( stats::rnorm( p * n ), nrow = n, ncol = p )
  x_ <- t(x)
  beta <- (1:p)/p
  y <- x %*% beta + stats::rnorm(n)
  post_beta <- matrix(beta, nrow=p, ncol=s) + stats::rnorm(p*s, 0, 0.1)
  post_mu <- x %*% post_beta
  transp <- "hilbert"
  nvars <- c(2,4,8)
  
  WpProj:::check_mosek()
  test <- W2IP(X = x, Y = post_mu, theta = post_beta, transport.method = transp, 
               infimum.maxit = 10, 
               tol = 1e-7, solver = "mosek",
               display.progress = FALSE,nvars = nvars)
  testthat::expect_equal(test$nzero, nvars)
  
})


# test_that("times work for W2IP LP", { # not work for lpsolve
#   set.seed(84370158)
#   
#   n <- 1000
#   p <- 500
#   s <- 1000
#   
#   x <- matrix( rnorm( p * n ), nrow = n, ncol = p )
#   x_ <- t(x)
#   beta <- (1:p)/p
#   y <- x %*% beta + rnorm(n)
#   post_beta <- matrix(beta, nrow=p, ncol=s) + rnorm(p*s, 0, 0.1)
#   post_mu <- x %*% post_beta
#   transp <- "exact"
#   
#   time.start <- proc.time()
#   testthat::expect_warning(W2IP(X = x, Y = post_mu, theta = post_beta, transport.method = transp, 
#                infimum.maxit = 10, 
#                tol = 1e-7,nvars = 1, solver = "lp",
#                display.progress = FALSE, control = list(tm_limit = 1)))
#   time.end <- proc.time()
#   testthat::expect_lt((time.end - time.start)[3], 100)
#   
# })
