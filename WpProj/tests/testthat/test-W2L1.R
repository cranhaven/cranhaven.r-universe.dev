test_that("W2L1 function for selection variable, exact", {
  set.seed(111)
  
  ##### Testing R Functions ####
  n <- 32
  p <- 10
  s <- 21
  
  x <- matrix( stats::rnorm( p * n ), nrow = n, ncol = p )
  x_ <- t(x)
  beta <- (1:10)/10
  y <- x %*% beta + stats::rnorm(n)
  post_beta <- matrix(beta, nrow=p, ncol=s) + stats::rnorm(p*s, 0, 0.1)
  post_mu <- x %*% post_beta
  
  xtx <- crossprod(x)/n #* wt + diag(1,p,p) * (1 - wt)
  xty <- crossprod(x, post_mu)/n #* wt + post_beta * (1 - wt)
  
  
  #exact
  transp <- "exact"
  otopt <- list(same = TRUE, method = "selection.variable",
                transport.method = transp, epsilon = 0.05, niter = 100)
  suffStat_star <- sufficientStatistics(x, post_mu, t(post_beta), otopt)
  xtx_star <- suffStat_star$XtX #* wt + diag(post_beta_norm) * (1-wt)
  xty_star <- suffStat_star$XtY #* wt + post_beta_norm * (1-wt)
  
  active.idx <- seq(2,10,2)
  
  out <- W2L1(x, post_mu, post_beta,
              penalty = "selection.lasso", method = "selection.variable", nlambda = 5)
  testthat::expect_equal(xtx_star, out$xtx)
  testthat::expect_equal(xty_star, out$xty)
  testthat::expect_equal(out$beta[,6], rep(1,p))
})
testthat::test_that("W2L1 function for selection variable,hilbert", {
  set.seed(111)
  
  ##### Testing R Functions ####
  n <- 32
  p <- 10
  s <- 21
  
  x <- matrix( stats::rnorm( p * n ), nrow = n, ncol = p )
  x_ <- t(x)
  beta <- (1:10)/10
  y <- x %*% beta + stats::rnorm(n)
  post_beta <- matrix(beta, nrow=p, ncol=s) + stats::rnorm(p*s, 0, 0.1)
  post_mu <- x %*% post_beta
  
  xtx <- crossprod(x)/n #* wt + diag(1,p,p) * (1 - wt)
  xty <- crossprod(x, post_mu)/n #* wt + post_beta * (1 - wt)
  
  #hilbert
  transp <- "hilbert"
  otopt <- list(same = TRUE, method = "selection.variable",
                transport.method = transp, epsilon = 0.05, niter = 100)
  suffStat_star <- sufficientStatistics(x, post_mu, t(post_beta), otopt)
  xtx_star <- suffStat_star$XtX #* wt + diag(post_beta_norm) * (1-wt)
  xty_star <- suffStat_star$XtY #* wt + post_beta_norm * (1-wt)
  
  active.idx <- seq(2,10,2)
  
  out <- W2L1(x, post_mu, post_beta,
              penalty = "selection.lasso", method = "selection.variable",
              transport.method = transp, nlambda = 5)
  testthat::expect_equal(xtx_star, out$xtx)
  testthat::expect_equal(xty_star, out$xty)
  testthat::expect_equal(out$beta[,6], rep(1,p))
  
})
testthat::test_that("W2L1 function for selection variable,rank", {
  set.seed(111)
  
  ##### Testing R Functions ####
  n <- 32
  p <- 10
  s <- 21
  
  x <- matrix( stats::rnorm( p * n ), nrow = n, ncol = p )
  x_ <- t(x)
  beta <- (1:10)/10
  y <- x %*% beta + stats::rnorm(n)
  post_beta <- matrix(beta, nrow=p, ncol=s) + stats::rnorm(p*s, 0, 0.1)
  post_mu <- x %*% post_beta
  
  xtx <- crossprod(x)/n #* wt + diag(1,p,p) * (1 - wt)
  xty <- crossprod(x, post_mu)/n #* wt + post_beta * (1 - wt)
  
  # rank
  transp <- "rank"
  otopt <- list(same = TRUE, method = "selection.variable",
                transport.method = transp, epsilon = 0.05, niter = 100)
  suffStat_star <- sufficientStatistics(x, post_mu, t(post_beta), otopt)
  xtx_star <- suffStat_star$XtX #* wt + diag(post_beta_norm) * (1-wt)
  xty_star <- suffStat_star$XtY #* wt + post_beta_norm * (1-wt)
  
  active.idx <- seq(2,10,2)
  
  out <- W2L1(x, post_mu, post_beta,
              penalty = "selection.lasso", method = "selection.variable",
              transport.method = transp, infimum.maxit = 1e3, nlambda = 5)
  testthat::expect_equal(xtx_star, out$xtx)
  testthat::expect_equal(xty_star, out$xty)
  testthat::expect_equal(out$beta[,6], rep(1,p))
})
testthat::test_that("W2L1 function for selection variable, univariate.approx.pwr", {
  set.seed(111)
  
  ##### Testing R Functions ####
  n <- 32
  p <- 10
  s <- 21
  
  x <- matrix( stats::rnorm( p * n ), nrow = n, ncol = p )
  x_ <- t(x)
  beta <- (1:10)/10
  y <- x %*% beta + stats::rnorm(n)
  post_beta <- matrix(beta, nrow=p, ncol=s) + stats::rnorm(p*s, 0, 0.1)
  post_mu <- x %*% post_beta
  
  xtx <- crossprod(x)/n #* wt + diag(1,p,p) * (1 - wt)
  xty <- crossprod(x, post_mu)/n #* wt + post_beta * (1 - wt)
  #univariate.approx
  transp <- "univariate.approximation.pwr"
  otopt <- list(same = TRUE, method = "selection.variable",
                transport.method = transp, epsilon = 0.05, niter = 100)
  suffStat_star <- sufficientStatistics(x, post_mu, t(post_beta), otopt)
  xtx_star <- suffStat_star$XtX #* wt + diag(post_beta_norm) * (1-wt)
  xty_star <- suffStat_star$XtY #* wt + post_beta_norm * (1-wt)
  
  active.idx <- seq(2,10,2)
  
  out <- W2L1(x, post_mu, post_beta,
              penalty = "selection.lasso", method = "selection.variable", nlambda = 5,
              transport.method = transp)
  testthat::expect_equal(xtx_star, out$xtx)
  testthat::expect_equal(xty_star, out$xty)
  testthat::expect_equal(out$beta[,6], rep(1,p))
  
  #generate warning if not select selection.lasso
  out2 <- W2L1(x, post_mu, post_beta,
               penalty = "lasso", method = "selection.variable", nlambda = 5,
               transport.method = transp)
  testthat::expect_equal(out$beta, out2$beta)
  
})

testthat::test_that("W2L1 matches oem, univ approx",{
  set.seed(283947)
  
  n <- 32
  p <- 10
  s <- 21
  
  x <- matrix(stats::rnorm(p*n), nrow=n, ncol=p)
  beta <- (1:10)/10
  y <- x %*% beta + stats::rnorm(n)
  
  #posterior
  prec <- crossprod(x) + diag(1,p,p)*1
  mu_post <- solve(prec, crossprod(x,y))
  alpha <- 1 + n/2
  beta <- 1 + 0.5 * (crossprod(y) + t(mu_post) %*% prec %*% mu_post )
  sigma_post <- 1/stats::rgamma(s, alpha, 1/beta)
  theta <- sapply(sigma_post, function(ss) mu_post + t(chol(ss * solve(prec))) %*% matrix(stats::rnorm(p, 0, 1),p,1))
  
  post_mu <- x %*% theta
  post_diff <- matrix(c(y),nrow=n,ncol=s) + matrix(stats::rnorm(s*n,0,0.01),nrow=n,ncol=s)
  post_vdiff <- matrix(stats::rnorm(n*s),nrow=n,ncol=s)
  xtx <- crossprod(x)/n 
  xty <- crossprod(x, post_mu)/n 
  lambda <- 0
  nlambda <- 100
  lambda.min.ratio <- 1e-10
  gamma <- 1
  penalty.factor <- 1/rowMeans(theta^2)
  penalty.factor.null <- rep(1,p)
  transp <- "univariate.approximation.pwr"
  
  otopt <- list(same = TRUE, method = "selection.variable",
                transport.method = transp, epsilon = 0.05, niter = 100)
  otoptdiff <- list(same = FALSE, method = "selection.variable",
                transport.method = transp, epsilon = 0.05, niter = 100)
  suffstat <- sufficientStatistics(x,post_mu, t(theta), otopt)
  suffstatd <- sufficientStatistics(x,post_diff, t(theta), otoptdiff)
  suffstatdd <- sufficientStatistics(x,post_vdiff, t(theta), otoptdiff)
  
  #compare to oem
  check <- oem::oem.xtx(xtx=suffstat$XtX, xty=suffstat$XtY, family="gaussian",penalty="ols", lambda=0,maxit=10000, scale.factor = sqrt(diag(suffstat$XtX)))
  w2 <- W2L1(X=x, Y=NULL, 
             theta=theta, penalty="ols",
             nlambda = 1, lambda.min.ratio = lambda.min.ratio,
             infimum.maxit=1, maxit = 1e3, gamma = gamma, 
             display.progress = FALSE, lambda=0,
             method="scale",transport.method = transp)
  
  testthat::expect_equal(c(check$beta[[1]]), c(w2$beta))
  testthat::expect_equal(c(check$beta[[1]]), c(w2$beta))
  testthat::expect_equal(c(check$d), c(w2$d)) #eigenvals good
  testthat::expect_lte(check$niter[[1]], w2$niter[1,1])
  testthat::expect_equal(c(w2$xtx), c(suffstat$XtX))
  testthat::expect_equal(c(w2$xty), c(suffstat$XtY)) #xty same!
  
  check2 <- oem::oem.xtx(xtx=suffstatd$XtX, xty=suffstatd$XtY, family="gaussian",penalty="ols", lambda=0,maxit=10000, scale.factor = sqrt(diag(suffstatd$XtX)))
  w22 <- W2L1(X=x, Y=post_diff, 
              theta=theta, penalty="ols",
              nlambda = 1, lambda.min.ratio = lambda.min.ratio,
              infimum.maxit=1, maxit = 1e3, gamma = gamma, 
              display.progress = FALSE, lambda=0,
              method="scale",transport.method = transp)
  testthat::expect_equal(c(check2$beta[[1]]), c(w22$beta))
  testthat::expect_equal(c(check2$d), c(w22$d)) #eigenvals good
  testthat::expect_lte(check2$niter[[1]], w22$niter[1,1])
  testthat::expect_equal(c(w22$xtx), c(suffstatd$XtX))
  testthat::expect_equal(c(w22$xty), c(suffstatd$XtY)) #xty same!
  
  check3 <- oem::oem.xtx(xtx=suffstatdd$XtX, xty=suffstatdd$XtY, family="gaussian",penalty="ols", lambda=0,maxit=10000, scale.factor = sqrt(diag(suffstatdd$XtX)))
  w23 <- W2L1(X=x, Y=post_vdiff, 
              theta=theta, penalty="ols",
              nlambda = 1, lambda.min.ratio = lambda.min.ratio,
              infimum.maxit=1, maxit = 1e3, gamma = gamma, 
              display.progress = FALSE, lambda=0,
              method="scale",transport.method = transp)
  testthat::expect_equal(c(check3$beta[[1]]), c(w23$beta)) 
  testthat::expect_equal(c(check3$d), c(w23$d)) #eigenvals good
  testthat::expect_lte(check3$niter[[1]], w23$niter[1,1])
  testthat::expect_equal(c(w23$xtx), c(suffstatdd$XtX))
  testthat::expect_equal(c(w23$xty), c(suffstatdd$XtY)) #xty same!
})
testthat::test_that("W2L1 matches oem, rank",{
  set.seed(283947)
  
  n <- 32
  p <- 10
  s <- 21
  
  x <- matrix(stats::rnorm(p*n), nrow=n, ncol=p)
  beta <- (1:10)/10
  y <- x %*% beta + stats::rnorm(n)
  
  #posterior
  prec <- crossprod(x) + diag(1,p,p)*1
  mu_post <- solve(prec, crossprod(x,y))
  alpha <- 1 + n/2
  beta <- 1 + 0.5 * (crossprod(y) + t(mu_post) %*% prec %*% mu_post )
  sigma_post <- 1/stats::rgamma(s, alpha, 1/beta)
  theta <- sapply(sigma_post, function(ss) mu_post + t(chol(ss * solve(prec))) %*% matrix(stats::rnorm(p, 0, 1),p,1))
  
  post_mu <- x %*% theta
  post_diff <- matrix(c(y),nrow=n,ncol=s) + matrix(stats::rnorm(s*n,0,0.01),nrow=n,ncol=s)
  post_vdiff <- matrix(stats::rnorm(n*s),nrow=n,ncol=s)
  xtx <- crossprod(x)/n 
  xty <- crossprod(x, post_mu)/n 
  lambda <- 0
  nlambda <- 100
  lambda.min.ratio <- 1e-10
  gamma <- 1
  penalty.factor <- 1/rowMeans(theta^2)
  penalty.factor.null <- rep(1,p)
  transp <- "rank"
  
  otopt <- list(same = TRUE, method = "selection.variable",
                transport.method = transp, epsilon = 0.05, niter = 100)
  otoptdiff <- list(same = FALSE, method = "selection.variable",
                    transport.method = transp, epsilon = 0.05, niter = 100)
  suffstat <- sufficientStatistics(x,post_mu, t(theta), otopt)
  suffstatd <- sufficientStatistics(x,post_diff, t(theta), otoptdiff)
  suffstatdd <- sufficientStatistics(x,post_vdiff, t(theta), otoptdiff)
  
  #compare to oem
  check <- oem::oem.xtx(xtx=suffstat$XtX, xty=suffstat$XtY, family="gaussian",penalty="ols", lambda=0,maxit=10000, scale.factor = sqrt(diag(suffstat$XtX)))
  w2 <- W2L1(X=x, Y=NULL, 
             theta=theta, penalty="ols",
             nlambda = 1, lambda.min.ratio = lambda.min.ratio,
             infimum.maxit=1, maxit = 1e3, gamma = gamma, 
             display.progress = FALSE, lambda=0,
             method="scale",
             transport.method = transp)
  
  testthat::expect_equal(c(check$beta[[1]]), c(w2$beta))
  testthat::expect_equal(c(check$beta[[1]]), c(w2$beta))
  testthat::expect_equal(c(check$d), c(w2$d)) #eigenvals good
  testthat::expect_lte(check$niter[[1]], w2$niter[1,1])
  testthat::expect_equal(c(w2$xtx), c(suffstat$XtX))
  testthat::expect_equal(c(w2$xty), c(suffstat$XtY)) #xty same!
  
  check2 <- oem::oem.xtx(xtx=suffstatd$XtX, xty=suffstatd$XtY, family="gaussian",penalty="ols", lambda=0,maxit=10000, scale.factor = sqrt(diag(suffstatd$XtX)))
  w22 <- W2L1(X=x, Y=post_diff, 
              theta=theta, penalty="ols",
              nlambda = 1, lambda.min.ratio = lambda.min.ratio,
              infimum.maxit=1, maxit = 1e3, gamma = gamma, 
              display.progress = FALSE, lambda=0,
              method="scale",transport.method = transp)
  testthat::expect_equal(c(check2$beta[[1]]), c(w22$beta))
  testthat::expect_equal(c(check2$d), c(w22$d)) #eigenvals good
  testthat::expect_lte(check2$niter[[1]], w22$niter[1,1])
  testthat::expect_equal(c(w22$xtx), c(suffstatd$XtX))
  testthat::expect_equal(c(w22$xty), c(suffstatd$XtY)) #xty same!
  
  check3 <- oem::oem.xtx(xtx=suffstatdd$XtX, xty=suffstatdd$XtY, family="gaussian",penalty="ols", lambda=0,maxit=10000, scale.factor = sqrt(diag(suffstatdd$XtX)))
  w23 <- W2L1(X=x, Y=post_vdiff, 
              theta=theta, penalty="ols",
              nlambda = 1, lambda.min.ratio = lambda.min.ratio,
              infimum.maxit=1, maxit = 1e3, gamma = gamma, 
              display.progress = FALSE, lambda=0,
              method="scale",transport.method = transp)
  testthat::expect_equal(c(check3$beta[[1]]), c(w23$beta)) 
  testthat::expect_equal(c(check3$d), c(w23$d)) #eigenvals good
  testthat::expect_lte(check3$niter[[1]], w23$niter[1,1])
  testthat::expect_equal(c(w23$xtx), c(suffstatdd$XtX))
  testthat::expect_equal(c(w23$xty), c(suffstatdd$XtY)) #xty same!
})
testthat::test_that("W2L1 matches oem, hilbert",{
  set.seed(283947)
  
  n <- 32
  p <- 10
  s <- 21
  
  x <- matrix(stats::rnorm(p*n), nrow=n, ncol=p)
  beta <- (1:10)/10
  y <- x %*% beta + stats::rnorm(n)
  
  #posterior
  prec <- crossprod(x) + diag(1,p,p)*1
  mu_post <- solve(prec, crossprod(x,y))
  alpha <- 1 + n/2
  beta <- 1 + 0.5 * (crossprod(y) + t(mu_post) %*% prec %*% mu_post )
  sigma_post <- 1/stats::rgamma(s, alpha, 1/beta)
  theta <- sapply(sigma_post, function(ss) mu_post + t(chol(ss * solve(prec))) %*% matrix(stats::rnorm(p, 0, 1),p,1))
  
  post_mu <- x %*% theta
  post_diff <- matrix(c(y),nrow=n,ncol=s) + matrix(stats::rnorm(s*n,0,0.01),nrow=n,ncol=s)
  post_vdiff <- matrix(stats::rnorm(n*s),nrow=n,ncol=s)
  xtx <- crossprod(x)/n 
  xty <- crossprod(x, post_mu)/n 
  lambda <- 0
  nlambda <- 100
  lambda.min.ratio <- 1e-10
  gamma <- 1
  penalty.factor <- 1/rowMeans(theta^2)
  penalty.factor.null <- rep(1,p)
  transp <- "hilbert"
  
  otopt <- list(same = TRUE, method = "selection.variable",
                transport.method = transp, epsilon = 0.05, niter = 100)
  otoptdiff <- list(same = FALSE, method = "selection.variable",
                    transport.method = transp, epsilon = 0.05, niter = 100)
  suffstat <- sufficientStatistics(x,post_mu, t(theta), otopt)
  suffstatd <- sufficientStatistics(x,post_diff, t(theta), otoptdiff)
  suffstatdd <- sufficientStatistics(x,post_vdiff, t(theta), otoptdiff)
  
  #compare to oem
  check <- oem::oem.xtx(xtx=suffstat$XtX, xty=suffstat$XtY, family="gaussian",penalty="ols", lambda=0,maxit=10000, scale.factor = sqrt(diag(suffstat$XtX)))
  w2 <- W2L1(X=x, Y=NULL, 
             theta=theta, penalty="ols",
             nlambda = 1, lambda.min.ratio = lambda.min.ratio,
             infimum.maxit=1, maxit = 1e3, gamma = gamma, 
             display.progress = FALSE, lambda=0,
             method="scale",transport.method = transp)
  
  testthat::expect_equal(c(check$beta[[1]]), c(w2$beta))
  testthat::expect_equal(c(check$beta[[1]]), c(w2$beta))
  testthat::expect_equal(c(check$d), c(w2$d)) #eigenvals good
  testthat::expect_lte(check$niter[[1]], w2$niter[1,1])
  testthat::expect_equal(c(w2$xtx), c(suffstat$XtX))
  testthat::expect_equal(c(w2$xty), c(suffstat$XtY)) #xty same!
  
  check2 <- oem::oem.xtx(xtx=suffstatd$XtX, xty=suffstatd$XtY, family="gaussian",penalty="ols", lambda=0,maxit=10000, scale.factor = sqrt(diag(suffstatd$XtX)))
  w22 <- W2L1(X=x, Y=post_diff, 
              theta=theta, penalty="ols",
              nlambda = 1, lambda.min.ratio = lambda.min.ratio,
              infimum.maxit=1, maxit = 1e3, gamma = gamma, 
              display.progress = FALSE, lambda=0,
              method="scale",transport.method = transp)
  testthat::expect_equal(c(check2$beta[[1]]), c(w22$beta), tolerance = 1e-3)
  testthat::expect_equal(c(check2$d), c(w22$d)) #eigenvals good
  testthat::expect_lte(check2$niter[[1]], w22$niter[1,1])
  testthat::expect_equal(c(w22$xtx), c(suffstatd$XtX))
  testthat::expect_equal(c(w22$xty), c(suffstatd$XtY)) #xty same!
  
  check3 <- oem::oem.xtx(xtx=suffstatdd$XtX, xty=suffstatdd$XtY, family="gaussian",penalty="ols", lambda=0,maxit=10000, scale.factor = sqrt(diag(suffstatdd$XtX)))
  w23 <- W2L1(X=x, Y=post_vdiff, 
              theta=theta, penalty="ols",
              nlambda = 1, lambda.min.ratio = lambda.min.ratio,
              infimum.maxit=1, maxit = 1e3, gamma = gamma, 
              display.progress = FALSE, lambda=0,
              method="scale",transport.method = transp)
  testthat::expect_equal(c(check3$beta[[1]]), c(w23$beta)) #worse
  testthat::expect_equal(c(check3$d), c(w23$d)) #eigenvals good
  testthat::expect_lte(check3$niter[[1]], w23$niter[1,1])
  testthat::expect_equal(c(w23$xtx), c(suffstatdd$XtX))
  testthat::expect_equal(c(w23$xty), c(suffstatdd$XtY)) #xty same!
})
testthat::test_that("W2L1 matches oem, exact",{
  set.seed(283947)
  
  n <- 32
  p <- 10
  s <- 21
  
  x <- matrix(stats::rnorm(p*n), nrow=n, ncol=p)
  beta <- (1:10)/10
  y <- x %*% beta + stats::rnorm(n)
  
  #posterior
  prec <- crossprod(x) + diag(1,p,p)*1
  mu_post <- solve(prec, crossprod(x,y))
  alpha <- 1 + n/2
  beta <- 1 + 0.5 * (crossprod(y) + t(mu_post) %*% prec %*% mu_post )
  sigma_post <- 1/stats::rgamma(s, alpha, 1/beta)
  theta <- sapply(sigma_post, function(ss) mu_post + t(chol(ss * solve(prec))) %*% matrix(stats::rnorm(p, 0, 1),p,1))
  
  post_mu <- x %*% theta
  post_diff <- matrix(c(y),nrow=n,ncol=s) + matrix(stats::rnorm(s*n,0,0.01),nrow=n,ncol=s)
  post_vdiff <- matrix(stats::rnorm(n*s),nrow=n,ncol=s)
  xtx <- crossprod(x)/n 
  xty <- crossprod(x, post_mu)/n 
  lambda <- 0
  nlambda <- 100
  lambda.min.ratio <- 1e-10
  gamma <- 1
  penalty.factor <- 1/rowMeans(theta^2)
  penalty.factor.null <- rep(1,p)
  transp <- "exact"
  
  otopt <- list(same = TRUE, method = "selection.variable",
                transport.method = transp, epsilon = 0.05, niter = 100)
  otoptdiff <- list(same = FALSE, method = "selection.variable",
                    transport.method = transp, epsilon = 0.05, niter = 100)
  suffstat <- sufficientStatistics(x,post_mu, t(theta), otopt)
  suffstatd <- sufficientStatistics(x,post_diff, t(theta), otoptdiff)
  suffstatdd <- sufficientStatistics(x,post_vdiff, t(theta), otoptdiff)
  
  #compare to oem
  check <- oem::oem.xtx(xtx=suffstat$XtX, xty=suffstat$XtY, family="gaussian",penalty="ols", lambda=0,maxit=10000, scale.factor = sqrt(diag(suffstat$XtX)))
  w2 <- W2L1(X=x, Y=NULL, 
             theta=theta, penalty="ols",
             nlambda = 1, lambda.min.ratio = lambda.min.ratio,
             infimum.maxit=1, maxit = 1e3, gamma = gamma, 
             display.progress = FALSE, lambda=0,
             method="scale")
  
  testthat::expect_equal(c(check$beta[[1]]), c(w2$beta))
  testthat::expect_equal(c(check$beta[[1]]), c(w2$beta))
  testthat::expect_equal(c(check$d), c(w2$d)) #eigenvals good
  testthat::expect_lte(check$niter[[1]], w2$niter[1,1])
  testthat::expect_equal(c(w2$xtx), c(suffstat$XtX))
  testthat::expect_equal(c(w2$xty), c(suffstat$XtY)) #xty same!
  
  check2 <- oem::oem.xtx(xtx=suffstatd$XtX, xty=suffstatd$XtY, family="gaussian",penalty="ols", lambda=0,maxit=10000, scale.factor = sqrt(diag(suffstatd$XtX)))
  w22 <- W2L1(X=x, Y=post_diff, 
              theta=theta, penalty="ols",
              nlambda = 1, lambda.min.ratio = lambda.min.ratio,
              infimum.maxit=1, maxit = 1e3, gamma = gamma, 
              display.progress = FALSE, lambda=0,
              method="scale")
  testthat::expect_equal(c(check2$beta[[1]]), c(w22$beta), tolerance = 1e-3)
  testthat::expect_equal(c(check2$d), c(w22$d)) #eigenvals good
  testthat::expect_lte(check2$niter[[1]], w22$niter[1,1])
  testthat::expect_equal(c(w22$xtx), c(suffstatd$XtX))
  testthat::expect_equal(c(w22$xty), c(suffstatd$XtY)) #xty same!
  
  check3 <- oem::oem.xtx(xtx=suffstatdd$XtX, xty=suffstatdd$XtY, family="gaussian",penalty="ols", lambda=0,maxit=10000, scale.factor = sqrt(diag(suffstatdd$XtX)))
  w23 <- W2L1(X=x, Y=post_vdiff, 
              theta=theta, penalty="ols",
              nlambda = 1, lambda.min.ratio = lambda.min.ratio,
              infimum.maxit=1, maxit = 1e3, gamma = gamma, 
              display.progress = FALSE, lambda=0,
              method="scale")
  testthat::expect_equal(c(check3$beta[[1]]), c(w23$beta)) #worse
  testthat::expect_equal(c(check3$d), c(w23$d)) #eigenvals good
  testthat::expect_lte(check3$niter[[1]], w23$niter[1,1])
  testthat::expect_equal(c(w23$xtx), c(suffstatdd$XtX))
  testthat::expect_equal(c(w23$xty), c(suffstatdd$XtY)) #xty same!
})

testthat::test_that("W2L1 gives right values for selection.lasso",{
  set.seed(283947)
  
  n <- 32
  p <- 10
  s <- 21
  
  x <- matrix(stats::rnorm(p*n), nrow=n, ncol=p)
  beta <- (1:10)/10
  y <- x %*% beta + stats::rnorm(n)
  
  #posterior
  prec <- crossprod(x) + diag(1,p,p)*1
  mu_post <- solve(prec, crossprod(x,y))
  alpha <- 1 + n/2
  beta <- 1 + 0.5 * (crossprod(y) + t(mu_post) %*% prec %*% mu_post )
  sigma_post <- 1/stats::rgamma(s, alpha, 1/beta)
  theta <- sapply(sigma_post, function(ss) mu_post + t(chol(ss * solve(prec))) %*% matrix(stats::rnorm(p, 0, 1),p,1))
  
  post_mu <- x %*% theta
  post_diff <- matrix(c(y),nrow=n,ncol=s) + matrix(stats::rnorm(s*n,0,0.01),nrow=n,ncol=s)
  post_vdiff <- matrix(stats::rnorm(n*s),nrow=n,ncol=s)
  xtx <- crossprod(x)/n 
  xty <- crossprod(x, post_mu)/n 
  lambda <- 0
  nlambda <- 100
  lambda.min.ratio <- 1e-10
  gamma <- 1
  penalty.factor <- 1/rowMeans(theta^2)
  penalty.factor.null <- rep(1,p)
  transp <- "exact"
  
  selection <- W2L1(X=x, Y=NULL, 
                    theta=theta, penalty="selection.lasso",
                    nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                    infimum.maxit=1e4, maxit = 1e3, gamma = gamma,
                    display.progress = FALSE, lambda=lambda,
                    penalty.factor = penalty.factor.null, method="selection.variable",
                    transport.method = transp)
  testthat::expect_equal(all(selection$beta %in% c(0,1)),TRUE)
  selection <- W2L1(X=x, Y=NULL, 
                    theta=theta, penalty="selection.lasso",
                    nlambda = 10, lambda.min.ratio = lambda.min.ratio,
                    infimum.maxit=1e4, maxit = 1e3, gamma = gamma,
                    display.progress = FALSE,
                    penalty.factor = penalty.factor.null, method="selection.variable",
                    transport.method = 'univariate.approximation.pwr')
  testthat::expect_equal(all(selection$beta %in% c(0,1)),TRUE)
  
  selection <- W2L1(X=x, Y=NULL, 
                    theta=theta, penalty="selection.lasso",
                    nlambda = 10, lambda.min.ratio = lambda.min.ratio,
                    infimum.maxit=1e4, maxit = 1e3, gamma = gamma,
                    display.progress = FALSE,
                    penalty.factor = penalty.factor.null, method="selection.variable", model.size = 3,
                    transport.method = transp)
  testthat::expect_equal(all(selection$beta %in% c(0,1)),TRUE)
  selection <- W2L1(X=x, Y=NULL, 
                    theta=theta, penalty="selection.lasso",
                    nlambda = 100, lambda.min.ratio = 0.1,
                    infimum.maxit=1e4, maxit = 1e3, gamma = gamma,
                    display.progress = FALSE,
                    penalty.factor = penalty.factor.null, method="selection.variable", model.size = 3,
                    transport.method = transp)
  testthat::expect_equal(all(selection$beta %in% c(0,1)),TRUE)
  
  selection <- W2L1(X=x, Y=NULL, 
                    theta=theta, penalty="selection.lasso",
                    nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                    infimum.maxit=1e4, maxit = 1e3, gamma = gamma,
                    display.progress = FALSE,
                    penalty.factor = rep(1,p), method="selection.variable",
                    transport.method = transp)
  testthat::expect_equal(all(selection$beta %in% c(0,1)),TRUE)
  testthat::expect_equal(any(selection$beta %in% c(0)),TRUE)
  testthat::expect_equal(any(selection$beta %in% c(1)),TRUE)
  
  scale <- W2L1(X=x, Y=NULL, 
                theta=theta, penalty="mcp",
                nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                infimum.maxit=1e4, maxit = 1e3, gamma = gamma,
                display.progress = FALSE, lambda=lambda,
                penalty.factor = penalty.factor.null, method="scale")
  testthat::expect_equal(c(scale$beta[,1]), rep(1,p), tolerance = 1e-4) #should be pretty close
})

testthat::test_that("W2L1 function for projection", {
  set.seed(283947)
  
  n <- 256
  p <- 10
  s <- 21
  
  x <- matrix(stats::rnorm(p*n), nrow=n, ncol=p)
  beta <- (1:10)/10
  y <- x %*% beta + stats::rnorm(n)
  
  #posterior
  prec <- crossprod(x) + diag(1,p,p)*1
  mu_post <- solve(prec, crossprod(x,y))
  alpha <- 1 + n/2
  beta <- 1 + 0.5 * (crossprod(y) + t(mu_post) %*% prec %*% mu_post )
  sigma_post <- 1/stats::rgamma(s, alpha, 1/beta)
  theta <- sapply(sigma_post, function(ss) mu_post + t(chol(ss * solve(prec))) %*% matrix(stats::rnorm(p, 0, 1),p,1))
  
  post_mu <- x %*% theta
  post_diff <- matrix(c(y),nrow=n,ncol=s) + matrix(stats::rnorm(s*n,0,0.01),nrow=n,ncol=s)
  post_vdiff <- matrix(stats::rnorm(n*s),nrow=n,ncol=s)
  xtx <- crossprod(x)/n 
  xty <- crossprod(x, post_mu)/n 
  lambda <- 0
  nlambda <- 100
  lambda.min.ratio <- 1e-10
  gamma <- 1
  penalty.factor <- 1/rowMeans(theta^2)
  penalty.factor.null <- rep(1,p)
  transp <- "hilbert"
  projectionols <- W2L1(X=x, Y=NULL, 
                     theta=theta, penalty="ols",
                     nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                     infimum.maxit=1, maxit = 1e3, gamma = gamma,
                     display.progress = FALSE, lambda=lambda,
                     penalty.factor = penalty.factor.null, method="projection",
                     tol = 0)
  testthat::expect_equal(c(projectionols$beta), c(theta)) #should be pretty close
  testthat::expect_equal(c(projectionols$beta), c(coef(lm(post_mu ~ x + 0))))#should be pretty close
  testthat::expect_equal(c(theta), c(coef(lm(post_mu ~ x + 0))))#should be pretty close
  
    
  projectionmcp <- W2L1(X=x, Y=NULL, 
                     theta=theta, penalty="mcp",
                     nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                     infimum.maxit=1, maxit = 1e3, gamma = gamma,
                     display.progress = FALSE, lambda=lambda,
                     penalty.factor = penalty.factor.null, method="projection",
                     tol = 0)
  testthat::expect_equal(c(projectionmcp$beta[,2]), c(theta)) #should be pretty close
  testthat::expect_equal(c(projectionmcp$beta[,1]), c(theta)) #should be pretty close
  testthat::expect_equal(c(projectionmcp$beta[,1]), c(projectionols$beta)) #should be pretty close
  
  
  projectionscad <- W2L1(X=x, Y=NULL, 
                     theta=theta, penalty="scad",
                     nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                     infimum.maxit=1, maxit = 1e3, gamma = gamma,
                     display.progress = FALSE, lambda=lambda,
                     penalty.factor = penalty.factor, method="projection",
                     tol = 0)
  testthat::expect_equal(c(projectionscad$beta[,2]), c(theta)) #should be pretty close
  testthat::expect_equal(c(projectionscad$beta[,1]), c(theta)) #should be pretty close
  
  
  projectionlasso <- W2L1(X=x, Y=NULL, 
                     theta=theta, penalty="lasso",
                     nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                     infimum.maxit=1, maxit = 1e3, gamma = gamma,
                     display.progress = FALSE, lambda = lambda,
                     penalty.factor = penalty.factor, method="projection",
                     tol= 0)
  testthat::expect_equal(c(projectionlasso$beta[,2]), c(theta)) #should be pretty close
  testthat::expect_equal(c(projectionlasso$beta[,1]), c(theta)) #should be pretty close  
  
  projectionlasso <- W2L1(X=x, Y=NULL, 
                          theta=theta, penalty="lasso",
                          nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                          infimum.maxit=1, maxit = 1e3, gamma = gamma,
                          display.progress = FALSE, 
                          penalty.factor = penalty.factor, method="projection",
                          tol = 0)
  testthat::expect_equal(c(projectionlasso$beta[,101]), c(theta)) #should be pretty close 
  
  #should warn about infimum
  testthat::expect_warning(W2L1(X=x, Y=NULL, 
                                theta=theta, penalty="lasso",
                                nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                                infimum.maxit=1, maxit = 1, gamma = gamma,
                                display.progress = FALSE, 
                                penalty.factor = penalty.factor, method="projection"))
})

testthat::test_that("W2L1 function for grouped projection", {
  set.seed(283947)
  
  n <- 32
  p <- 20
  g <- 10
  s <- 21
  
  x <- matrix(stats::rnorm(p*n), nrow=n, ncol=p)
  beta <- rep((1:g)/g, p/g)
  groups <- rep(1:g, p/g)
  y <- x %*% beta + stats::rnorm(n)
  
  #posterior
  prec <- crossprod(x) + diag(1,p,p)*1
  mu_post <- solve(prec, crossprod(x,y))
  alpha <- 1 + n/2
  beta <- 1 + 0.5 * (crossprod(y) + t(mu_post) %*% prec %*% mu_post )
  sigma_post <- 1/stats::rgamma(s, alpha, 1/beta)
  theta <- sapply(sigma_post, function(ss) mu_post + t(chol(ss * solve(prec))) %*% matrix(stats::rnorm(p, 0, 1),p,1))
  
  post_mu <- x %*% theta
  post_diff <- matrix(c(y),nrow=n,ncol=s) + matrix(stats::rnorm(s*n,0,0.01),nrow=n,ncol=s)
  post_vdiff <- matrix(stats::rnorm(n*s),nrow=n,ncol=s)
  xtx <- crossprod(x)/n 
  xty <- crossprod(x, post_mu)/n 
  lambda <- 0
  nlambda <- 10
  lambda.min.ratio <- 1e-10
  gamma <- 1
  penalty.factor <- 1/rowMeans(theta^2)
  penalty.factor.null <- rep(1,p)
  transp <- "hilbert"
  projectionols_nogroup <- WpProj:::W2L1(X=x, Y=NULL, 
                               theta=theta, penalty="ols",
                               nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                               infimum.maxit=1, maxit = 1e3, gamma = gamma,
                               display.progress = FALSE, lambda=lambda,
                               penalty.factor = penalty.factor.null, method="projection",
                               tol = 0)
  projectionols <- WpProj:::W2L1(X=x, Y=NULL, 
                        theta=theta, penalty="ols", groups = groups,
                        nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                        infimum.maxit=1, maxit = 1e3, gamma = gamma,
                        display.progress = FALSE, lambda=lambda,
                        penalty.factor = penalty.factor.null, method="projection",
                        tol = 0)
  testthat::expect_equal(c(projectionols$beta), c(theta)) #should be pretty close
  testthat::expect_equal(c(projectionols_nogroup$beta), c(theta)) #should be pretty close
  testthat::expect_equal(c(projectionols$beta), c(coef(lm(post_mu ~ x + 0))))#should be pretty close
  testthat::expect_equal(c(projectionols_nogroup$beta), c(coef(lm(post_mu ~ x + 0))))#should be pretty close
  testthat::expect_equal(c(theta), c(coef(lm(post_mu ~ x + 0))))#should be pretty close
  
  
  projectionmcp_nogroup <-WpProj::: W2L1(X=x, Y=NULL, 
                        theta=theta, penalty="mcp",
                        nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                        infimum.maxit=1, maxit = 1e3, gamma = gamma,
                        display.progress = FALSE,
                        penalty.factor = penalty.factor.null, method="projection",
                        tol = 0)
  projectionmcp <- WpProj:::W2L1(X=x, Y=NULL, 
                        theta=theta, penalty="grp.mcp", groups = groups,
                        nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                        infimum.maxit=1, maxit = 1e3, gamma = gamma,
                        display.progress = FALSE,
                        penalty.factor = penalty.factor.null, method="projection",
                        tol = 0)
  testthat::expect_equal(c(projectionmcp$beta[,11]), c(projectionmcp_nogroup$beta[,11])) #should be pretty close
  testthat::expect_equal(c(projectionmcp$beta[,11]), c(theta)) #should be pretty close
  testthat::expect_equal(c(projectionmcp_nogroup$beta[,11]), c(theta)) #should be pretty close
  testthat::expect_equal(c(projectionmcp$beta[,11]), c(projectionols$beta)) #should be pretty close
  testthat::expect_equal(c(projectionmcp_nogroup$beta[,11]), c(projectionols$beta)) #should be pretty close
  for(i in 1:10) testthat::expect_true(length(unique(abs(projectionmcp$beta[seq(i,p*s,10),1])<1e-14))==1)
  
  
  projectionscad <- W2L1(X=x, Y=NULL, 
                         theta=theta, penalty="grp.scad", groups = groups,
                         nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                         infimum.maxit=1, maxit = 1e3, gamma = gamma,
                         display.progress = FALSE,
                         penalty.factor = penalty.factor.null, method="projection",
                         tol = 0)
  testthat::expect_equal(c(projectionscad$beta[,11]), c(projectionmcp_nogroup$beta[,11])) #should be pretty close
  testthat::expect_equal(c(projectionscad$beta[,11]), c(theta)) #should be pretty close
  testthat::expect_equal(c(projectionscad$beta[,11]), c(projectionols$beta)) #should be pretty close
  for(i in 1:10) testthat::expect_true(length(unique(abs(projectionscad$beta[seq(i,p*s,10),1])==0))==1)
  
  projectionlasso <- W2L1(X=x, Y=NULL, 
                          theta=theta, penalty="grp.lasso",groups = groups,
                          nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                          infimum.maxit=1, maxit = 1e3, gamma = gamma,
                          display.progress = FALSE, 
                          penalty.factor = penalty.factor.null, method="projection",
                          tol= 0)
  testthat::expect_equal(c(projectionlasso$beta[,11]), c(theta)) #should be pretty close
  testthat::expect_equal(c(projectionlasso$beta[,11]), c(projectionols$beta)) #should be pretty close
  for(i in 1:10) testthat::expect_true(length(unique(abs(projectionlasso$beta[seq(i,p*s,10),1])<1e-14))==1)
  
  projectionmcp.net <- W2L1(X=x, Y=NULL, 
                       theta=theta, penalty="grp.mcp.net", groups = groups,
                       nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                       infimum.maxit=1, maxit = 1e3, gamma = gamma,
                       display.progress = FALSE, 
                       penalty.factor = penalty.factor.null, method="projection",
                       tol = 0)
  testthat::expect_equal(c(projectionmcp.net$beta[,11]), c(theta)) #should be pretty close
  testthat::expect_equal(c(projectionmcp.net$beta[,11]), c(projectionols$beta)) #should be pretty close
  for(i in 1:10) testthat::expect_true(length(unique(abs(projectionmcp.net$beta[seq(i,p*s,10),1])<1e-14))==1)  
  
  projectionscad.net <- W2L1(X=x, Y=NULL, 
                            theta=theta, penalty="grp.scad.net", groups = groups,
                            nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                            infimum.maxit=1, maxit = 1e3, gamma = gamma,
                            display.progress = FALSE, 
                            penalty.factor = penalty.factor.null, method="projection",
                            tol = 0)
  testthat::expect_equal(c(projectionscad.net$beta[,11]), c(theta)) #should be pretty close
  testthat::expect_equal(c(projectionscad.net$beta[,11]), c(projectionols$beta)) #should be pretty close
  for(i in 1:10) testthat::expect_true(length(unique(abs(projectionscad.net$beta[seq(i,p*s,10),1])<1e-14))==1)  
  
})
