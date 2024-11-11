testthat::test_that("test univariate approximation suff stat", {
  set.seed(222)
  
  #### Setup Data ####
  n <- 128
  p <- 10
  s <- 100
  
  x <- matrix(rnorm(p*n), nrow=n, ncol=p)
  beta <- (1:10)/10
  y <- x %*% beta + rnorm(n)
  
  #posterior
  prec <- crossprod(x) + diag(1,p,p)*1
  mu_post <- solve(prec, crossprod(x,y))
  alpha <- 1 + n/2
  beta <- 1 + 0.5 * (crossprod(y) + t(mu_post) %*% prec %*% mu_post )
  sigma_post <- 1/rgamma(s, alpha, 1/beta)
  theta <- sapply(sigma_post, function(ss) mu_post + t(chol(ss * solve(prec))) %*% matrix(rnorm(p, 0, 1),p,1))
  
  post_mu <- x %*% theta
  post_diff <- matrix(c(y),nrow=n,ncol=s) + matrix(rnorm(s*n,0,0.01),nrow=n,ncol=s)
  post_vdiff <- matrix(rnorm(n*s),nrow=n,ncol=s)
  
  xtx <- crossprod(x)/n 
  xty <- crossprod(x, post_mu)/n 
  
  active.idx <- seq(2,10,2)
  
  transport_method <- "univariate.approximation.pwr"
  OTopt <- list(same = TRUE,
                method = "selection.variable",
                transport.method = transport_method,
                epsilon = 0.05,
                niter = 100)
  OToptproj <- OTopt
  OToptproj$method <- "projection"
  OToptproj$same <- FALSE
  # check univariate.approximation
  
  dat <- list(temp=matrix(0, n, p), xtx = matrix(0,p,p), xty = rep_len(0, p),
              mu = rep(0, n), idx_mu = rep(0, n),
              sort_y = rep(0, n))
  
  for(i in 1:n) {
    dat$temp <- t(theta) * matrix(x[i,,drop=FALSE], s,p, byrow = TRUE)
    dat$mu <- rowSums(dat$temp)
    dat$idx_mu <- order(dat$mu)
    dat$sort_y <- sort(post_mu[i,])
    dat$xtx = dat$xtx + crossprod(dat$temp)
    dat$xty = dat$xty + crossprod(dat$temp[dat$idx_mu,,drop=FALSE], dat$sort_y)
  }
  dat$xtx = dat$xtx/(n*s)
  dat$xty = dat$xty/(n*s)
  
  out <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta), 
                              OTopt)
  
  out_no_trans <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = theta, 
                                       OTopt)
  testthat::expect_equal(out$XtX, dat$xtx)
  testthat::expect_equal(out$XtY, dat$xty)
  testthat::expect_equal(out_no_trans$XtX, dat$xtx)
  testthat::expect_equal(out_no_trans$XtY, dat$xty)
  testthat::expect_equal(out_no_trans$XtY, out$XtY)
  
  #check same flag
  
  out.same <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta), 
                                   OTopt)
  
  testthat::expect_equal(out.same$XtX, dat$xtx)
  testthat::expect_equal(out.same$XtY, dat$xty)
  
  # check subset of data
  p_act <- length(active.idx)
  dat.subset <- list(temp=matrix(0, n, p_act), xtx = matrix(0,p_act,p_act), xty = rep_len(0, p_act),
              mu = rep(0, n), idx_mu = rep(0, n),
              sort_y = rep(0, n))
  
  for(i in 1:n) {
    dat.subset$temp <- t(theta[active.idx,,drop=FALSE]) * matrix(x[i,active.idx,drop=FALSE], s,p_act, byrow = TRUE)
    dat.subset$mu <- rowSums(dat.subset$temp)
    dat.subset$idx_mu <- order(dat.subset$mu)
    dat.subset$sort_y <- sort(post_mu[i,])
    dat.subset$xtx = dat.subset$xtx + crossprod(dat.subset$temp)
    dat.subset$xty = dat.subset$xty + crossprod(dat.subset$temp[dat.subset$idx_mu,,drop=FALSE], dat.subset$sort_y)
  }
  dat.subset$xtx = dat.subset$xtx/(n*s)
  dat.subset$xty = dat.subset$xty/(n*s)
  
  otoptfalse <- OTopt
  otoptfalse$same <- FALSE
  
  out.subset <- sufficientStatistics(X_ = x[,active.idx], Y_ = post_mu, theta_ = t(theta[active.idx,]), 
                                     otoptfalse)
  
  testthat::expect_equal(out.subset$XtX, dat.subset$xtx)
  testthat::expect_equal(out.subset$XtY, dat.subset$xty, tolerance = 1e-2)
  
  # check projection is just normal crossprods
  
  proj <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta), 
                               OToptproj)
  OToptproj$same <- TRUE
  proj.same <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta), 
                                    OToptproj)
  testthat::expect_equal(proj$XtX, xtx)
  testthat::expect_equal(proj$XtY, xty)
  testthat::expect_equal(proj.same$XtX, xtx)
  testthat::expect_equal(proj.same$XtY, xty)
  
})

testthat::test_that("test hilbert suff stat", {
  #check hilbert (assumes transport_plan is correct!!!!)
  set.seed(222)
  
  #### Setup Data ####
  n <- 128
  p <- 10
  s <- 100
  
  x <- matrix(rnorm(p*n), nrow=n, ncol=p)
  beta <- (1:10)/10
  y <- x %*% beta + rnorm(n)
  
  #posterior
  prec <- crossprod(x) + diag(1,p,p)*1
  mu_post <- solve(prec, crossprod(x,y))
  alpha <- 1 + n/2
  beta <- 1 + 0.5 * (crossprod(y) + t(mu_post) %*% prec %*% mu_post )
  sigma_post <- 1/rgamma(s, alpha, 1/beta)
  theta <- sapply(sigma_post, function(ss) mu_post + t(chol(ss * solve(prec))) %*% matrix(rnorm(p, 0, 1),p,1))
  
  post_mu <- x %*% theta
  # post_diff <- matrix(c(y),nrow=n,ncol=s) + matrix(rnorm(s*n,0,0.01),nrow=n,ncol=s)
  # post_vdiff <- matrix(rnorm(n*s),nrow=n,ncol=s)
  
  xtx <- crossprod(x)/n
  xty <- crossprod(x, post_mu)/n 
  
  active.idx <- seq(2,10,2)
  
  transport_method <- "hilbert"
  
  OTopt <- list(same = FALSE,
                method = "selection.variable",
                transport.method = transport_method,
                epsilon = 0.05,
                niter = 100)
  OToptproj <- OTopt
  OToptproj$method <- "projection"
  OToptproj$same <- FALSE
  
  # same mu's
  
  tplan <- transport_plan(X = post_mu, Y = post_mu, ground_p = 2, p = 2, 
                          observation.orientation = "colwise", 
                          method = transport_method, is.X.sorted = TRUE)
  #cost out of sorts because I'm lying to the program about x being sorted
  # hilbert_idx <- tplan$tplan$from
  
  dat <- list(temp=matrix(0, n, p), xtx = matrix(0,p,p), xty = rep_len(0, p),
              mu = rep(0, n), idx_mu = rep(0, n),
              sort_y = rep(0, n))
  
  for(i in 1:n) {
    dat$temp <- t(theta) * matrix(x[i,,drop=FALSE], s,p, byrow = TRUE)
    dat$mu <- rowSums(dat$temp)
    dat$idx_mu <- tplan$tplan$from
    dat$sort_y <- post_mu[i,tplan$tplan$from]
    dat$xtx = dat$xtx + crossprod(dat$temp)
    dat$xty = dat$xty + crossprod(dat$temp[dat$idx_mu,,drop=FALSE], dat$sort_y)
  }
  dat$xtx = dat$xtx/(n*s)
  dat$xty = dat$xty/(n*s)
  out <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta), 
                              OTopt)
  out.trans <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta), 
                                    OTopt)
  
  testthat::expect_equal(out$XtX, dat$xtx)
  testthat::expect_equal(out$XtY, dat$xty)
  testthat::expect_equal(out.trans$XtX, dat$xtx)
  testthat::expect_equal(out.trans$XtY, dat$xty)
  
  #check same flag
  OTopt$same <- TRUE
  out.same <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta), 
                                   OTopt)
  OTopt$same <- FALSE
  
  testthat::expect_equal(out.same$XtX, dat$xtx)
  testthat::expect_equal(out.same$XtY, dat$xty)
  
  # check subset of data
  p_act <- length(active.idx)
  dat.subset <- list(temp=matrix(0, n, p_act), xtx = matrix(0,p_act,p_act), xty = rep_len(0, p_act),
                     mu = rep(0, n), idx_mu = rep(0, n),
                     sort_y = rep(0, n))
  
  tplan.sub <- transport_plan(X = x[,active.idx,drop=FALSE] %*% theta[active.idx,,drop=FALSE], 
                              Y = post_mu, ground_p = 2, p = 2, 
                              observation.orientation = "colwise", 
                              method = transport_method, is.X.sorted = FALSE)
  #cost out of sorts because I'm lying to the program about x being sorted
  # hilbert_idx.sub <- tplan.sub$tplan$to
  
  for(i in 1:n) {
    dat.subset$temp <- t(theta[active.idx,,drop=FALSE]) * matrix(x[i,active.idx,drop=FALSE], s, p_act, byrow = TRUE)
    # dat.subset$mu <- rowSums(dat.subset$temp)
    dat.subset$sort_y <- post_mu[i,tplan.sub$tplan$to] # no sorting needed here
    dat.subset$xtx = dat.subset$xtx + crossprod(dat.subset$temp)/(n*s)
    dat.subset$xty = dat.subset$xty + crossprod(dat.subset$temp[tplan.sub$tplan$from,], dat.subset$sort_y)/(n*s)
  }
  # dat.subset$xtx = dat.subset$xtx
  # dat.subset$xty = dat.subset$xty/(n*s)
  
  out.subset <- sufficientStatistics(X_ = x[,active.idx], Y_ = post_mu, theta_ = t(theta[active.idx,]), 
                                     OTopt)
  
  testthat::expect_equal(out.subset$XtX, dat.subset$xtx)
  testthat::expect_equal(out.subset$XtY, dat.subset$xty)
  
  # check projection is just normal crossprods
  otoptproj <- OTopt
  otoptproj$method <- "projection"
  proj <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta), 
                               otoptproj)
  otoptproj$same <- TRUE
  proj.same <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta), 
                                    otoptproj)
  testthat::expect_equal(proj$XtX, xtx)
  testthat::expect_equal(proj$XtY, xty)
  testthat::expect_equal(proj.same$XtX, xtx)
  testthat::expect_equal(proj.same$XtY, xty)
  
})

testthat::test_that("test rank suff stat", {
  #check rank (assumes transport_plan is correct!!!!)
  
  set.seed(222)
  
  #### Setup Data ####
  n <- 128
  p <- 10
  s <- 100
  
  x <- matrix(rnorm(p*n), nrow=n, ncol=p)
  beta <- (1:10)/10
  y <- x %*% beta + rnorm(n)
  
  #posterior
  prec <- crossprod(x) + diag(1,p,p)*1
  mu_post <- solve(prec, crossprod(x,y))
  alpha <- 1 + n/2
  beta <- 1 + 0.5 * (crossprod(y) + t(mu_post) %*% prec %*% mu_post )
  sigma_post <- 1/rgamma(s, alpha, 1/beta)
  theta <- sapply(sigma_post, function(ss) mu_post + t(chol(ss * solve(prec))) %*% matrix(rnorm(p, 0, 1),p,1))
  
  post_mu <- x %*% theta
  # post_diff <- matrix(c(y),nrow=n,ncol=s) + matrix(rnorm(s*n,0,0.01),nrow=n,ncol=s)
  # post_vdiff <- matrix(rnorm(n*s),nrow=n,ncol=s)
  
  xtx <- crossprod(x)/n
  xty <- crossprod(x, post_mu)/n 
  
  active.idx <- seq(2,10,2)
  
  transport_method <- "rank"
  
  OTopt <- list(same = FALSE,
                method = "selection.variable",
                transport.method = transport_method,
                epsilon = 0.05,
                niter = 100)
  OToptproj <- OTopt
  OToptproj$method <- "projection"
  OToptproj$same <- FALSE
  # same mu's
  
  tplan <- transport_plan(X = post_mu, Y = post_mu, ground_p = 2, p = 2, 
                          observation.orientation = "colwise", 
                          method = transport_method, is.X.sorted = FALSE)
  # rank_idx <- tplan$tplan$to
  
  dat <- list(temp=matrix(0, n, p), xtx = matrix(0,p,p), xty = rep_len(0, p),
              mu = rep(0, n), idx_mu = rep(0, n),
              sort_y = rep(0, n))
  
  for(i in 1:n) {
    dat$temp <- t(theta[,tplan$tplan$to]) * matrix(x[i,,drop=FALSE], s,p, byrow = TRUE)
    dat$mu <- rowSums(dat$temp)
    dat$sort_y <- post_mu[i,]
    dat$xtx = dat$xtx + crossprod(dat$temp)/(n*s)
    dat$xty = dat$xty + crossprod(dat$temp[tplan$tplan$from,], dat$sort_y)/(n*s)
  }
  out <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta), 
                              OTopt)
  out.trans <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta), 
                                    OTopt)
  
  testthat::expect_equal(out$XtX, dat$xtx)
  testthat::expect_equal(out$XtY, dat$xty)
  testthat::expect_equal(out.trans$XtX, dat$xtx)
  testthat::expect_equal(out.trans$XtY, dat$xty)
  
  #check same flag
  OTopt$same <- TRUE
  out.same <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta), 
                                   OTopt)
  OTopt$same <- FALSE
  
  testthat::expect_equal(out.same$XtX, dat$xtx)
  testthat::expect_equal(out.same$XtY, dat$xty)
  
  # check subset of data
  p_act <- length(active.idx)
  dat.subset <- list(temp=matrix(0, n, p_act), xtx = matrix(0,p_act,p_act), xty = rep_len(0, p_act),
                     mu = rep(0, n), idx_mu = rep(0, n),
                     sort_y = rep(0, n))
  
  tplan.sub <- transport_plan(X = post_mu, Y = x[,active.idx,drop=FALSE] %*% theta[active.idx,,drop=FALSE], ground_p = 2, p = 2, 
                              observation.orientation = "colwise", 
                              method = transport_method, is.X.sorted = FALSE)
  # rank_idx.sub <- tplan.sub$tplan$to
  
  for(i in 1:n) {
    dat.subset$temp <- t(theta[active.idx,tplan.sub$tplan$to,drop=FALSE]) * matrix(x[i,active.idx,drop=FALSE], s, p_act, byrow = TRUE)
    # dat.subset$mu <- rowSums(dat.subset$temp)
    dat.subset$sort_y <- post_mu[i,tplan.sub$tplan$from] # no sorting needed here
    dat.subset$xtx = dat.subset$xtx + crossprod(dat.subset$temp)/(n*s)
    dat.subset$xty = dat.subset$xty + crossprod(dat.subset$temp, dat.subset$sort_y)/(n*s)
  }
  # dat.subset$xtx = dat.subset$xtx
  # dat.subset$xty = dat.subset$xty/(n*s)
  
  out.subset <- sufficientStatistics(X_ = x[,active.idx], Y_ = post_mu, theta_ = t(theta[active.idx,]), 
                                     OTopt)
  
  testthat::expect_equal(out.subset$XtX, dat.subset$xtx)
  testthat::expect_equal(out.subset$XtY, dat.subset$xty)
  
  # check projection is just normal crossprods
  
  proj <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta), 
                               OToptproj)
  OToptproj$same <- TRUE
  proj.same <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta), 
                                    OToptproj)
  testthat::expect_equal(proj$XtX, xtx)
  testthat::expect_equal(proj$XtY, xty)
  testthat::expect_equal(proj.same$XtX, xtx)
  testthat::expect_equal(proj.same$XtY, xty)
  
})

testthat::test_that("test shortsimplex suff stat", {
  #check rank (assumes transport_plan is correct!!!!)
  
  set.seed(222)
  
  #### Setup Data ####
  n <- 128
  p <- 10
  s <- 100
  
  x <- matrix(rnorm(p*n), nrow=n, ncol=p)
  beta <- (1:10)/10
  y <- x %*% beta + rnorm(n)
  
  #posterior
  prec <- crossprod(x) + diag(1,p,p)*1
  mu_post <- solve(prec, crossprod(x,y))
  alpha <- 1 + n/2
  beta <- 1 + 0.5 * (crossprod(y) + t(mu_post) %*% prec %*% mu_post )
  sigma_post <- 1/rgamma(s, alpha, 1/beta)
  theta <- sapply(sigma_post, function(ss) mu_post + t(chol(ss * solve(prec))) %*% matrix(rnorm(p, 0, 1),p,1))
  
  post_mu <- x %*% theta
  # post_diff <- matrix(c(y),nrow=n,ncol=s) + matrix(rnorm(s*n,0,0.01),nrow=n,ncol=s)
  # post_vdiff <- matrix(rnorm(n*s),nrow=n,ncol=s)
  
  xtx <- crossprod(x)/n
  xty <- crossprod(x, post_mu)/n 
  
  active.idx <- seq(2,10,2)
  
  transport_method <- "exact"
  
  OTopt <- list(same = FALSE,
                method = "selection.variable",
                transport.method = transport_method,
                epsilon = 0.05,
                niter = 100)
  otoptproj <- OTopt
  otoptproj$method <- "projection"
  otoptproj$same <- FALSE
  
  # same mu's
  
  tplan <- transport_plan(X = post_mu, Y = post_mu, ground_p = 2, p = 2, 
                          observation.orientation = "colwise", 
                          method = transport_method, is.X.sorted = FALSE)
  exact_idx <- tplan$tplan$to
  
  dat <- list(temp=matrix(0, n, p), xtx = matrix(0,p,p), xty = rep_len(0, p),
              mu = rep(0, n), idx_mu = rep(0, n),
              sort_y = rep(0, n))
  
  for(i in 1:n) {
    dat$temp <- t(theta[,exact_idx]) * matrix(x[i,,drop=FALSE], s,p, byrow = TRUE)
    dat$mu <- rowSums(dat$temp)
    dat$sort_y <- post_mu[i,]
    dat$xtx = dat$xtx + crossprod(dat$temp)/(n*s)
    dat$xty = dat$xty + crossprod(dat$temp, dat$sort_y)/(n*s)
  }
  out <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta), 
                              OTopt)
  out.trans <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta), 
                                    OTopt)
  
  testthat::expect_equal(out$XtX, dat$xtx)
  testthat::expect_equal(out$XtY, dat$xty)
  testthat::expect_equal(out.trans$XtX, dat$xtx)
  testthat::expect_equal(out.trans$XtY, dat$xty)
  
  #check same flag
  OTopt$same <- TRUE
  out.same <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta), 
                                   OTopt)
  OTopt$same <- FALSE
  
  testthat::expect_equal(out.same$XtX, dat$xtx)
  testthat::expect_equal(out.same$XtY, dat$xty)
  
  # check subset of data
  p_act <- length(active.idx)
  dat.subset <- list(temp=matrix(0, n, p_act), xtx = matrix(0,p_act,p_act), xty = rep_len(0, p_act),
                     mu = rep(0, n), idx_mu = rep(0, n),
                     sort_y = rep(0, n))
  
  tplan.sub <- transport_plan(X = post_mu, Y = x[,active.idx,drop=FALSE] %*% theta[active.idx,,drop=FALSE], ground_p = 2, p = 2, 
                              observation.orientation = "colwise", 
                              method = transport_method, is.X.sorted = FALSE)
  exact_idx.sub <- tplan.sub$tplan$to
  
  for(i in 1:n) {
    dat.subset$temp <- t(theta[active.idx,exact_idx.sub,drop=FALSE]) * matrix(x[i,active.idx,drop=FALSE], s, p_act, byrow = TRUE)
    # dat.subset$mu <- rowSums(dat.subset$temp)
    dat.subset$sort_y <- post_mu[i,] # no sorting needed here
    dat.subset$xtx = dat.subset$xtx + crossprod(dat.subset$temp)/(n*s)
    dat.subset$xty = dat.subset$xty + crossprod(dat.subset$temp, dat.subset$sort_y)/(n*s)
  }
  # dat.subset$xtx = dat.subset$xtx
  # dat.subset$xty = dat.subset$xty/(n*s)
  
  out.subset <- sufficientStatistics(X_ = x[,active.idx], Y_ = post_mu, theta_ = t(theta[active.idx,]), 
                                     OTopt)
  
  testthat::expect_equal(out.subset$XtX, dat.subset$xtx)
  testthat::expect_equal(out.subset$XtY, dat.subset$xty)
  
  # check projection is just normal crossprods
  proj <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta), 
                               otoptproj)
  otoptproj$same <- TRUE
  proj.same <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta), 
                                    otoptproj)
  testthat::expect_equal(proj$XtX, xtx)
  testthat::expect_equal(proj$XtY, xty)
  testthat::expect_equal(proj.same$XtX, xtx)
  testthat::expect_equal(proj.same$XtY, xty)
  
})

testthat::test_that("test sinkhorn suff stat", {
  #check rank (assumes transport_plan is correct!!!!)
  set.seed(222)

  #### Setup Data ####
  n <- 128
  p <- 10
  s <- 100

  x <- matrix(rnorm(p*n), nrow=n, ncol=p)
  beta <- (1:10)/10
  y <- x %*% beta + rnorm(n)

  #posterior
  prec <- crossprod(x) + diag(1,p,p)*1
  mu_post <- solve(prec, crossprod(x,y))
  alpha <- 1 + n/2
  beta <- 1 + 0.5 * (crossprod(y) + t(mu_post) %*% prec %*% mu_post )
  sigma_post <- 1/rgamma(s, alpha, 1/beta)
  theta <- sapply(sigma_post, function(ss) mu_post + t(chol(ss * solve(prec))) %*% matrix(rnorm(p, 0, 1),p,1))

  post_mu <- x %*% theta
  # post_diff <- matrix(c(y),nrow=n,ncol=s) + matrix(rnorm(s*n,0,0.01),nrow=n,ncol=s)
  # post_vdiff <- matrix(rnorm(n*s),nrow=n,ncol=s)

  xtx <- crossprod(x)/n
  xty <- crossprod(x, post_mu)/n

  active.idx <- seq(2,10,2)

  transport_method <- "sinkhorn"

  OTopt <- list(same = FALSE,
                method = "selection.variable",
                transport.method = transport_method,
                epsilon = 0.05,
                niter = 100)
  OToptproj <- OTopt
  OToptproj$method <- "projection"
  OToptproj$same <- FALSE

  # same mu's

  tplan <- transport_plan(X = post_mu, Y = post_mu, ground_p = 2, p = 2,
                          observation.orientation = "colwise",
                          method = transport_method, is.X.sorted = FALSE)

  dat <- list(temp=matrix(0, n, p), xtx = matrix(0,p,p), xty = rep_len(0, p),
              mu = rep(0, n), idx_mu = rep(0, n),
              sort_y = rep(0, n))

  natoms <- length(tplan$tplan$to)
  theta_sort <- t(theta[,tplan$tplan$to]) * matrix(sqrt(tplan$tplan$mass),nrow=natoms, ncol=p)
  mu_sort <- post_mu[,tplan$tplan$from] * matrix(sqrt(tplan$tplan$mass),nrow=n, ncol=natoms, byrow=TRUE)
  
  for(i in 1:n) {
    dat$temp <- theta_sort * matrix(x[i,,drop=FALSE], nrow=natoms, ncol = p, byrow=TRUE)
    dat$sort_y <- mu_sort[i,]
    dat$xtx = dat$xtx + crossprod(dat$temp)/(n)
    dat$xty = dat$xty + crossprod(dat$temp, dat$sort_y) /(n)
  }
  out <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta),
                              OTopt)
  out.trans <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta),
                                    OTopt)

  testthat::expect_equal(out$XtX, dat$xtx)
  testthat::expect_equal(out$XtY, dat$xty)
  testthat::expect_equal(out.trans$XtX, dat$xtx)
  testthat::expect_equal(out.trans$XtY, dat$xty)

  #check same flag
  OTopt$same <- TRUE
  out.same <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta),
                                   OTopt)
  OTopt$same <- FALSE
  
  dat.same <- list(temp=matrix(0, n, p), xtx = matrix(0,p,p), xty = rep_len(0, p),
             mu = rep(0, n), idx_mu = rep(0, n),
             sort_y = rep(0, n))
  
  for(i in 1:n) {
    dat.same$temp <- t(theta) * matrix(x[i,,drop=FALSE], s,p, byrow = TRUE)
    dat.same$mu <- rowSums(dat$temp)
    dat.same$sort_y <- post_mu[i,]
    dat.same$xtx = dat.same$xtx + crossprod(dat.same$temp)/(n*s)
    dat.same$xty = dat.same$xty + crossprod(dat.same$temp, dat.same$sort_y)/(n*s)
  }

  testthat::expect_equal(out.same$XtX, dat.same$xtx)
  testthat::expect_equal(dat$xtx, dat.same$xtx)
  testthat::expect_equal(dat$xtx, out.same$XtX)
  testthat::expect_equal(out.same$XtY, dat.same$xty)
  testthat::expect_lt(sqrt(sum((out.same$XtY- dat$xty)^2)), 1e-5)
  testthat::expect_lt(sqrt(sum((out.same$XtY - out$XtY)^2)), 1e-5)
  testthat::expect_lt(sqrt(sum((dat$xty - dat.same$xty)^2)), 1e-5)
  testthat::expect_equal(sqrt(sum((dat$xty - dat.same$xty)^2)), sqrt(sum((out.same$XtY - out$XtY)^2)))
  testthat::expect_equal(sqrt(sum((dat$xty - dat.same$xty)^2)), sqrt(sum((out.same$XtY- dat$xty)^2)))

  # check subset of data
  p_act <- length(active.idx)
  dat.subset <- list(temp=matrix(0, s*s, p_act), xtx = matrix(0,p_act,p_act), xty = rep_len(0, p_act),
                     sort_y = rep(0, n))

  tplan.sub <- transport_plan(X = post_mu, Y = x[,active.idx,drop=FALSE] %*% theta[active.idx,,drop=FALSE], ground_p = 2, p = 2,
                              observation.orientation = "colwise",
                              method = transport_method)
  natoms.sub <- length(tplan.sub$tplan$to)
  theta_sort.sub <- t(theta[,tplan.sub$tplan$to]) * sqrt(tplan.sub$tplan$mass)
  mu_sort.sub <- post_mu[,tplan.sub$tplan$from] * matrix(sqrt(tplan.sub$tplan$mass), 
                                                        nrow = n, ncol=natoms.sub, byrow=TRUE)
  for(i in 1:n) {
    dat.subset$temp <- theta_sort.sub[,active.idx,drop=FALSE] * matrix(x[i,active.idx,drop=FALSE], nrow=natoms.sub, ncol=p_act, byrow=TRUE)
    dat.subset$sort_y <- mu_sort.sub[i,]
    dat.subset$xtx = dat.subset$xtx + crossprod(dat.subset$temp)/n
    dat.subset$xty = dat.subset$xty + crossprod(dat.subset$temp, dat.subset$sort_y) /n
  }
  # dat.subset$xtx = dat.subset$xtx
  # dat.subset$xty = dat.subset$xty/(n*s)

  out.subset <- sufficientStatistics(X_ = x[,active.idx], Y_ = post_mu, theta_ = t(theta[active.idx,]),
                                     OTopt)

  testthat::expect_equal(out.subset$XtX, dat.subset$xtx)
  testthat::expect_equal(out.subset$XtY, dat.subset$xty)

  # check projection is just normal crossprods
  proj <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta),
                               OToptproj)
  OToptproj$same <- TRUE
  proj.same <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta),
                                    OToptproj)
  testthat::expect_equal(proj$XtX, xtx)
  testthat::expect_equal(proj$XtY, xty)
  testthat::expect_equal(proj.same$XtX, xtx)
  testthat::expect_equal(proj.same$XtY, xty)

})

testthat::test_that("test greenkhorn suff stat", {
  #check rank (assumes transport_plan is correct!!!!)
  set.seed(222)
  
  #### Setup Data ####
  n <- 128
  p <- 10
  s <- 100
  
  x <- matrix(rnorm(p*n), nrow=n, ncol=p)
  beta <- (1:10)/10
  y <- x %*% beta + rnorm(n)
  
  #posterior
  prec <- crossprod(x) + diag(1,p,p)*1
  mu_post <- solve(prec, crossprod(x,y))
  alpha <- 1 + n/2
  beta <- 1 + 0.5 * (crossprod(y) + t(mu_post) %*% prec %*% mu_post )
  sigma_post <- 1/rgamma(s, alpha, 1/beta)
  theta <- sapply(sigma_post, function(ss) mu_post + t(chol(ss * solve(prec))) %*% matrix(rnorm(p, 0, 1),p,1))
  
  post_mu <- x %*% theta
  # post_diff <- matrix(c(y),nrow=n,ncol=s) + matrix(rnorm(s*n,0,0.01),nrow=n,ncol=s)
  # post_vdiff <- matrix(rnorm(n*s),nrow=n,ncol=s)
  
  xtx <- crossprod(x)/n
  xty <- crossprod(x, post_mu)/n
  
  active.idx <- seq(2,10,2)
  
  transport_method <- "greenkhorn"
  
  OTopt <- list(same = FALSE,
                method = "selection.variable",
                transport.method = transport_method,
                epsilon = 0.05,
                niter = 100)
  OToptproj <- OTopt
  OToptproj$method <- "projection"
  OToptproj$same <- FALSE
  
  # same mu's
  
  tplan <- transport_plan(X = post_mu, Y = post_mu, ground_p = 2, p = 2,
                          observation.orientation = "colwise",
                          method = transport_method, is.X.sorted = FALSE)
  
  dat <- list(temp=matrix(0, n, p), xtx = matrix(0,p,p), xty = rep_len(0, p),
              mu = rep(0, n), idx_mu = rep(0, n),
              sort_y = rep(0, n))
  
  natoms <- length(tplan$tplan$to)
  theta_sort <- t(theta[,tplan$tplan$to]) * matrix(sqrt(tplan$tplan$mass),nrow=natoms, ncol=p)
  mu_sort <- post_mu[,tplan$tplan$from] * matrix(sqrt(tplan$tplan$mass),nrow=n, ncol=natoms, byrow=TRUE)
  
  for(i in 1:n) {
    dat$temp <- theta_sort * matrix(x[i,,drop=FALSE], nrow=natoms, ncol = p, byrow=TRUE)
    dat$sort_y <- mu_sort[i,]
    dat$xtx = dat$xtx + crossprod(dat$temp)/(n)
    dat$xty = dat$xty + crossprod(dat$temp, dat$sort_y) /(n)
  }
  out <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta),
                              OTopt)
  out.trans <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta),
                                    OTopt)
  
  testthat::expect_equal(out$XtX, dat$xtx)
  testthat::expect_equal(out$XtY, dat$xty)
  testthat::expect_equal(out.trans$XtX, dat$xtx)
  testthat::expect_equal(out.trans$XtY, dat$xty)
  
  #check same flag
  OTopt$same <- TRUE
  out.same <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta),
                                   OTopt)
  OTopt$same <- FALSE
  
  dat.same <- list(temp=matrix(0, n, p), xtx = matrix(0,p,p), xty = rep_len(0, p),
                   mu = rep(0, n), idx_mu = rep(0, n),
                   sort_y = rep(0, n))
  
  for(i in 1:n) {
    dat.same$temp <- t(theta) * matrix(x[i,,drop=FALSE], s,p, byrow = TRUE)
    dat.same$mu <- rowSums(dat$temp)
    dat.same$sort_y <- post_mu[i,]
    dat.same$xtx = dat.same$xtx + crossprod(dat.same$temp)/(n*s)
    dat.same$xty = dat.same$xty + crossprod(dat.same$temp, dat.same$sort_y)/(n*s)
  }
  
  testthat::expect_equal(out.same$XtX, dat.same$xtx)
  testthat::expect_equal(dat$xtx, dat.same$xtx)
  testthat::expect_equal(dat$xtx, out.same$XtX)
  testthat::expect_equal(out.same$XtY, dat.same$xty)
  testthat::expect_lt(sqrt(sum((out.same$XtY- dat$xty)^2)), 1e-5)
  testthat::expect_lt(sqrt(sum((out.same$XtY - out$XtY)^2)), 1e-5)
  testthat::expect_lt(sqrt(sum((dat$xty - dat.same$xty)^2)), 1e-5)
  testthat::expect_equal(sqrt(sum((dat$xty - dat.same$xty)^2)), sqrt(sum((out.same$XtY - out$XtY)^2)))
  testthat::expect_equal(sqrt(sum((dat$xty - dat.same$xty)^2)), sqrt(sum((out.same$XtY- dat$xty)^2)))
  
  # check subset of data
  p_act <- length(active.idx)
  dat.subset <- list(temp=matrix(0, s*s, p_act), xtx = matrix(0,p_act,p_act), xty = rep_len(0, p_act),
                     sort_y = rep(0, n))
  
  tplan.sub <- transport_plan(X = post_mu, Y = x[,active.idx,drop=FALSE] %*% theta[active.idx,,drop=FALSE], ground_p = 2, p = 2,
                              observation.orientation = "colwise",
                              method = transport_method)
  natoms.sub <- length(tplan.sub$tplan$to)
  theta_sort.sub <- t(theta[,tplan.sub$tplan$to]) * sqrt(tplan.sub$tplan$mass)
  mu_sort.sub <- post_mu[,tplan.sub$tplan$from] * matrix(sqrt(tplan.sub$tplan$mass), 
                                                         nrow = n, ncol=natoms.sub, byrow=TRUE)
  for(i in 1:n) {
    dat.subset$temp <- theta_sort.sub[,active.idx,drop=FALSE] * matrix(x[i,active.idx,drop=FALSE], nrow=natoms.sub, ncol=p_act, byrow=TRUE)
    dat.subset$sort_y <- mu_sort.sub[i,]
    dat.subset$xtx = dat.subset$xtx + crossprod(dat.subset$temp)/n
    dat.subset$xty = dat.subset$xty + crossprod(dat.subset$temp, dat.subset$sort_y) /n
  }
  # dat.subset$xtx = dat.subset$xtx
  # dat.subset$xty = dat.subset$xty/(n*s)
  
  out.subset <- sufficientStatistics(X_ = x[,active.idx], Y_ = post_mu, theta_ = t(theta[active.idx,]),
                                     OTopt)
  
  testthat::expect_equal(out.subset$XtX, dat.subset$xtx)
  testthat::expect_equal(out.subset$XtY, dat.subset$xty)
  
  # check projection is just normal crossprods
  proj <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta),
                               OToptproj)
  OToptproj$same <- TRUE
  proj.same <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta),
                                    OToptproj)
  testthat::expect_equal(proj$XtX, xtx)
  testthat::expect_equal(proj$XtY, xty)
  testthat::expect_equal(proj.same$XtX, xtx)
  testthat::expect_equal(proj.same$XtY, xty)
  
})

# testthat::test_that("test randkhorn suff stat", {
#   #check rank (assumes transport_plan is correct!!!!)
#   set.seed(222)
#   
#   #### Setup Data ####
#   n <- 128
#   p <- 10
#   s <- 100
#   
#   x <- matrix(rnorm(p*n), nrow=n, ncol=p)
#   beta <- (1:10)/10
#   y <- x %*% beta + rnorm(n)
#   
#   #posterior
#   prec <- crossprod(x) + diag(1,p,p)*1
#   mu_post <- solve(prec, crossprod(x,y))
#   alpha <- 1 + n/2
#   beta <- 1 + 0.5 * (crossprod(y) + t(mu_post) %*% prec %*% mu_post )
#   sigma_post <- 1/rgamma(s, alpha, 1/beta)
#   theta <- sapply(sigma_post, function(ss) mu_post + t(chol(ss * solve(prec))) %*% matrix(rnorm(p, 0, 1),p,1))
#   
#   post_mu <- x %*% theta
#   # post_diff <- matrix(c(y),nrow=n,ncol=s) + matrix(rnorm(s*n,0,0.01),nrow=n,ncol=s)
#   # post_vdiff <- matrix(rnorm(n*s),nrow=n,ncol=s)
#   
#   xtx <- crossprod(x)/n
#   xty <- crossprod(x, post_mu)/n
#   
#   active.idx <- seq(2,10,2)
#   
#   transport_method <- "randkhorn"
#   
#   OTopt <- list(same = FALSE,
#                 method = "selection.variable",
#                 transport.method = transport_method,
#                 epsilon = 0.05,
#                 niter = 100)
#   OToptproj <- OTopt
#   OToptproj$method <- "projection"
#   OToptproj$same <- FALSE
#   
#   # same mu's
#   
#   tplan <- transport_plan(X = post_mu, Y = post_mu, ground_p = 2, p = 2,
#                           observation.orientation = "colwise",
#                           method = transport_method, is.X.sorted = FALSE)
#   
#   dat <- list(temp=matrix(0, n, p), xtx = matrix(0,p,p), xty = rep_len(0, p),
#               mu = rep(0, n), idx_mu = rep(0, n),
#               sort_y = rep(0, n))
#   
#   natoms <- length(tplan$tplan$to)
#   theta_sort <- t(theta[,tplan$tplan$to]) * matrix(sqrt(tplan$tplan$mass),nrow=natoms, ncol=p)
#   mu_sort <- post_mu[,tplan$tplan$from] * matrix(sqrt(tplan$tplan$mass),nrow=n, ncol=natoms, byrow=TRUE)
#   
#   for(i in 1:n) {
#     dat$temp <- theta_sort * matrix(x[i,,drop=FALSE], nrow=natoms, ncol = p, byrow=TRUE)
#     dat$sort_y <- mu_sort[i,]
#     dat$xtx = dat$xtx + crossprod(dat$temp)/(n)
#     dat$xty = dat$xty + crossprod(dat$temp, dat$sort_y) /(n)
#   }
#   out <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta),
#                               OTopt)
#   out.trans <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta),
#                                     OTopt)
#   
#   testthat::expect_equal(out$XtX, dat$xtx)
#   testthat::expect_equal(out$XtY, dat$xty)
#   testthat::expect_equal(out.trans$XtX, dat$xtx)
#   testthat::expect_equal(out.trans$XtY, dat$xty)
#   
#   #check same flag
#   OTopt$same <- TRUE
#   out.same <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta),
#                                    OTopt)
#   OTopt$same <- FALSE
#   
#   dat.same <- list(temp=matrix(0, n, p), xtx = matrix(0,p,p), xty = rep_len(0, p),
#                    mu = rep(0, n), idx_mu = rep(0, n),
#                    sort_y = rep(0, n))
#   
#   for(i in 1:n) {
#     dat.same$temp <- t(theta) * matrix(x[i,,drop=FALSE], s,p, byrow = TRUE)
#     dat.same$mu <- rowSums(dat$temp)
#     dat.same$sort_y <- post_mu[i,]
#     dat.same$xtx = dat.same$xtx + crossprod(dat.same$temp)/(n*s)
#     dat.same$xty = dat.same$xty + crossprod(dat.same$temp, dat.same$sort_y)/(n*s)
#   }
#   
#   testthat::expect_equal(out.same$XtX, dat.same$xtx)
#   testthat::expect_equal(dat$xtx, dat.same$xtx)
#   testthat::expect_equal(dat$xtx, out.same$XtX)
#   testthat::expect_equal(out.same$XtY, dat.same$xty)
#   testthat::expect_lt(sqrt(sum((out.same$XtY- dat$xty)^2)), 1e-5)
#   testthat::expect_lt(sqrt(sum((out.same$XtY - out$XtY)^2)), 1e-5)
#   testthat::expect_lt(sqrt(sum((dat$xty - dat.same$xty)^2)), 1e-5)
#   testthat::expect_equal(sqrt(sum((dat$xty - dat.same$xty)^2)), sqrt(sum((out.same$XtY - out$XtY)^2)))
#   testthat::expect_equal(sqrt(sum((dat$xty - dat.same$xty)^2)), sqrt(sum((out.same$XtY- dat$xty)^2)))
#   
#   # check subset of data
#   p_act <- length(active.idx)
#   dat.subset <- list(temp=matrix(0, s*s, p_act), xtx = matrix(0,p_act,p_act), xty = rep_len(0, p_act),
#                      sort_y = rep(0, n))
#   
#   tplan.sub <- transport_plan(X = post_mu, Y = x[,active.idx,drop=FALSE] %*% theta[active.idx,,drop=FALSE], ground_p = 2, p = 2,
#                               observation.orientation = "colwise",
#                               method = transport_method)
#   natoms.sub <- length(tplan.sub$tplan$to)
#   theta_sort.sub <- t(theta[,tplan.sub$tplan$to]) * sqrt(tplan.sub$tplan$mass)
#   mu_sort.sub <- post_mu[,tplan.sub$tplan$from] * matrix(sqrt(tplan.sub$tplan$mass), 
#                                                          nrow = n, ncol=natoms.sub, byrow=TRUE)
#   for(i in 1:n) {
#     dat.subset$temp <- theta_sort.sub[,active.idx,drop=FALSE] * matrix(x[i,active.idx,drop=FALSE], nrow=natoms.sub, ncol=p_act, byrow=TRUE)
#     dat.subset$sort_y <- mu_sort.sub[i,]
#     dat.subset$xtx = dat.subset$xtx + crossprod(dat.subset$temp)/n
#     dat.subset$xty = dat.subset$xty + crossprod(dat.subset$temp, dat.subset$sort_y) /n
#   }
#   # dat.subset$xtx = dat.subset$xtx
#   # dat.subset$xty = dat.subset$xty/(n*s)
#   
#   out.subset <- sufficientStatistics(X_ = x[,active.idx], Y_ = post_mu, theta_ = t(theta[active.idx,]),
#                                      OTopt)
#   
#   testthat::expect_equal(out.subset$XtX, dat.subset$xtx)
#   testthat::expect_equal(out.subset$XtY, dat.subset$xty)
#   
#   # check projection is just normal crossprods
#   proj <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta),
#                                OToptproj)
#   OToptproj$same <- TRUE
#   proj.same <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta),
#                                     OToptproj)
#   testthat::expect_equal(proj$XtX, xtx)
#   testthat::expect_equal(proj$XtY, xty)
#   testthat::expect_equal(proj.same$XtX, xtx)
#   testthat::expect_equal(proj.same$XtY, xty)
#   
# })
# 
# testthat::test_that("test gandkhorn suff stat", {
#   #check rank (assumes transport_plan is correct!!!!)
#   set.seed(222)
#   
#   #### Setup Data ####
#   n <- 128
#   p <- 10
#   s <- 100
#   
#   x <- matrix(rnorm(p*n), nrow=n, ncol=p)
#   beta <- (1:10)/10
#   y <- x %*% beta + rnorm(n)
#   
#   #posterior
#   prec <- crossprod(x) + diag(1,p,p)*1
#   mu_post <- solve(prec, crossprod(x,y))
#   alpha <- 1 + n/2
#   beta <- 1 + 0.5 * (crossprod(y) + t(mu_post) %*% prec %*% mu_post )
#   sigma_post <- 1/rgamma(s, alpha, 1/beta)
#   theta <- sapply(sigma_post, function(ss) mu_post + t(chol(ss * solve(prec))) %*% matrix(rnorm(p, 0, 1),p,1))
#   
#   post_mu <- x %*% theta
#   # post_diff <- matrix(c(y),nrow=n,ncol=s) + matrix(rnorm(s*n,0,0.01),nrow=n,ncol=s)
#   # post_vdiff <- matrix(rnorm(n*s),nrow=n,ncol=s)
#   
#   xtx <- crossprod(x)/n
#   xty <- crossprod(x, post_mu)/n
#   
#   active.idx <- seq(2,10,2)
#   
#   transport_method <- "gandkhorn"
#   
#   OTopt <- list(same = FALSE,
#                 method = "selection.variable",
#                 transport.method = transport_method,
#                 epsilon = 0.05,
#                 niter = 100)
#   OToptproj <- OTopt
#   OToptproj$method <- "projection"
#   OToptproj$same <- FALSE
#   
#   # same mu's
#   
#   tplan <- transport_plan(X = post_mu, Y = post_mu, ground_p = 2, p = 2,
#                           observation.orientation = "colwise",
#                           method = transport_method, is.X.sorted = FALSE)
#   
#   dat <- list(temp=matrix(0, n, p), xtx = matrix(0,p,p), xty = rep_len(0, p),
#               mu = rep(0, n), idx_mu = rep(0, n),
#               sort_y = rep(0, n))
#   
#   natoms <- length(tplan$tplan$to)
#   theta_sort <- t(theta[,tplan$tplan$to]) * matrix(sqrt(tplan$tplan$mass),nrow=natoms, ncol=p)
#   mu_sort <- post_mu[,tplan$tplan$from] * matrix(sqrt(tplan$tplan$mass),nrow=n, ncol=natoms, byrow=TRUE)
#   
#   for(i in 1:n) {
#     dat$temp <- theta_sort * matrix(x[i,,drop=FALSE], nrow=natoms, ncol = p, byrow=TRUE)
#     dat$sort_y <- mu_sort[i,]
#     dat$xtx = dat$xtx + crossprod(dat$temp)/(n)
#     dat$xty = dat$xty + crossprod(dat$temp, dat$sort_y) /(n)
#   }
#   out <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta),
#                               OTopt)
#   out.trans <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta),
#                                     OTopt)
#   
#   testthat::expect_equal(out$XtX, dat$xtx)
#   testthat::expect_equal(out$XtY, dat$xty)
#   testthat::expect_equal(out.trans$XtX, dat$xtx)
#   testthat::expect_equal(out.trans$XtY, dat$xty)
#   
#   #check same flag
#   OTopt$same <- TRUE
#   out.same <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta),
#                                    OTopt)
#   OTopt$same <- FALSE
#   
#   dat.same <- list(temp=matrix(0, n, p), xtx = matrix(0,p,p), xty = rep_len(0, p),
#                    mu = rep(0, n), idx_mu = rep(0, n),
#                    sort_y = rep(0, n))
#   
#   for(i in 1:n) {
#     dat.same$temp <- t(theta) * matrix(x[i,,drop=FALSE], s,p, byrow = TRUE)
#     dat.same$mu <- rowSums(dat$temp)
#     dat.same$sort_y <- post_mu[i,]
#     dat.same$xtx = dat.same$xtx + crossprod(dat.same$temp)/(n*s)
#     dat.same$xty = dat.same$xty + crossprod(dat.same$temp, dat.same$sort_y)/(n*s)
#   }
#   
#   testthat::expect_equal(out.same$XtX, dat.same$xtx)
#   testthat::expect_equal(dat$xtx, dat.same$xtx)
#   testthat::expect_equal(dat$xtx, out.same$XtX)
#   testthat::expect_equal(out.same$XtY, dat.same$xty)
#   testthat::expect_lt(sqrt(sum((out.same$XtY- dat$xty)^2)), 1e-5)
#   testthat::expect_lt(sqrt(sum((out.same$XtY - out$XtY)^2)), 1e-5)
#   testthat::expect_lt(sqrt(sum((dat$xty - dat.same$xty)^2)), 1e-5)
#   testthat::expect_equal(sqrt(sum((dat$xty - dat.same$xty)^2)), sqrt(sum((out.same$XtY - out$XtY)^2)))
#   testthat::expect_equal(sqrt(sum((dat$xty - dat.same$xty)^2)), sqrt(sum((out.same$XtY- dat$xty)^2)))
#   
#   # check subset of data
#   p_act <- length(active.idx)
#   dat.subset <- list(temp=matrix(0, s*s, p_act), xtx = matrix(0,p_act,p_act), xty = rep_len(0, p_act),
#                      sort_y = rep(0, n))
#   
#   tplan.sub <- transport_plan(X = post_mu, Y = x[,active.idx,drop=FALSE] %*% theta[active.idx,,drop=FALSE], ground_p = 2, p = 2,
#                               observation.orientation = "colwise",
#                               method = transport_method)
#   natoms.sub <- length(tplan.sub$tplan$to)
#   theta_sort.sub <- t(theta[,tplan.sub$tplan$to]) * sqrt(tplan.sub$tplan$mass)
#   mu_sort.sub <- post_mu[,tplan.sub$tplan$from] * matrix(sqrt(tplan.sub$tplan$mass), 
#                                                          nrow = n, ncol=natoms.sub, byrow=TRUE)
#   for(i in 1:n) {
#     dat.subset$temp <- theta_sort.sub[,active.idx,drop=FALSE] * matrix(x[i,active.idx,drop=FALSE], nrow=natoms.sub, ncol=p_act, byrow=TRUE)
#     dat.subset$sort_y <- mu_sort.sub[i,]
#     dat.subset$xtx = dat.subset$xtx + crossprod(dat.subset$temp)/n
#     dat.subset$xty = dat.subset$xty + crossprod(dat.subset$temp, dat.subset$sort_y) /n
#   }
#   # dat.subset$xtx = dat.subset$xtx
#   # dat.subset$xty = dat.subset$xty/(n*s)
#   
#   out.subset <- sufficientStatistics(X_ = x[,active.idx], Y_ = post_mu, theta_ = t(theta[active.idx,]),
#                                      OTopt)
#   
#   testthat::expect_equal(out.subset$XtX, dat.subset$xtx)
#   testthat::expect_equal(out.subset$XtY, dat.subset$xty)
#   
#   # check projection is just normal crossprods
#   proj <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta),
#                                OToptproj)
#   OToptproj$same <- TRUE
#   proj.same <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta),
#                                     OToptproj)
#   testthat::expect_equal(proj$XtX, xtx)
#   testthat::expect_equal(proj$XtY, xty)
#   testthat::expect_equal(proj.same$XtX, xtx)
#   testthat::expect_equal(proj.same$XtY, xty)
#   
# })

