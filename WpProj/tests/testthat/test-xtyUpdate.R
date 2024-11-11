test_that("test univariate approximation xtyupdate", {
  set.seed(12431)
  
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
  res <- rep(1,p)
  
  
  active.idx <- seq(2,10,2)
  res.sub <- rep(0,p)
  res.sub[active.idx] <-1 
  
  transport_method <- "univariate.approximation.pwr"
  
  OToptions.sel.var <- list(same = FALSE,
                            method = "selection.variable",
                            transport.method = transport_method,
                            epsilon = 0.05,
                            niter = 100)
  
  OToptions.proj <- list(same = FALSE,
                            method = "projection",
                            transport.method = transport_method,
                            epsilon = 0.05,
                            niter = 100)
  
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
  
  #min row 1 post_mu = 0.2973531
  #entry 1 post_mu = 0.3004788
  out <- xtyUpdate(X_ = x, Y_ = post_mu, theta_ = t(theta), result_ = res,
                   OToptions.sel.var)
  outcheck <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta),
                                   OToptions.sel.var)
  
  OTopt.notrans <- OToptions.sel.var
  OTopt.notrans$same <- TRUE
  out_no_trans <- xtyUpdate(X_ = x, Y_ = post_mu, theta_ = theta, result_=res,
                            OTopt.notrans)
  testthat::expect_equal(out, dat$xty)
  testthat::expect_equal(outcheck$XtY, dat$xty)
  testthat::expect_equal(out, outcheck$XtY)
  testthat::expect_equal(out_no_trans, dat$xty)
  testthat::expect_equal(out_no_trans, out)
  
  #check to see that suffstat fun gives same answers
  
  OToptsame <- OToptions.sel.var
  OToptsame$same <- TRUE
  out.same <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta), 
                                   OToptsame)
  
  testthat::expect_equal(out.same$XtY, out)
  testthat::expect_equal(out.same$XtY, dat$xty)
  
  # check subset of data
  dat.subset <- list(temp=matrix(0, n, p), xtx = matrix(0,p,p), xty = rep_len(0, p),
                     mu = rep(0, n), idx_mu = rep(0, n),
                     sort_y = rep(0, n))
  
  for(i in 1:n) {
    dat.subset$temp <- t(theta) * matrix(x[i,,drop=FALSE], s,p, byrow = TRUE)
    dat.subset$mu <- dat.subset$temp %*% res.sub
    dat.subset$idx_mu <- order(dat.subset$mu)
    dat.subset$sort_y <- sort(post_mu[i,])
    dat.subset$xtx = dat.subset$xtx + crossprod(dat.subset$temp)
    dat.subset$xty = dat.subset$xty + crossprod(dat.subset$temp[dat.subset$idx_mu,,drop=FALSE], dat.subset$sort_y)
  }
  dat.subset$xtx = dat.subset$xtx/(n*s)
  dat.subset$xty = dat.subset$xty/(n*s)
  
  out.subset <- xtyUpdate(X_ = x, Y_ = post_mu, theta_ = theta, result_=res.sub,
                          OToptions.sel.var)
  
  
  testthat::expect_equal(out.subset, dat.subset$xty)

  # check projection gives error
  testthat::expect_error( xtyUpdate(X_ = x, Y_ = post_mu, 
                                   theta_ = theta, result_=res.sub,
                                   OToptions.proj) )
  
  # check nonsense entry for method gives error
  OToptions.sel.var.error <- OToptions.sel.var
  OToptions.sel.var.error$method <- "thumb"
  testthat::expect_error( xtyUpdate(X_ = x, Y_ = post_mu, theta_ = theta, result_=res.sub,
                                    OToptions.sel.var.error) )
  
  #check nonsense transport gives error
  OToptions.sel.var.error2 <- OToptions.sel.var
  OToptions.sel.var.error2$transport.method <- "ideal"
  testthat::expect_error( xtyUpdate(X_ = x, Y_ = post_mu, 
                                    theta_ = theta, result_=res.sub,
                                    OToptions.sel.var.error2) )
  
})

testthat::test_that("test hilbert xtyupdate", {
  #check hilbert (assumes transport_plan is correct!!!!)
  
  set.seed(12431)
  
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
  res <- rep(1,p)
  
  
  active.idx <- seq(2,10,2)
  res.sub <- rep(0,p)
  res.sub[active.idx] <-1 
  
  transport_method <- "hilbert"
  OToptions.sel.var <- list(same = FALSE,
                            method = "selection.variable",
                            transport.method = transport_method,
                            epsilon = 0.05,
                            niter = 100)
  
  OToptions.proj <- list(same = TRUE,
                         method = "projection",
                         transport.method = transport_method,
                         epsilon = 0.05,
                         niter = 100)
  
  # same mu's
  
  tplan <- transport_plan(X = post_mu, Y = post_mu, ground_p = 2, p = 2, 
                          observation.orientation = "colwise", 
                          method = transport_method, is.X.sorted = TRUE)
  
  dat <- list(temp=matrix(0, n, p), xtx = matrix(0,p,p), xty = rep_len(0, p),
              mu = rep(0, n), idx_mu = rep(0, n),
              sort_y = rep(0, n))
  orders <- order(tplan$tplan$from)
  mu_order <- post_mu[,orders]
  theta_order <- theta[,orders]
  
  for(i in 1:n) {
    dat$temp <- t(theta_order) * matrix(x[i,,drop=FALSE], s,p, byrow = TRUE)
    dat$mu <- rowSums(dat$temp)
    dat$sort_y <- mu_order[i,]
    dat$xtx = dat$xtx + crossprod(dat$temp)
    dat$xty = dat$xty + crossprod(dat$temp, dat$sort_y)
  }
  dat$xtx = dat$xtx/(n*s)
  dat$xty = dat$xty/(n*s)
  out <- xtyUpdate(X_ = x, Y_ = post_mu, theta_ = t(theta), result_ = res,
                   OToptions.sel.var)
  
  OToptions.sel.var.notransp <- OToptions.sel.var
  OToptions.sel.var.notransp$same = TRUE
  out_no_trans <- xtyUpdate(X_ = x, Y_ = post_mu, theta_ = theta, result_=res,
                            OToptions.sel.var.notransp)
  testthat::expect_equal(out, dat$xty)
  testthat::expect_equal(out_no_trans, dat$xty)
  testthat::expect_equal(out_no_trans, out)
  
  #check to see that suffstat fun gives same answers
  
  out.same <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta), 
                                   OToptions.sel.var)
  
  testthat::expect_equal(out.same$XtY, out)
  testthat::expect_equal(out.same$XtY, dat$xty)
  
  # check with different result tab of data
  dat.subset <- list(temp=matrix(0, n, p), xtx = matrix(0,p,p), xty = rep_len(0, p),
                     mu = rep(0, n), idx_mu = rep(0, n),
                     sort_y = rep(0, n))
  
  tplan.sub <- transport_plan(X = post_mu, Y = x[,active.idx,drop=FALSE] %*% theta[active.idx,,drop=FALSE], ground_p = 2, p = 2, 
                              observation.orientation = "colwise", 
                              method = transport_method, is.X.sorted = FALSE)
  #cost out of sorts because I'm lying to the program about x being sorted
  orders.sub <- order(tplan.sub$tplan$from)
  mu_order.sub <- post_mu[,orders.sub]
  theta_order.sub <- theta[,tplan.sub$tplan$to]
  
  for(i in 1:n) {
    dat.subset$temp <- t(theta_order.sub) * matrix(x[i,,drop=FALSE], s,p, byrow = TRUE)
    # dat.subset$mu <- rowSums(dat.subset$temp[,active.idx])
    dat.subset$sort_y <- mu_order.sub[i,]
    dat.subset$xtx = dat.subset$xtx + crossprod(dat.subset$temp)/(n*s)
    dat.subset$xty = dat.subset$xty + crossprod(dat.subset$temp, dat.subset$sort_y)/(n*s)
  }
  
  out.subset <- xtyUpdate(X_ = x, Y_ = post_mu, theta_ = theta, result_=res.sub,
                          OToptions.sel.var)
  
  
  testthat::expect_equal(out.subset, dat.subset$xty)
  
  # check projection gives error
  testthat::expect_error( xtyUpdate(X_ = x, Y_ = post_mu, 
                                    theta_ = theta, result_=res.sub,
                                    OToptions.proj) )
  
  # check nonsense entry for method gives error
  OToptions.sel.var.error <- OToptions.sel.var
  OToptions.sel.var.error$method <- "thumb"
  testthat::expect_error( xtyUpdate(X_ = x, Y_ = post_mu, theta_ = theta, result_=res.sub,
                                    OToptions.sel.var.error) )
  
  #check nonsense transport gives error
  OToptions.sel.var.error2 <- OToptions.sel.var
  OToptions.sel.var.error2$transport.method <- "ideal"
  testthat::expect_error( xtyUpdate(X_ = x, Y_ = post_mu, 
                                    theta_ = theta, result_=res.sub,
                                    OToptions.sel.var.error2) )
  
})

testthat::test_that("test rank xtyupdate", {
  #check hilbert (assumes transport_plan is correct!!!!)
  
  set.seed(12431)
  
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
  res <- rep(1,p)
  
  
  active.idx <- seq(2,10,2)
  res.sub <- rep(0,p)
  res.sub[active.idx] <-1 
  
  transport_method <- "rank"
  OToptions.sel.var <- list(same = FALSE,
                            method = "selection.variable",
                            transport.method = transport_method,
                            epsilon = 0.05,
                            niter = 100)
  
  OToptions.proj <- list(same = TRUE,
                         method = "projection",
                         transport.method = transport_method,
                         epsilon = 0.05,
                         niter = 100)
  
  # same mu's
  
  tplan <- transport_plan(X = post_mu, Y = post_mu, ground_p = 2, p = 2, 
                          observation.orientation = "colwise", 
                          method = transport_method, is.X.sorted = TRUE)
  
  dat <- list(temp=matrix(0, n, p), xtx = matrix(0,p,p), xty = rep_len(0, p),
              mu = rep(0, n), idx_mu = rep(0, n),
              sort_y = rep(0, n))
  orders <- order(tplan$tplan$from)
  mu_order <- post_mu[,orders]
  theta_order <- theta[,orders]
  
  for(i in 1:n) {
    dat$temp <- t(theta_order) * matrix(x[i,,drop=FALSE], s,p, byrow = TRUE)
    dat$mu <- rowSums(dat$temp)
    dat$sort_y <- mu_order[i,]
    dat$xtx = dat$xtx + crossprod(dat$temp)
    dat$xty = dat$xty + crossprod(dat$temp, dat$sort_y)
  }
  dat$xtx = dat$xtx/(n*s)
  dat$xty = dat$xty/(n*s)
  out <- xtyUpdate(X_ = x, Y_ = post_mu, theta_ = t(theta), result_ = res,
                   OToptions.sel.var)
  
  OToptions.sel.var.notransp <- OToptions.sel.var
  OToptions.sel.var.notransp$same = TRUE
  out_no_trans <- xtyUpdate(X_ = x, Y_ = post_mu, theta_ = theta, result_=res,
                            OToptions.sel.var.notransp)
  testthat::expect_equal(out, dat$xty)
  testthat::expect_equal(out_no_trans, dat$xty)
  testthat::expect_equal(out_no_trans, out)
  
  #check to see that suffstat fun gives same answers
  
  out.same <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta), 
                                   OToptions.sel.var)
  
  testthat::expect_equal(out.same$XtY, out)
  testthat::expect_equal(out.same$XtY, dat$xty)
  
  # check with different result tab of data
  dat.subset <- list(temp=matrix(0, n, p), xtx = matrix(0,p,p), xty = rep_len(0, p),
                     mu = rep(0, n), idx_mu = rep(0, n),
                     sort_y = rep(0, n))
  
  tplan.sub <- transport_plan(X = post_mu, Y = x[,active.idx,drop=FALSE] %*% theta[active.idx,,drop=FALSE], ground_p = 2, p = 2, 
                              observation.orientation = "colwise", 
                              method = transport_method, is.X.sorted = FALSE)
  #cost out of sorts because I'm lying to the program about x being sorted
  orders.sub <- order(tplan.sub$tplan$from)
  mu_order.sub <- post_mu[,orders.sub]
  theta_order.sub <- theta[,tplan.sub$tplan$to]
  
  for(i in 1:n) {
    dat.subset$temp <- t(theta_order.sub) * matrix(x[i,,drop=FALSE], s,p, byrow = TRUE)
    # dat.subset$mu <- rowSums(dat.subset$temp[,active.idx])
    dat.subset$sort_y <- mu_order.sub[i,]
    dat.subset$xtx = dat.subset$xtx + crossprod(dat.subset$temp)/(n*s)
    dat.subset$xty = dat.subset$xty + crossprod(dat.subset$temp, dat.subset$sort_y)/(n*s)
  }
  
  out.subset <- xtyUpdate(X_ = x, Y_ = post_mu, theta_ = theta, result_=res.sub,
                          OToptions.sel.var)
  
  
  testthat::expect_equal(out.subset, dat.subset$xty)
  
  # check projection gives error
  testthat::expect_error( xtyUpdate(X_ = x, Y_ = post_mu, 
                                    theta_ = theta, result_=res.sub,
                                    OToptions.proj) )
  
  # check nonsense entry for method gives error
  OToptions.sel.var.error <- OToptions.sel.var
  OToptions.sel.var.error$method <- "thumb"
  testthat::expect_error( xtyUpdate(X_ = x, Y_ = post_mu, theta_ = theta, result_=res.sub,
                                    OToptions.sel.var.error) )
  
  #check nonsense transport gives error
  OToptions.sel.var.error2 <- OToptions.sel.var
  OToptions.sel.var.error2$transport.method <- "ideal"
  testthat::expect_error( xtyUpdate(X_ = x, Y_ = post_mu, 
                                    theta_ = theta, result_=res.sub,
                                    OToptions.sel.var.error2) )
  
})

testthat::test_that("test exact xtyupdate", {
  #check shortsimplex (assumes transport_plan is correct!!!!)
  
  set.seed(12431)
  
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
  res <- rep(1,p)
  
  
  active.idx <- seq(2,10,2)
  res.sub <- rep(0,p)
  res.sub[active.idx] <-1 
  
  transport_method <- "exact"
  OToptions.sel.var <- list(same = TRUE,
                            method = "selection.variable",
                            transport.method = transport_method,
                            epsilon = 0.05,
                            niter = 100)
  
  OToptions.proj <- list(same = TRUE,
                         method = "projection",
                         transport.method = transport_method,
                         epsilon = 0.05,
                         niter = 100)
  
  # same mu's
  
  tplan <- transport_plan(X = post_mu, Y = post_mu, ground_p = 2, p = 2, 
                          observation.orientation = "colwise", 
                          method = transport_method, is.X.sorted = TRUE)
  #cost out of sorts because I'm lying to the program about x being sorted
  trans_idx <- tplan$tplan$to
  
  dat <- list(temp=matrix(0, n, p), xtx = matrix(0,p,p), xty = rep_len(0, p),
              mu = rep(0, n), idx_mu = rep(0, n),
              sort_y = rep(0, n))
  
  for(i in 1:n) {
    dat$temp <- t(theta) * matrix(x[i,,drop=FALSE], s,p, byrow = TRUE)
    dat$mu <- rowSums(dat$temp)
    dat$idx_mu <- trans_idx
    dat$sort_y <- post_mu[i,trans_idx]
    dat$xtx = dat$xtx + crossprod(dat$temp)
    dat$xty = dat$xty + crossprod(dat$temp[dat$idx_mu,,drop=FALSE], dat$sort_y)
  }
  dat$xtx = dat$xtx/(n*s)
  dat$xty = dat$xty/(n*s)
  
  out <- xtyUpdate(X_ = x, Y_ = post_mu, theta_ = t(theta), result_ = res,
                   OToptions.sel.var)
  
  out_no_trans <- xtyUpdate(X_ = x, Y_ = post_mu, theta_ = theta, result_=res,
                            OToptions.sel.var)
  testthat::expect_equal(out, dat$xty)
  testthat::expect_equal(out_no_trans, dat$xty)
  testthat::expect_equal(out_no_trans, out)
  
  #check to see that suffstat fun gives same answers
  
  out.same <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta), 
                                   OToptions.sel.var)
  
  testthat::expect_equal(out.same$XtY, out)
  testthat::expect_equal(out.same$XtY, dat$xty)
  
  # check with different result tab of data
  dat.subset <- list(temp=matrix(0, n, p), xtx = matrix(0,p,p), xty = rep_len(0, p),
                     mu = rep(0, n), idx_mu = rep(0, n),
                     sort_y = rep(0, n))
  
  tplan.sub <- transport_plan(X = post_mu, Y = x[,active.idx,drop=FALSE] %*% theta[active.idx,,drop=FALSE], ground_p = 2, p = 2, 
                              observation.orientation = "colwise", 
                              method = transport_method, is.X.sorted = FALSE)
  #cost out of sorts because I'm lying to the program about x being sorted
  trans_idx.sub <- tplan.sub$tplan$from
  
  for(i in 1:n) {
    dat.subset$temp <- t(theta[,drop=FALSE]) * matrix(x[i,,drop=FALSE], s, p, byrow = TRUE)
    # dat.subset$mu <- rowSums(dat.subset$temp)
    dat.subset$sort_y <- post_mu[i,trans_idx.sub] # no sorting needed here
    dat.subset$xtx = dat.subset$xtx + crossprod(dat.subset$temp)/(n*s)
    dat.subset$xty = dat.subset$xty + crossprod(dat.subset$temp, dat.subset$sort_y)/(n*s)
  }
  # dat.subset$xtx = dat.subset$xtx
  # dat.subset$xty = dat.subset$xty/(n*s)
  
  out.subset <- xtyUpdate(X_ = x, Y_ = post_mu, theta_ = theta, result_=res.sub,
                          OToptions.sel.var)
  
  
  testthat::expect_equal(out.subset, dat.subset$xty)
  
  # check projection gives error
  testthat::expect_error( xtyUpdate(X_ = x, Y_ = post_mu, 
                                    theta_ = theta, result_=res.sub,
                                    OToptions.proj) )
  
  # check nonsense entry for method gives error
  OToptions.sel.var.error <- OToptions.sel.var
  OToptions.sel.var.error$method <- "thumb"
  testthat::expect_error( xtyUpdate(X_ = x, Y_ = post_mu, theta_ = theta, result_=res.sub,
                                    OToptions.sel.var.error) )
  
  #check nonsense transport gives error
  OToptions.sel.var.error2 <- OToptions.sel.var
  OToptions.sel.var.error2$transport.method <- "ideal"
  testthat::expect_error( xtyUpdate(X_ = x, Y_ = post_mu, 
                                    theta_ = theta, result_=res.sub,
                                    OToptions.sel.var.error2) )
  
})

testthat::test_that("test shortsimplex xtyupdate", {
  #check shortsimplex (assumes transport_plan is correct!!!!)
  
  set.seed(12431)
  
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
  res <- rep(1,p)
  
  
  active.idx <- seq(2,10,2)
  res.sub <- rep(0,p)
  res.sub[active.idx] <-1 
  
  transport_method <- "shortsimplex"
  tplan_transport <- "exact"
  
  OToptions.sel.var <- list(same = TRUE,
                            method = "selection.variable",
                            transport.method = transport_method,
                            epsilon = 0.05,
                            niter = 100)
  
  OToptions.proj <- list(same = TRUE,
                         method = "projection",
                         transport.method = transport_method,
                         epsilon = 0.05,
                         niter = 100)
  
  # same mu's
  
  tplan <- transport_plan(X = post_mu, Y = post_mu, ground_p = 2, p = 2, 
                          observation.orientation = "colwise", 
                          method = tplan_transport, is.X.sorted = TRUE)
  #cost out of sorts because I'm lying to the program about x being sorted
  trans_idx <- tplan$tplan$from
  
  dat <- list(temp=matrix(0, n, p), xtx = matrix(0,p,p), xty = rep_len(0, p),
              mu = rep(0, n), idx_mu = rep(0, n),
              sort_y = rep(0, n))
  
  for(i in 1:n) {
    dat$temp <- t(theta) * matrix(x[i,,drop=FALSE], s,p, byrow = TRUE)
    dat$mu <- rowSums(dat$temp)
    dat$idx_mu <- trans_idx
    dat$sort_y <- post_mu[i,tplan$tplan$from]
    dat$xtx = dat$xtx + crossprod(dat$temp)
    dat$xty = dat$xty + crossprod(dat$temp[dat$idx_mu,,drop=FALSE], dat$sort_y)
  }
  dat$xtx = dat$xtx/(n*s)
  dat$xty = dat$xty/(n*s)
  
  out <- xtyUpdate(X_ = x, Y_ = post_mu, theta_ = theta, result_ = res,
                   OToptions.sel.var)
  out.trans <- xtyUpdate(X_ = x, Y_ = post_mu, theta_ = t(theta), result_ = res,
                         OToptions.sel.var)
  
  testthat::expect_equal(out, dat$xty)
  testthat::expect_equal(out.trans, dat$xty)
  
  
  # check with different result tab of data
  dat.subset <- list(temp=matrix(0, n, p), xtx = matrix(0,p,p), xty = rep_len(0, p),
                     mu = rep(0, n), idx_mu = rep(0, n),
                     sort_y = rep(0, n))
  
  tplan.sub <- transport_plan(X = post_mu, Y = x[,active.idx,drop=FALSE] %*% theta[active.idx,,drop=FALSE], ground_p = 2, p = 2, 
                              observation.orientation = "colwise", 
                              method = tplan_transport, is.X.sorted = FALSE)
  #cost out of sorts because I'm lying to the program about x being sorted
  trans_idx.sub <- tplan.sub$tplan$from
  
  for(i in 1:n) {
    dat.subset$temp <- t(theta[,trans_idx.sub,drop=FALSE]) * matrix(x[i,,drop=FALSE], s, p, byrow = TRUE)
    # dat.subset$mu <- rowSums(dat.subset$temp)
    dat.subset$sort_y <- post_mu[i,] # no sorting needed here
    dat.subset$xtx = dat.subset$xtx + crossprod(dat.subset$temp)/(n*s)
    dat.subset$xty = dat.subset$xty + crossprod(dat.subset$temp, dat.subset$sort_y)/(n*s)
  }
  # dat.subset$xtx = dat.subset$xtx
  # dat.subset$xty = dat.subset$xty/(n*s)
  
  out.subset <- xtyUpdate(X_ = x, Y_ = post_mu, theta_ = t(theta), result_ = res.sub, 
                          OToptions.sel.var)
  
  testthat::expect_equal(out.subset, dat.subset$xty)
  
  # check proper errors
  testthat::expect_error(xtyUpdate(X_ = x, Y_ = post_mu, theta_ = t(theta), result_ = res,
                                   OToptions.proj))
  
  OTopterror <- OToptions.sel.var
  OTopterror$transport.method <- "ideal"
  testthat::expect_error(xtyUpdate(X_ = x, Y_ = post_mu, theta_ = t(theta), result_ = res,
                                   OTopterror))
  
})

testthat::test_that("test sinkhorn xtyupdate", {
  set.seed(12431)
  
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
  res <- rep(1,p)
  
  
  active.idx <- seq(2,10,2)
  res.sub <- rep(0,p)
  res.sub[active.idx] <-1 
  
  transport_method <- "sinkhorn"
  OToptions.sel.var <- list(same = TRUE,
                            method = "selection.variable",
                            transport.method = transport_method,
                            epsilon = 0.05,
                            niter = 100)
  
  OToptions.proj <- list(same = TRUE,
                         method = "projection",
                         transport.method = transport_method,
                         epsilon = 0.05,
                         niter = 100)
  
  # same mu's
  
  tplan <- transport_plan(X = post_mu, Y = post_mu, ground_p = 2, p = 2, 
                          observation.orientation = "colwise", 
                          method = transport_method, is.X.sorted = TRUE)
  #cost out of sorts because I'm lying to the program about x being sorted
  
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
  
  out <- xtyUpdate(X_ = x, Y_ = post_mu, theta_ = t(theta), result_ = res,
                   OToptions.sel.var)
  
  out_no_trans <- xtyUpdate(X_ = x, Y_ = post_mu, theta_ = theta, result_=res,
                            OToptions.sel.var)
  testthat::expect_equal(out, dat$xty)
  testthat::expect_equal(out_no_trans, dat$xty)
  testthat::expect_equal(out_no_trans, out)
  
  #check to see that suffstat fun gives same answers
  
  out.same <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta), 
                                   OToptions.sel.var)
  
  testthat::expect_lt(norm(out.same$XtY-out, "F"), 7e-8)
  testthat::expect_lt(norm(out.same$XtY - dat$xty, "F"), 7e-8)
  testthat::expect_equal(norm(out.same$XtY-out, "F"), 
                         norm(out.same$XtY - dat$xty, "F"))
  
  # check with different result tab of data
  p_act <- length(active.idx)
  dat.subset <- list(temp=matrix(0, s*s, p), xtx = matrix(0,p,p), xty = rep_len(0, p),
                     sort_y = rep(0, n))
  
  tplan.sub <- transport_plan(X = post_mu, Y = x[,active.idx,drop=FALSE] %*% theta[active.idx,,drop=FALSE], ground_p = 2, p = 2,
                              observation.orientation = "colwise",
                              method = transport_method)
  natoms.sub <- length(tplan.sub$tplan$to)
  theta_sort.sub <- t(theta[,tplan.sub$tplan$to]) * sqrt(tplan.sub$tplan$mass)
  mu_sort.sub <- post_mu[,tplan.sub$tplan$from] * matrix(sqrt(tplan.sub$tplan$mass), 
                                                         nrow = n, ncol=natoms.sub, byrow=TRUE)
  for(i in 1:n) {
    dat.subset$temp <- theta_sort.sub * matrix(x[i,,drop=FALSE], nrow=natoms.sub, ncol=p, byrow=TRUE)
    dat.subset$sort_y <- mu_sort.sub[i,]
    dat.subset$xtx = dat.subset$xtx + crossprod(dat.subset$temp)/n
    dat.subset$xty = dat.subset$xty + crossprod(dat.subset$temp, dat.subset$sort_y) /n
  }
  
  out.subset <- xtyUpdate(X_ = x, Y_ = post_mu, theta_ = theta, result_=res.sub,
                          OToptions.sel.var)
  
  
  testthat::expect_equal(out.subset, dat.subset$xty)
  
  # check projection gives error
  testthat::expect_error( xtyUpdate(X_ = x, Y_ = post_mu, 
                                    theta_ = theta, result_=res.sub,
                                    OToptions.proj) )
  
  # check nonsense entry for method gives error
  OToptions.sel.var.error <- OToptions.sel.var
  OToptions.sel.var.error$method <- "thumb"
  testthat::expect_error( xtyUpdate(X_ = x, Y_ = post_mu, theta_ = theta, result_=res.sub,
                                    OToptions.sel.var.error) )
  
  #check nonsense transport gives error
  OToptions.sel.var.error2 <- OToptions.sel.var
  OToptions.sel.var.error2$transport.method <- "ideal"
  testthat::expect_error( xtyUpdate(X_ = x, Y_ = post_mu, 
                                    theta_ = theta, result_=res.sub,
                                    OToptions.sel.var.error2) )
  
})

testthat::test_that("test greenkhorn xtyupdate", {
  set.seed(12431)
  
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
  res <- rep(1,p)
  
  
  active.idx <- seq(2,10,2)
  res.sub <- rep(0,p)
  res.sub[active.idx] <-1 
  
  transport_method <- "greenkhorn"
  OToptions.sel.var <- list(same = TRUE,
                            method = "selection.variable",
                            transport.method = transport_method,
                            epsilon = 0.05,
                            niter = 100)
  
  OToptions.proj <- list(same = TRUE,
                         method = "projection",
                         transport.method = transport_method,
                         epsilon = 0.05,
                         niter = 100)
  
  # same mu's
  
  tplan <- transport_plan(X = post_mu, Y = post_mu, ground_p = 2, p = 2, 
                          observation.orientation = "colwise", 
                          method = transport_method, is.X.sorted = TRUE)
  #cost out of sorts because I'm lying to the program about x being sorted
  
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
  
  out <- xtyUpdate(X_ = x, Y_ = post_mu, theta_ = t(theta), result_ = res,
                   OToptions.sel.var)
  
  out_no_trans <- xtyUpdate(X_ = x, Y_ = post_mu, theta_ = theta, result_=res,
                            OToptions.sel.var)
  testthat::expect_equal(out, dat$xty)
  testthat::expect_equal(out_no_trans, dat$xty)
  testthat::expect_equal(out_no_trans, out)
  
  #check to see that suffstat fun gives same answers
  
  out.same <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta), 
                                   OToptions.sel.var)
  
  testthat::expect_lt(norm(out.same$XtY-out, "F"), 7e-8)
  testthat::expect_lt(norm(out.same$XtY - dat$xty, "F"), 7e-8)
  testthat::expect_equal(norm(out.same$XtY-out, "F"), 
                         norm(out.same$XtY - dat$xty, "F"))
  
  # check with different result tab of data
  p_act <- length(active.idx)
  dat.subset <- list(temp=matrix(0, s*s, p), xtx = matrix(0,p,p), xty = rep_len(0, p),
                     sort_y = rep(0, n))
  
  tplan.sub <- transport_plan(X = post_mu, Y = x[,active.idx,drop=FALSE] %*% theta[active.idx,,drop=FALSE], ground_p = 2, p = 2,
                              observation.orientation = "colwise",
                              method = transport_method)
  natoms.sub <- length(tplan.sub$tplan$to)
  theta_sort.sub <- t(theta[,tplan.sub$tplan$to]) * sqrt(tplan.sub$tplan$mass)
  mu_sort.sub <- post_mu[,tplan.sub$tplan$from] * matrix(sqrt(tplan.sub$tplan$mass), 
                                                         nrow = n, ncol=natoms.sub, byrow=TRUE)
  for(i in 1:n) {
    dat.subset$temp <- theta_sort.sub * matrix(x[i,,drop=FALSE], nrow=natoms.sub, ncol=p, byrow=TRUE)
    dat.subset$sort_y <- mu_sort.sub[i,]
    dat.subset$xtx = dat.subset$xtx + crossprod(dat.subset$temp)/n
    dat.subset$xty = dat.subset$xty + crossprod(dat.subset$temp, dat.subset$sort_y) /n
  }
  
  out.subset <- xtyUpdate(X_ = x, Y_ = post_mu, theta_ = theta, result_=res.sub,
                          OToptions.sel.var)
  
  
  testthat::expect_equal(out.subset, dat.subset$xty)
  
  # check projection gives error
  testthat::expect_error( xtyUpdate(X_ = x, Y_ = post_mu, 
                                    theta_ = theta, result_=res.sub,
                                    OToptions.proj) )
  
  # check nonsense entry for method gives error
  OToptions.sel.var.error <- OToptions.sel.var
  OToptions.sel.var.error$method <- "thumb"
  testthat::expect_error( xtyUpdate(X_ = x, Y_ = post_mu, theta_ = theta, result_=res.sub,
                                    OToptions.sel.var.error) )
  
  #check nonsense transport gives error
  OToptions.sel.var.error2 <- OToptions.sel.var
  OToptions.sel.var.error2$transport.method <- "ideal"
  testthat::expect_error( xtyUpdate(X_ = x, Y_ = post_mu, 
                                    theta_ = theta, result_=res.sub,
                                    OToptions.sel.var.error2) )
  
})

# testthat::test_that("test randkhorn xtyupdate", {
#   set.seed(12431)
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
#   post_diff <- matrix(c(y),nrow=n,ncol=s) + matrix(rnorm(s*n,0,0.01),nrow=n,ncol=s)
#   post_vdiff <- matrix(rnorm(n*s),nrow=n,ncol=s)
#   
#   xtx <- crossprod(x)/n 
#   xty <- crossprod(x, post_mu)/n 
#   res <- rep(1,p)
#   
#   
#   active.idx <- seq(2,10,2)
#   res.sub <- rep(0,p)
#   res.sub[active.idx] <-1 
#   
#   transport_method <- "greenkhorn"
#   OToptions.sel.var <- list(same = TRUE,
#                             method = "selection.variable",
#                             transport.method = transport_method,
#                             epsilon = 0.05,
#                             niter = 100)
#   
#   OToptions.proj <- list(same = TRUE,
#                          method = "projection",
#                          transport.method = transport_method,
#                          epsilon = 0.05,
#                          niter = 100)
#   
#   # same mu's
#   
#   tplan <- WpProj:::transport_plan(X = post_mu, Y = post_mu, ground_p = 2, p = 2, 
#                           observation.orientation = "colwise", 
#                           method = transport_method, is.X.sorted = TRUE)
#   #cost out of sorts because I'm lying to the program about x being sorted
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
#   
#   out <- WpProj:::xtyUpdate(X_ = x, Y_ = post_mu, theta_ = t(theta), result_ = res,
#                    OToptions.sel.var)
#   
#   out_no_trans <- WpProj:::xtyUpdate(X_ = x, Y_ = post_mu, theta_ = theta, result_=res,
#                             OToptions.sel.var)
#   testthat::expect_equal(out, dat$xty)
#   testthat::expect_equal(out_no_trans, dat$xty)
#   testthat::expect_equal(out_no_trans, out)
#   
#   #check to see that suffstat fun gives same answers
#   
#   out.same <- WpProj:::sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta), 
#                                    OToptions.sel.var)
#   
#   testthat::expect_lt(norm(out.same$XtY-out, "F"), 7e-8)
#   testthat::expect_lt(norm(out.same$XtY - dat$xty, "F"), 7e-8)
#   testthat::expect_equal(norm(out.same$XtY-out, "F"), 
#                          norm(out.same$XtY - dat$xty, "F"))
#   
#   # check with different result tab of data
#   p_act <- length(active.idx)
#   dat.subset <- list(temp=matrix(0, s*s, p), xtx = matrix(0,p,p), xty = rep_len(0, p),
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
#     dat.subset$temp <- theta_sort.sub * matrix(x[i,,drop=FALSE], nrow=natoms.sub, ncol=p, byrow=TRUE)
#     dat.subset$sort_y <- mu_sort.sub[i,]
#     dat.subset$xtx = dat.subset$xtx + crossprod(dat.subset$temp)/n
#     dat.subset$xty = dat.subset$xty + crossprod(dat.subset$temp, dat.subset$sort_y) /n
#   }
#   
#   out.subset <- xtyUpdate(X_ = x, Y_ = post_mu, theta_ = theta, result_=res.sub,
#                           OToptions.sel.var)
#   
#   
#   testthat::expect_equal(out.subset, dat.subset$xty)
#   
#   # check projection gives error
#   testthat::expect_error( xtyUpdate(X_ = x, Y_ = post_mu, 
#                                     theta_ = theta, result_=res.sub,
#                                     OToptions.proj) )
#   
#   # check nonsense entry for method gives error
#   OToptions.sel.var.error <- OToptions.sel.var
#   OToptions.sel.var.error$method <- "thumb"
#   testthat::expect_error( xtyUpdate(X_ = x, Y_ = post_mu, theta_ = theta, result_=res.sub,
#                                     OToptions.sel.var.error) )
#   
#   #check nonsense transport gives error
#   OToptions.sel.var.error2 <- OToptions.sel.var
#   OToptions.sel.var.error2$transport.method <- "ideal"
#   testthat::expect_error( xtyUpdate(X_ = x, Y_ = post_mu, 
#                                     theta_ = theta, result_=res.sub,
#                                     OToptions.sel.var.error2) )
#   
# })
# 
# testthat::test_that("test gandkhorn xtyupdate", {
#   set.seed(12431)
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
#   post_diff <- matrix(c(y),nrow=n,ncol=s) + matrix(rnorm(s*n,0,0.01),nrow=n,ncol=s)
#   post_vdiff <- matrix(rnorm(n*s),nrow=n,ncol=s)
#   
#   xtx <- crossprod(x)/n 
#   xty <- crossprod(x, post_mu)/n 
#   res <- rep(1,p)
#   
#   
#   active.idx <- seq(2,10,2)
#   res.sub <- rep(0,p)
#   res.sub[active.idx] <-1 
#   
#   transport_method <- "gandkhorn"
#   OToptions.sel.var <- list(same = TRUE,
#                             method = "selection.variable",
#                             transport.method = transport_method,
#                             epsilon = 0.05,
#                             niter = 100)
#   
#   OToptions.proj <- list(same = TRUE,
#                          method = "projection",
#                          transport.method = transport_method,
#                          epsilon = 0.05,
#                          niter = 100)
#   
#   # same mu's
#   
#   tplan <- transport_plan(X = post_mu, Y = post_mu, ground_p = 2, p = 2, 
#                           observation.orientation = "colwise", 
#                           method = transport_method, is.X.sorted = TRUE)
#   #cost out of sorts because I'm lying to the program about x being sorted
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
#   
#   out <- xtyUpdate(X_ = x, Y_ = post_mu, theta_ = t(theta), result_ = res,
#                    OToptions.sel.var)
#   
#   out_no_trans <- xtyUpdate(X_ = x, Y_ = post_mu, theta_ = theta, result_=res,
#                             OToptions.sel.var)
#   testthat::expect_equal(out, dat$xty)
#   testthat::expect_equal(out_no_trans, dat$xty)
#   testthat::expect_equal(out_no_trans, out)
#   
#   #check to see that suffstat fun gives same answers
#   
#   out.same <- sufficientStatistics(X_ = x, Y_ = post_mu, theta_ = t(theta), 
#                                    OToptions.sel.var)
#   
#   testthat::expect_lt(norm(out.same$XtY-out, "F"), 7e-8)
#   testthat::expect_lt(norm(out.same$XtY - dat$xty, "F"), 7e-8)
#   testthat::expect_equal(norm(out.same$XtY-out, "F"), 
#                          norm(out.same$XtY - dat$xty, "F"))
#   
#   # check with different result tab of data
#   p_act <- length(active.idx)
#   dat.subset <- list(temp=matrix(0, s*s, p), xtx = matrix(0,p,p), xty = rep_len(0, p),
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
#     dat.subset$temp <- theta_sort.sub * matrix(x[i,,drop=FALSE], nrow=natoms.sub, ncol=p, byrow=TRUE)
#     dat.subset$sort_y <- mu_sort.sub[i,]
#     dat.subset$xtx = dat.subset$xtx + crossprod(dat.subset$temp)/n
#     dat.subset$xty = dat.subset$xty + crossprod(dat.subset$temp, dat.subset$sort_y) /n
#   }
#   
#   out.subset <- xtyUpdate(X_ = x, Y_ = post_mu, theta_ = theta, result_=res.sub,
#                           OToptions.sel.var)
#   
#   
#   testthat::expect_equal(out.subset, dat.subset$xty)
#   
#   # check projection gives error
#   testthat::expect_error( xtyUpdate(X_ = x, Y_ = post_mu, 
#                                     theta_ = theta, result_=res.sub,
#                                     OToptions.proj) )
#   
#   # check nonsense entry for method gives error
#   OToptions.sel.var.error <- OToptions.sel.var
#   OToptions.sel.var.error$method <- "thumb"
#   testthat::expect_error( xtyUpdate(X_ = x, Y_ = post_mu, theta_ = theta, result_=res.sub,
#                                     OToptions.sel.var.error) )
#   
#   #check nonsense transport gives error
#   OToptions.sel.var.error2 <- OToptions.sel.var
#   OToptions.sel.var.error2$transport.method <- "ideal"
#   testthat::expect_error( xtyUpdate(X_ = x, Y_ = post_mu, 
#                                     theta_ = theta, result_=res.sub,
#                                     OToptions.sel.var.error2) )
#   
# })
