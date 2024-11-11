test_that("stepwise works", {
  require(WpProj)
  set.seed(111)
  
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
                 method = "selection.variable",
                 transport.method = "exact",
                 epsilon = 0.05,
                 niter = 100)
  
  suffStat_star <- sufficientStatistics(x, post_mu, t(post_beta), OTopts)
  xtx_star <- suffStat_star$XtX #* wt + diag(post_beta_norm) * (1-wt)
  xty_star <- suffStat_star$XtY #* wt + post_beta_norm * (1-wt)
  
  active.idx <- seq(2,10,2)
  
  out <- WPSW(x, post_mu, t(post_beta), force = 1, power = 2,
              direction = c("backward"), 
              method=c("selection.variable"))
  out1 <- WPSW(x, post_mu, t(post_beta), force = 1, power = 2,
               direction = c("forward"), 
               method=c("selection.variable"))
  
  testthat::expect_true(all(out$beta %in% c(0,1)))
  testthat::expect_true(all(out1$beta %in% c(0,1)))
  
  #check model size truncation
  out <- WPSW(x, t(post_mu), t(post_beta), force = 1, power = 2,
              direction = c("backward"), 
              method=c("selection.variable"), model.size = 8)
  out1 <- WPSW(x, t(post_mu), t(post_beta), force = 1, power = 2,
               direction = c("forward"), 
               method=c("selection.variable"), model.size = 3)
  
  testthat::expect_true(all(which(apply(out$beta, 2, function(i) all(!is.na(i)))) %in% 1:3))
  testthat::expect_true(all(which(apply(out1$beta, 2, function(i) all(!is.na(i)))) %in% c(1,2,3)))
  testthat::expect_true(all(out$beta %in% c(NA,0,1)))
  testthat::expect_true(all(out1$beta %in% c(NA, 0,1)))
  
  #check projection
  out2 <- WPSW(x, t(post_mu), t(post_beta), force = 1, power = 2,
               direction = c("backward"), 
               method=c("projection"))
  out3 <- WPSW(x, t(post_mu), t(post_beta), force = 1, power = 2,
               direction = c("forward"), 
               method=c("projection"))
  testthat::expect_true(all((colSums(out2$beta!=0)/s) %in% 1:10))
  testthat::expect_true(all((colSums(out3$beta!=0)/s) %in% 1:10))
  
  #check modelsize truncation
  out4 <- WPSW(x, t(post_mu), t(post_beta), force = 1, power = 2,
               direction = c("backward"), 
               method=c("projection"), model.size = 8)
  out5 <- WPSW(x, t(post_mu), t(post_beta), force = 1, power = 2,
               direction = c("forward"), 
               method=c("projection"), model.size = 3)

  testthat::expect_true(all(which(apply(out4$beta, 2, function(i) all(!is.na(i)))) %in% 1:3))
  testthat::expect_true(all(which(apply(out5$beta, 2, function(i) all(!is.na(i)))) %in% 1:3))
})

testthat::test_that("model size works when force is null", {
  require(WpProj)
  set.seed(1234807)
  
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
                 method = "selection.variable",
                 transport.method = "exact",
                 epsilon = 0.05,
                 niter = 100)
  
  suffStat_star <- sufficientStatistics(x, post_mu, t(post_beta), OTopts)
  xtx_star <- suffStat_star$XtX #* wt + diag(post_beta_norm) * (1-wt)
  xty_star <- suffStat_star$XtY #* wt + post_beta_norm * (1-wt)
  
  active.idx <- seq(2,10,2)
  
  testthat::expect_invisible( out <- WPSW(x, t(post_mu), t(post_beta), power = 2,
               direction = c("backward"), 
               method=c("projection"), model.size = 8))
  testthat::expect_invisible(out1 <- WPSW(x, t(post_mu), t(post_beta), power = 2,
               direction = c("forward"), 
               method=c("projection"), model.size = 3))
  testthat::expect_invisible( out2 <- WPSW(x, t(post_mu), t(post_beta), power = 2,
                                           direction = c("backward"), 
                                           method=c("projection")))
  
})
  
testthat::test_that("nzero gives right number", {
  require(WpProj)
  set.seed(9786)
  
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
                 method = "selection.variable",
                 transport.method = "exact",
                 epsilon = 0.05,
                 niter = 100)
  
  suffStat_star <- sufficientStatistics(x, post_mu, t(post_beta), OTopts)
  xtx_star <- suffStat_star$XtX #* wt + diag(post_beta_norm) * (1-wt)
  xty_star <- suffStat_star$XtY #* wt + post_beta_norm * (1-wt)
  
  active.idx <- seq(2,10,2)
  
  out <- WPSW(x, post_mu, t(post_beta), force = 1, power = 2,
              direction = c("backward"), 
              method=c("selection.variable"))
  out1 <- WPSW(x, post_mu, t(post_beta), force = 1, power = 2,
               direction = c("forward"), 
               method=c("selection.variable"))
  
  testthat::expect_true(all(out$nzero %in% 1:p))
  testthat::expect_true(all(out1$nzero %in% 1:p))
  
  #check model size truncation
  out <- WPSW(x, t(post_mu), t(post_beta), force = 1, power = 2,
              direction = c("backward"), 
              method=c("selection.variable"), model.size = 8)
  out1 <- WPSW(x, t(post_mu), t(post_beta), force = 1, power = 2,
               direction = c("forward"), 
               method=c("selection.variable"), model.size = 3)
  
  testthat::expect_true(all(out$nzero %in% 8:p))
  testthat::expect_true(all(out1$nzero %in% 1:3))
  testthat::expect_true(all(out$beta %in% c(NA,0,1)))
  testthat::expect_true(all(out1$beta %in% c(NA, 0,1)))
  
  #check projection
  out2 <- WPSW(x, t(post_mu), t(post_beta), force = 1, power = 2,
               direction = c("backward"), 
               method=c("projection"))
  out3 <- WPSW(x, t(post_mu), t(post_beta), force = 1, power = 2,
               direction = c("forward"), 
               method=c("projection"))
  testthat::expect_true(all(out2$nzero %in% 1:p))
  testthat::expect_true(all(out3$nzero %in% 1:p))
  testthat::expect_true(all((colSums(out2$beta!=0)/s) %in% 1:p))
  testthat::expect_true(all((colSums(out3$beta!=0)/s) %in% 1:p))
  
  #check modelsize truncation
  out4 <- WPSW(x, t(post_mu), t(post_beta), force = 1, power = 2,
               direction = c("backward"), 
               method=c("projection"), model.size = 8)
  out5 <- WPSW(x, t(post_mu), t(post_beta), force = 1, power = 2,
               direction = c("forward"), 
               method=c("projection"), model.size = 3)
  testthat::expect_true(all(out4$nzero %in% 1:p))
  testthat::expect_true(all(out5$nzero %in% 1:p))
  testthat::expect_true(all((colSums(out4$beta!=0)/s) %in% 1:p))
  testthat::expect_true(all((colSums(out5$beta!=0)/s) %in% 1:p))
})
