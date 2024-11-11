test_that("WPL1 refers to W2L1 appropriately", {
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
  # print(x)
  projectionols <- WpProj:::WPL1(X=x, Y=NULL, power = 2.0,
                        theta=theta, penalty="ols",
                        nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                        infimum.maxit=1, maxit = 1e3, gamma = gamma,
                        display.progress = FALSE, lambda=lambda,
                        penalty.factor = penalty.factor.null, method="projection",
                        tol = 0)
  testthat::expect_equal(c(projectionols$beta), c(theta)) #should be pretty close
  testthat::expect_equal(c(projectionols$beta), c(coef(lm(post_mu ~ x + 0))))#should be pretty close
  testthat::expect_equal(c(theta), c(coef(lm(post_mu ~ x + 0))))#should be pretty close

  projectionmcp <- WpProj:::WPL1(X=x, Y=NULL, power = 2.0,
                        theta=theta, penalty="mcp",
                        nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                        infimum.maxit=1, maxit = 1e3, gamma = gamma,
                        display.progress = FALSE, lambda=lambda,
                        penalty.factor = penalty.factor.null, method="projection",
                        tol = 0)
  testthat::expect_equal(c(projectionmcp$beta[,2]), c(theta)) #should be pretty close
  testthat::expect_equal(c(projectionmcp$beta[,1]), c(theta)) #should be pretty close
  testthat::expect_equal(c(projectionmcp$beta[,1]), c(projectionols$beta)) #should be pretty close


  projectionscad <- WpProj:::WPL1(X=x, Y=NULL, power = 2.0,
                         theta=theta, penalty="scad",
                         nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                         infimum.maxit=1, maxit = 1e3, gamma = gamma,
                         display.progress = FALSE, lambda=lambda,
                         penalty.factor = penalty.factor, method="projection",
                         tol = 0)
  testthat::expect_equal(c(projectionscad$beta[,2]), c(theta)) #should be pretty close
  testthat::expect_equal(c(projectionscad$beta[,1]), c(theta)) #should be pretty close


  projectionlasso <- WpProj:::WPL1(X=x, Y=NULL, power = 2.0,
                          theta=theta, penalty="lasso",
                          nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                          infimum.maxit=1, maxit = 1e3, gamma = gamma,
                          display.progress = FALSE, lambda = lambda,
                          penalty.factor = penalty.factor, method="projection",
                          tol= 0)
  testthat::expect_equal(c(projectionlasso$beta[,2]), c(theta)) #should be pretty close
  testthat::expect_equal(c(projectionlasso$beta[,1]), c(theta)) #should be pretty close

  projectionlasso <- WpProj:::WPL1(X=x, Y=NULL, power = 2.0,
                          theta=theta, penalty="lasso",
                          nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                          infimum.maxit=1, maxit = 1e3, gamma = gamma,
                          display.progress = FALSE,
                          penalty.factor = penalty.factor, method="projection",
                          tol = 0)
  testthat::expect_equal(c(projectionlasso$beta[,101]), c(theta)) #should be pretty close

  
  testthat::skip_on_cran()
  #should warn about infimum
  if(Sys.info()['machine'] == "arm64") {
    testthat::expect_silent(WpProj:::WPL1(X=x, Y=NULL, power = 2.0,
                                          theta=theta, penalty="lasso",
                                          nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                                          infimum.maxit=1, maxit = 1, gamma = gamma,
                                          display.progress = FALSE,
                                          penalty.factor = penalty.factor, method="projection"))
  } else {
    testthat::expect_warning(WpProj:::WPL1(X=x, Y=NULL, power = 2.0,
                                          theta=theta, penalty="lasso",
                                          nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                                          infimum.maxit=1, maxit = 1, gamma = gamma,
                                          display.progress = FALSE,
                                          penalty.factor = penalty.factor, method="projection"))
  }
  
})

testthat::test_that("WPL1 works for Wp", {
  set.seed(87897)
  
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
  
  lambda <- 0
  nlambda <- 3
  lmr <- 1e-10
  gamma <- 1.5
  penalty.factor <- 1/rowMeans(theta^2)
  penalty.factor.null <- rep(1,p)
  post_mu <- x %*% theta
  # debugonce(W1L1)
  # projectionmcp <- W1L1(X=x, Y=post_mu, penalty="mcp",
  #                       nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
  #                       maxit = 1e2, gamma = gamma,
  #                       lambda=NULL)
  testthat::expect_silent(projectionmcp <- WpProj:::WPL1(X=x, Y=post_mu, penalty="mcp", p = 3,
                                                nlambda = nlambda, lambda.min.ratio = lmr,
                                                maxit = 5, gamma = gamma,
                                                lambda=NULL))
  testthat::expect_lte(mean(c(projectionmcp$beta[,3]) - c(theta)), 1e-7)
  
  testthat::skip_on_cran()
  
  testthat::expect_silent(projectionols <- 
                            WpProj:::WPL1(X=x, Y=post_mu, power = 3.0,
                                          maxit = 5,
                                 theta=NULL, penalty="ols", lambda.min.ratio = lmr,
                                 lambda=lambda))
  
  testthat::expect_lte(mean(c(projectionols$beta)-c(theta)), 1e-7) #should be pretty close
  
  testthat::expect_silent(projectionscad <- 
                            WpProj:::WPL1(X=x, Y=NULL, power = 3,
                                 theta=theta, penalty="scad",
                                 maxit = 5,
                                 nlambda = nlambda, lambda.min.ratio = lmr,
                                 lambda=NULL))
  testthat::expect_lte(mean(c(projectionscad$beta[,3])-c(theta)), 1e-7) #should be pretty close
  
  testthat::expect_silent(projectionlasso <-  WpProj:::WPL1(X=x, Y=NULL, power = 3,
                                                  theta=theta, penalty="lasso",
                                                  maxit = 5,
                                                  nlambda = nlambda, lambda = NULL))
  testthat::expect_lte(mean(c(projectionlasso$beta[,3])-c(theta)), 1e-5) #should be pretty close
  
  
  
})

testthat::test_that("WPL1 works for W1", {
  
  set.seed(87897)

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

  lambda <- 0
  nlambda <- 3
  lambda.min.ratio <- 1e-10
  gamma <- 1.5
  penalty.factor <- 1/rowMeans(theta^2)
  penalty.factor.null <- rep(1,p)
  post_mu <- x %*% theta
  # debugonce(W1L1)
  # projectionmcp <- W1L1(X=x, Y=post_mu, penalty="mcp",
  #                       nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
  #                       maxit = 1e2, gamma = gamma,
  #                       lambda=NULL)
  
  testthat::expect_silent(projectionmcp1 <- WpProj:::W1L1(X=x, Y=post_mu, penalty="mcp",
                                  nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                                  maxit = 5, gamma = gamma,
                                  lambda=NULL))
  testthat::expect_silent(projectionmcp2 <- WpProj:::WPL1(X=x, Y=post_mu, penalty="mcp", p = 1,
                                  nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                                  maxit = 5, gamma = gamma,
                                  lambda=NULL))
  
  testthat::skip_on_cran()
  
  projectionmcp1 <- WpProj:::W1L1(X=x, Y=post_mu, penalty="mcp",
                         nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                         maxit = 5, gamma = gamma,
                         lambda=NULL)
  projectionmcp2 <- WpProj:::WPL1(X=x, Y=post_mu, penalty="mcp", p = 1,
                        nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                        maxit = 5, gamma = gamma,
                        lambda=NULL)
  # testthat::expect_equal(c(projectionmcp1$beta[,3]), c(projectionmcp2$beta[,3]))#should be pretty close
  
  projectionols1 <- WpProj:::W1L1(X=x, Y=post_mu, penalty="ols",
                                nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                                maxit = 5, gamma = gamma,
                                lambda=lambda)

  projectionols2 <- WpProj:::WPL1(X=x, Y=post_mu, power = 1.0, maxit = 5,
                        theta=NULL, penalty="ols",
                        lambda=lambda)

  # testthat::expect_equivalent(c(projectionols1$beta), c(theta)) #should be pretty close
  # testthat::expect_equal(c(projectionols1$beta), c(projectionols2$beta))#should be pretty close


  projectionscad1 <- WpProj:::W1L1(X=x, Y=NULL,
                         theta=theta, penalty="scad",
                         nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                         lambda=NULL)
  projectionscad2 <- WpProj:::WPL1(X=x, Y=NULL, power = 1.0,
                         theta=theta, penalty="scad", maxit = 5,
                         nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                         lambda=NULL)
  # testthat::expect_equivalent(c(projectionscad1$beta[,1]), c(projectionscad2$beta[,1])) #should be pretty close


  projectionlasso1 <- WpProj:::W1L1(X=x, Y=NULL, power = 1.0,
                          theta=theta, penalty="lasso",
                          nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                          gamma = gamma,
                          lambda = NULL)
  projectionlasso2 <- WpProj:::WPL1(X=x, Y=NULL, power = 1.0, maxit = 5,
                           theta=theta, penalty="lasso",
                           nlambda = nlambda, lambda = NULL)
  # testthat::expect_equal(c(projectionlasso1$beta[,1]), c(projectionlasso2$beta[,1])) #should be pretty close

})

testthat::test_that("WPL1 works for WInf", {
  
  set.seed(87897)
  
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
  
  lambda <- 0
  nlambda <- 3
  lambda.min.ratio <- 1e-10
  gamma <- 1.5
  penalty.factor <- 1/rowMeans(theta^2)
  penalty.factor.null <- rep(1,p)
  post_mu <- x %*% theta
  # debugonce(W1L1)
  # projectionmcp <- W1L1(X=x, Y=post_mu, penalty="mcp",
  #                       nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
  #                       maxit = 1e2, gamma = gamma,
  #                       lambda=NULL)
  testthat::expect_silent(projectionmcp1 <- WpProj:::WInfL1(X=x, Y=post_mu, penalty="mcp",
                         nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                         maxit = 5, gamma = gamma,
                         lambda=NULL))
  testthat::expect_silent(projectionmcp2 <- WpProj:::WPL1(X=x, Y=post_mu, penalty="mcp", power = Inf,
                         nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                         maxit = 5, gamma = gamma,
                         lambda=NULL))
  # testthat::expect_equal(c(projectionmcp1$beta[,3]), c(projectionmcp2$beta[,3]))#should be pretty close
  
  # testthat::skip_on_cran()
  # projectionols1 <- WInfL1(X=x, Y=post_mu, penalty="ols",
  #                        nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
  #                        maxit = 1e2, gamma = gamma,
  #                        lambda=lambda)
  # 
  # projectionols2 <- WPL1(X=x, Y=post_mu, power = Inf,
  #                        theta=NULL, penalty="ols",
  #                        lambda=lambda)
  # 
  # testthat::expect_equivalent(c(projectionols1$beta), c(theta)) #should be pretty close
  # testthat::expect_equal(c(projectionols1$beta), c(projectionols2$beta))#should be pretty close
  # 
  # 
  # projectionscad1 <- WInfL1(X=x, Y=NULL,
  #                         theta=theta, penalty="scad",
  #                         nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
  #                         lambda=NULL)
  # projectionscad2 <- WPL1(X=x, Y=NULL, power = Inf,
  #                         theta=theta, penalty="scad",
  #                         nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
  #                         lambda=NULL)
  # testthat::expect_equivalent(c(projectionscad1$beta[,2]), c(projectionscad2$beta[,2])) #should be pretty close
  # 
  # 
  # projectionlasso1 <- WInfL1(X=x, Y=NULL,
  #                          theta=theta, penalty="lasso",
  #                          nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
  #                          gamma = gamma,
  #                          lambda = NULL)
  # projectionlasso2 <- WPL1(X=x, Y=NULL, power = Inf,
  #                          theta=theta, penalty="lasso", lambda.min.ratio = lambda.min.ratio,
  #                          nlambda = nlambda, lambda = NULL)
  # testthat::expect_equal(c(projectionlasso1$beta[,2]), c(projectionlasso2$beta[,2])) #should be pretty close
  
})

