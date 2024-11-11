testthat::test_that("lp_w1 works", {
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
  nlambda <- 2
  lambda.min.ratio <- 1e-10
  gamma <- 1.5
  penalty.factor <- 1/rowMeans(theta^2)
  penalty.factor.null <- rep(1,p)
  post_mu <- x %*% theta
  
  Y <- c(post_mu)
  X <- x
  n <- nrow(X)
  d <- ncol(X)
  s <- ncol(post_mu)
  cols <- lapply(1:s, function(ss) Matrix::sparseMatrix(i = n*(ss-1) + rep(1:n,d), 
                                                        j = rep(1:d,each = n), 
                                                        x = c(x),
                                                        dims = c(n*s, d)))
  Xmat <- do.call(cbind, cols)
  
  temp.deriv <- function(x, lambda, a){lambda}
  
  # debugonce(WpProj:::lp_prob_w1)
  testthat::expect_silent(problem_statement <- WpProj:::lp_prob_w1(Xmat, Y, lambda = rep(1, d), groups = rep(1:d, s)))
  
  # debugonce(WpProj:::lp_norm)
  
  output.cone <- WpProj:::lp_norm(Xmat, Y, power = 1, deriv_func = temp.deriv, 
                                   thresholder = WpProj:::soft_threshold, lambda = 10, 
                                   groups = rep(1:d,s), solver = "cone",
                                   gamma = 1.5, init = NULL, iter = 100, 
                                   tol = 1e-7, opts= list(verbose = 0))
  testthat::expect_equal(output.cone[10], 0)
  
  check_mosek()
  output.mosek <- WpProj:::lp_norm(Xmat, Y, power = 1, deriv_func = temp.deriv, 
                                   thresholder = WpProj:::soft_threshold, lambda = 10, 
                                   groups = rep(1:d,s), solver = "mosek",
                                   gamma = 1.5, init = NULL, iter = 100, tol = 1e-7, opts= list(verbose = 0))
  testthat::expect_equal(output.cone, output.mosek)
  
  check_gurobi()
  output.gurobi <- WpProj:::lp_norm(Xmat, Y, power = 1, deriv_func = temp.deriv, 
                                   thresholder = soft_threshold, lambda = 10, groups = rep(1:d,s), solver = "gurobi",
                                   gamma = 1.5, opts = list(OutputFlag = 0), init = NULL, iter = 100, tol = 1e-7)
  # function(X, Y, deriv_func, thresholder, lambda, groups, solver, gamma = 1.5, opts = NULL, init = NULL, iter = 100, tol = 1e-7)
  # debugonce(WpProj:::lp_norm)
  
  testthat::expect_true(sum((output.gurobi-output.mosek)^2) < 1e-3)
})

testthat::test_that("W1L1 works", {
  
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
  nlambda <- 2
  lambda.min.ratio <- 1e-10
  gamma <- 2.1
  penalty.factor <- 1/rowMeans(theta^2)
  penalty.factor.null <- rep(1,p)
  post_mu <- x %*% theta
  
  
  projection_none <- W1L1(X=x, Y=post_mu, penalty="none", solver = "cone",
                          nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                          maxit = 1e2, gamma = gamma,
                          lambda=lambda)
  
  testthat::expect_equal(c(projection_none$beta), c(theta)) #should be pretty close
  testthat::expect_equal(c(projection_none$beta), c(coef(lm(post_mu ~ x + 0))))#should be pretty close
  testthat::expect_equal(c(theta), c(coef(lm(post_mu ~ x + 0))))#should be pretty close
  
  
  
  projection_mcp <- W1L1(X=x, Y=post_mu, penalty="mcp", solver = "cone",
                         nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                         gamma = gamma, lambda=lambda)
  testthat::expect_equal(c(projection_mcp$beta), c(theta)) #should be pretty close
  testthat::expect_equal(c(projection_mcp$beta), c(theta)) #should be pretty close
  testthat::expect_equal(c(projection_mcp$beta), c(projection_none$beta)) #should be pretty close
  
  # debugonce(W1L1)
  projection_mcp <- W1L1(X=x, Y=post_mu, penalty="mcp", solver = "cone",
                         nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                         gamma = gamma)
  testthat::expect_lte(mean(projection_mcp$beta[,2] - c(theta)), 0.01)
  # testthat::expect_equivalent(c(projection_mcp$beta[,1]), rep(0, p * s))
  testthat::expect_equivalent(c(projection_mcp$power), 1)
  
  projection_scad <-W1L1(X=x, Y=post_mu, penalty="scad", solver = "cone",
                         nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                         gamma = gamma, lambda=lambda)
  testthat::expect_equal(c(projection_scad$beta), c(theta)) #should be pretty close
  
  projection_scad <- W1L1(X=x, Y=post_mu, penalty="scad", solver = "cone",
                          nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                          gamma = gamma)
  testthat::expect_lte(mean(projection_scad$beta[,2] - c(theta)), 0.01)
  # testthat::expect_equivalent(c(projection_scad$beta[,1]), rep(0, p * s))
  testthat::expect_equivalent(c(projection_scad$power), 1)
  
  # debugonce(W1L1)
  projection_lasso <- W1L1(X=x, Y=post_mu, penalty="lasso", solver = "cone",
                          nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                          gamma = gamma, lambda=lambda)
  testthat::expect_equal(c(projection_lasso$beta[,1]), c(theta)) #should be pretty close  
  
  projection_lasso <- W1L1(X=x, Y=post_mu, penalty="lasso", solver = "cone",
                           nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                           gamma = gamma)
  testthat::expect_lte(mean(projection_lasso$beta[,2] - c(theta)), 0.01)
  # testthat::expect_equivalent(c(projection_lasso$beta[,1]), rep(0, p * s))
  testthat::expect_equivalent(c(projection_lasso$power), 1)
  # projection_lasso <- W1L1(X=x, Y=post_mu, penalty="lasso",
  #                          nlambda = 1, lambda.min.ratio = lambda.min.ratio,
  #                          gamma = gamma, alg = "ip")
  
  check_mosek()
  projection_none <- W1L1(X=x, Y=post_mu, penalty="none", solver = "mosek",
                          nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                          maxit = 1e2, gamma = gamma)
  
  testthat::expect_equal(c(projection_none$beta), c(theta)) #should be pretty close
  testthat::expect_equal(c(projection_none$beta), c(coef(lm(post_mu ~ x + 0))))#should be pretty close
  testthat::expect_equal(c(theta), c(coef(lm(post_mu ~ x + 0))))#should be pretty close
  
  testthat::skip_on_cran()
  projection_none_rq <- W1L1(X=x, Y=post_mu, penalty="none", solver = "rqPen",
                             nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                             maxit = 1e2, gamma = gamma,
                             lambda=lambda)
  
  # projectionols <- WPL1(X=x, Y=post_mu, power = 1.0,
  #                       theta=NULL, penalty="ols",
  #                       nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
  #                       infimum.maxit=1, maxit = 1e2, gamma = gamma,
  #                       display.progress = FALSE, lambda=lambda,
  #                       method="projection",
  #                       tol = 0)
  # test0 <- reg_test2(x,post_mu, 1.99, 1000)
  # test1 <- reg_test(x,post_mu, 4, 1000)
  # test2 <- reg_test2(x,post_mu, 1.0, 1000)
  # test3 <- reg_test2(x,post_mu, 2.0, 1000)
  # testthat::expect_equivalent(projection_none$beta, test2$coef)
  
  
  testthat::expect_equal(c(projection_none_rq$beta), c(theta)) #should be pretty close
  testthat::expect_equal(c(projection_none_rq$beta), c(coef(lm(post_mu ~ x + 0))))#should be pretty close
  testthat::expect_equal(c(theta), c(coef(lm(post_mu ~ x + 0))))#should be pretty close
  
  
  
  
})

testthat::test_that("W1L1 works for mosek", {
  check_mosek()
  
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
  nlambda <- 2
  lambda.min.ratio <- 1e-10
  gamma <- 2.1
  penalty.factor <- 1/rowMeans(theta^2)
  penalty.factor.null <- rep(1,p)
  post_mu <- x %*% theta
  
  projection_none <- W1L1(X=x, Y=post_mu, penalty="none",
                          nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                          maxit = 1e2, gamma = gamma,
                          solver = "mosek",
                          lambda=lambda)
  
  testthat::expect_equal(c(projection_none$beta), c(theta)) #should be pretty close
  testthat::expect_equal(c(projection_none$beta), c(coef(lm(post_mu ~ x + 0))))#should be pretty close
  testthat::expect_equal(c(theta), c(coef(lm(post_mu ~ x + 0))))#should be pretty close
  
  projection_mcp <- W1L1(X=x, Y=post_mu, penalty="mcp", solver = "mosek",
                         nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                         gamma = gamma, lambda=lambda)
  testthat::expect_equal(c(projection_mcp$beta), c(theta)) #should be pretty close
  testthat::expect_equal(c(projection_mcp$beta), c(theta)) #should be pretty close
  testthat::expect_equal(c(projection_mcp$beta), c(projection_none$beta)) #should be pretty close
  
  projection_mcp <- W1L1(X=x, Y=post_mu, penalty="mcp",solver = "mosek",
                         nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                         gamma = gamma)
  testthat::expect_lte(mean(projection_mcp$beta[,2] - c(theta)), 0.01)
  testthat::expect_equivalent(c(projection_mcp$beta[,1]), rep(0, p * s))
  testthat::expect_equivalent(c(projection_mcp$power), 1)
  
  projection_scad <-W1L1(X=x, Y=post_mu, penalty="scad", solver = "mosek",
                         nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                         gamma = gamma, lambda=lambda)
  testthat::expect_equal(c(projection_scad$beta[,1]), c(theta)) #should be pretty close
  
  
  
  
  projection_scad <- W1L1(X=x, Y=post_mu, penalty="scad", solver = "mosek",
                          nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                          gamma = gamma)
  testthat::expect_lte(mean(projection_scad$beta[,2] - c(theta)), 0.01)
  testthat::expect_equivalent(c(projection_scad$beta[,1]), rep(0, p * s))
  testthat::expect_equivalent(c(projection_scad$power), 1)
  
  
  
  # debugonce(W1L1)
  projection_lasso <- W1L1(X=x, Y=post_mu, penalty="lasso", solver = "mosek",
                           nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                           gamma = gamma, lambda=lambda)
  testthat::expect_equal(c(projection_lasso$beta[,1]), c(theta)) #should be pretty close  
  
  
 
  
  projection_lasso <- W1L1(X=x, Y=post_mu, penalty="lasso",  solver = "mosek",
                           nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                           gamma = gamma)
  testthat::expect_lte(mean(projection_lasso$beta[,2] - c(theta)), 0.01)
  testthat::expect_equivalent(c(projection_lasso$beta[,1]), rep(0, p * s))
  testthat::expect_equivalent(c(projection_lasso$power), 1)
  # projection_lasso <- W1L1(X=x, Y=post_mu, penalty="lasso",
  #                          nlambda = 1, lambda.min.ratio = lambda.min.ratio,
  #                          gamma = gamma, alg = "ip")
  
  
  
  projection_mcp <- W1L1(X=x, Y=post_mu, penalty="mcp.net", solver = "mosek",
                         nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                         gamma = gamma)
  testthat::expect_equal(projection_mcp$penalty, "mcp") #should be pretty close
  
  
  projection_mcp <- W1L1(X=x, Y=post_mu, penalty="group.mcp",solver = "mosek",
                         nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                         gamma = gamma)
  testthat::expect_equal(projection_mcp$penalty, "mcp") #should be pretty close
  
  
  projection_scad <-W1L1(X=x, Y=post_mu, penalty="scad.net", solver = "mosek",
                         nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                         gamma = gamma)
  testthat::expect_equal(projection_scad$penalty, "scad") #should be pretty close
  
  
  projection_scad <- W1L1(X=x, Y=post_mu, penalty="group.scad", solver = "mosek",
                          nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                          gamma = gamma)
  testthat::expect_equal(projection_scad$penalty, "scad") #should be pretty close
  
  
  # debugonce(W1L1)
  projection_lasso <- W1L1(X=x, Y=post_mu, penalty="elastic.net", solver = "mosek",
                           nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                           gamma = gamma)
  testthat::expect_equal(projection_lasso$penalty, "lasso") #should be pretty close
  
  
  # projection_lasso <- W1L1(X=x, Y=post_mu, penalty="lasso",
  #                          nlambda = 1, lambda.min.ratio = lambda.min.ratio,
  #                          gamma = gamma, alg = "ip")
  
  projection_lasso <- W1L1(X=x, Y=post_mu, penalty="group.lasso",  solver = "mosek",
                           nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                           gamma = gamma)
  testthat::expect_equal(projection_lasso$penalty, "lasso") #should be pretty close
  
  # projection_lasso <- W1L1(X=x, Y=post_mu, penalty="lasso",
  #                          nlambda = 1, lambda.min.ratio = lambda.min.ratio,
  #                          gamma = gamma, alg = "ip")
  
  
})

testthat::test_that("W1L1 works for gurobi", {
  check_gurobi()
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
  nlambda <- 2
  lambda.min.ratio <- 1e-10
  gamma <- 2.1
  penalty.factor <- 1/rowMeans(theta^2)
  penalty.factor.null <- rep(1,p)
  post_mu <- x %*% theta
  
  
  projection_none <- W1L1(X=x, Y=post_mu, penalty="none",
                          nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                          maxit = 1e2, gamma = gamma,
                          solver = "gurobi",
                          lambda=lambda)
  
  # projectionols <- WPL1(X=x, Y=post_mu, power = 1.0,
  #                       theta=NULL, penalty="ols",
  #                       nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
  #                       infimum.maxit=1, maxit = 1e2, gamma = gamma,
  #                       display.progress = FALSE, lambda=lambda,
  #                       method="projection",
  #                       tol = 0)
  # test0 <- reg_test2(x,post_mu, 1.99, 1000)
  # test1 <- reg_test(x,post_mu, 4, 1000)
  # test2 <- reg_test2(x,post_mu, 1.0, 1000)
  # test3 <- reg_test2(x,post_mu, 2.0, 1000)
  # testthat::expect_equivalent(projection_none$beta, test2$coef)
  
  
  testthat::expect_equal(c(projection_none$beta), c(theta)) #should be pretty close
  testthat::expect_equal(c(projection_none$beta), c(coef(lm(post_mu ~ x + 0))))#should be pretty close
  testthat::expect_equal(c(theta), c(coef(lm(post_mu ~ x + 0))))#should be pretty close
  
  projection_mcp <- W1L1(X=x, Y=post_mu, penalty="mcp", solver = "gurobi",
                         nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                         gamma = gamma, lambda=lambda)
  testthat::expect_equal(c(projection_mcp$beta), c(theta)) #should be pretty close
  testthat::expect_equal(c(projection_mcp$beta), c(theta)) #should be pretty close
  testthat::expect_equal(c(projection_mcp$beta), c(projection_none$beta)) #should be pretty close
  
  # debugonce(W1L1)
  projection_mcp <- W1L1(X=x, Y=post_mu, penalty="mcp", solver = "gurobi",
                         nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                         gamma = gamma)
  testthat::expect_lte(mean(projection_mcp$beta[,2] - c(theta)), 0.01)
  testthat::expect_equivalent(c(projection_mcp$beta[,1]), rep(0, p * s))
  testthat::expect_equivalent(c(projection_mcp$power), 1)
  
  projection_scad <-W1L1(X=x, Y=post_mu, penalty="scad", solver = "gurobi",
                         nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                         gamma = gamma, lambda=lambda)
  testthat::expect_equal(c(projection_scad$beta[,1]), c(theta)) #should be pretty close
  
  projection_scad <- W1L1(X=x, Y=post_mu, penalty="scad", solver = "gurobi",
                          nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                          gamma = gamma)
  testthat::expect_lte(mean(projection_scad$beta[,2] - c(theta)), 0.01)
  testthat::expect_equivalent(c(projection_scad$beta[,1]), rep(0, p * s))
  testthat::expect_equivalent(c(projection_scad$power), 1)
  
  # debugonce(W1L1)
  projection_lasso <- W1L1(X=x, Y=post_mu, penalty="lasso", solver = "gurobi",
                           nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                           gamma = gamma, lambda=lambda)
  testthat::expect_equal(c(projection_lasso$beta[,1]), c(theta)) #should be pretty close  
  
  projection_lasso <- W1L1(X=x, Y=post_mu, penalty="lasso", solver = "gurobi",
                           nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                           gamma = gamma)
  testthat::expect_lte(mean(projection_lasso$beta[,2] - c(theta)), 0.01)
  testthat::expect_equivalent(c(projection_lasso$beta[,1]), rep(0, p * s))
  testthat::expect_equivalent(c(projection_lasso$power), 1)
  # projection_lasso <- W1L1(X=x, Y=post_mu, penalty="lasso",
  #                          nlambda = 1, lambda.min.ratio = lambda.min.ratio,
  #                          gamma = gamma, alg = "ip")
  
  
  projection_mcp <- W1L1(X=x, Y=post_mu, penalty="mcp.net", solver = "gurobi",
                         nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                         gamma = gamma)
  testthat::expect_equal(projection_mcp$penalty, "mcp") #should be pretty close
  # debugonce(W1L1)
  projection_mcp <- W1L1(X=x, Y=post_mu, penalty="group.mcp", solver = "gurobi",
                         nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                         gamma = gamma)
  testthat::expect_equal(projection_mcp$penalty, "mcp") #should be pretty close
  
  projection_scad <-W1L1(X=x, Y=post_mu, penalty="scad.net", solver = "gurobi",
                         nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                         gamma = gamma)
  testthat::expect_equal(projection_scad$penalty, "scad") #should be pretty close
  projection_scad <- W1L1(X=x, Y=post_mu, penalty="group.scad", solver = "gurobi",
                          nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                          gamma = gamma)
  testthat::expect_equal(projection_scad$penalty, "scad") #should be pretty close
  
  projection_lasso <- W1L1(X=x, Y=post_mu, penalty="group.lasso", solver = "gurobi",
                           nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                           gamma = gamma)
  testthat::expect_equal(projection_lasso$penalty, "lasso") #should be pretty close
  
  # debugonce(W1L1)
  projection_lasso <- W1L1(X=x, Y=post_mu, penalty="elastic.net", solver = "gurobi",
                           nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                           gamma = gamma,)
  testthat::expect_equal(projection_lasso$penalty, "lasso") #should be pretty close
  
  
}) 
