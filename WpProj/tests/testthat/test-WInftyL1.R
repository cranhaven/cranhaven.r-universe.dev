test_that("WInfL1 lp generates", {
  
  
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
  
  # debugonce(WpProj:::lp_prob_winf)
  testthat::expect_silent(problem_statement <- WpProj:::lp_prob_winf(Xmat, Y, lambda = rep(100, d), groups = rep(1:d, s)))
  mod <- WpProj:::lp_prob_to_model(problem_statement, solver = "cone", rep(0, ncol(Xmat)), NULL)
  testthat::expect_equal(mod$prob$objective$L$v, c(1,rep(100,d)))
  
  testthat::expect_silent(problem_statement <- WpProj:::lp_prob_winf(Xmat, Y, lambda = 1:10, groups = rep(1:d, s)))
  mod <- WpProj:::lp_prob_to_model(problem_statement, solver = "cone", rep(0, ncol(Xmat)), NULL)
  testthat::expect_equal(mod$prob$objective$L$v, c(1,1:10))
  
  # debugonce(WpProj:::lp_norm)
  # debugonce(WpProj:::lp_solve)
  # debugonce(ROI.plugin.ecos:::solve_OP)
  output.cone <- WpProj:::lp_norm(Xmat, Y, power = Inf, deriv_func = temp.deriv, 
                                    thresholder = WpProj:::soft_threshold, lambda = 1, groups = rep(1:d,s), solver = "cone",
                                    gamma = 1.5, opts = NULL, init = NULL, iter = 100, tol = 1e-7)
  
  check_mosek()
  output.mosek <- WpProj:::lp_norm(Xmat, Y, power = Inf, deriv_func = temp.deriv, 
                                   thresholder = WpProj:::soft_threshold, lambda = 1, groups = rep(1:d,s), solver = "mosek",
                                   gamma = 1.5, init = NULL, iter = 100, tol = 1e-7, opts= list(verbose = 0))
  testthat::expect_true(sum((output.cone-output.mosek)^2) < 1e-3)
  
  check_gurobi()
  output.gurobi <- WpProj:::lp_norm(Xmat, Y, power = Inf, deriv_func = temp.deriv, 
                                   thresholder = soft_threshold, lambda = 0, groups = rep(1:d,s), solver = "gurobi",
                    gamma = 1.5, opts = NULL, init = NULL, iter = 100, tol = 1e-7)
  # function(X, Y, deriv_func, thresholder, lambda, groups, solver, gamma = 1.5, opts = NULL, init = NULL, iter = 100, tol = 1e-7)
  # debugonce(WpProj:::lp_norm)
  
  testthat::expect_true(sum((output.gurobi-output.mosek)^2) < 1e-3)
})

testthat::test_that("WInfL1 works", {
  
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
  alpha_s <- 1 + n/2
  beta_s <- 1 + 0.5 * (crossprod(y) + t(mu_post) %*% prec %*% mu_post )
  sigma_post <- 1/stats::rgamma(s, alpha_s, 1/beta_s)
  theta <- sapply(sigma_post, function(ss) mu_post + t(chol(ss * solve(prec))) %*% matrix(stats::rnorm(p, 0, 1),p,1))
  
  lambda <- 0
  nlambda <- 2
  lambda.min.ratio <- 1e-10
  gamma <- 1.5
  penalty.factor <- 1/rowMeans(theta^2)
  penalty.factor.null <- rep(1,p)
  post_mu <- x %*% theta
  
  
  # debugonce(WpProj:::lp_solve)
  # debugonce(WpProj:::lp_prob_to_model)
  projection_none <- WInfL1(X=x, Y=post_mu, penalty="none", solver = "cone",
                          nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                          maxit = 1e2, gamma = gamma,
                          lambda=lambda)
  
  projection_lasso <- WInfL1(X=x, Y=post_mu, penalty="lasso", solver = "cone",
                             nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                             maxit = 1e2, gamma = gamma,
                             lambda=lambda)
  
  testthat::expect_equivalent(projection_none$beta, projection_lasso$beta)
  testthat::expect_equal(c(projection_none$beta), c(theta)) #should be pretty close
  testthat::expect_equal(c(projection_none$beta), c(coef(lm(post_mu ~ x + 0))))#should be pretty close
  testthat::expect_equal(c(theta), c(coef(lm(post_mu ~ x + 0))))#should be pretty close
  
  # debugonce(WInfL1) 
  
  projection_mcp <- WInfL1(X=x, Y=post_mu, penalty="mcp",
                           nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                           gamma = gamma, lambda=lambda)
  testthat::expect_equal(c(projection_mcp$beta), c(theta)) #should be pretty close
  testthat::expect_equal(c(projection_mcp$beta), c(projection_none$beta)) #should be pretty close
  
  
  projection_scad <-WInfL1(X=x, Y=post_mu, penalty="scad",
                           nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                           gamma = gamma, lambda=lambda)
  testthat::expect_equal(c(projection_scad$beta[,1]), c(theta)) #should be pretty close
  
  projection_scad <- WInfL1(X=x, Y=post_mu, penalty="scad",
                            nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                            gamma = gamma)
  testthat::expect_equal(c(projection_scad$beta[,2]), c(theta)) #should be pretty close
  
  projection_lasso <- WInfL1(X=x, Y=post_mu, penalty="lasso",
                             nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                             gamma = gamma)
  testthat::expect_equal(c(projection_lasso$beta[,2]), c(theta)) #should be pretty close  
  
  # compare with Rmosek
  if(rlang::is_installed("Rmosek")) {
    
    WpProj:::check_mosek()
    
    projection_none_mosek <- WInfL1(X=x, Y=post_mu, penalty="none", solver = "mosek",
                              nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                              maxit = 1e2, gamma = gamma,
                              lambda=lambda)
    projection_lasso_mosek <- WInfL1(X=x, Y=post_mu, penalty="lasso", solver = "mosek",
                              nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                              maxit = 1e2, gamma = gamma,
                              lambda=lambda)
    projection_lasso_mosek2 <- WInfL1(X=x, Y=post_mu, penalty="lasso", solver = "mosek",
                                     nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                                     maxit = 1e2, gamma = gamma)
    
    testthat::expect_equivalent(projection_none_mosek$beta, projection_lasso_mosek$beta)
    testthat::expect_equivalent(projection_none_mosek$beta, projection_none$beta)
    testthat::expect_equivalent(projection_lasso_mosek2$beta[,2], projection_none$beta)
  } else {
    check_mosek()
  }
  
  
})

testthat::test_that("WInfL1 changes penalty appropriately for net penalties", {
  
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
  
  projection_mcp <- WInfL1(X=x, Y=post_mu, penalty="mcp.net", solver = "cone",
                           nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                           gamma = gamma)
  testthat::expect_equal(projection_mcp$penalty, "mcp") #should be pretty close
  
  # debugonce(WInfL1)
  projection_mcp <- WInfL1(X=x, Y=post_mu, penalty="group.mcp", solver = "cone",
                           nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                           gamma = gamma)
  testthat::expect_equal(projection_mcp$penalty, "mcp") #should be pretty close
  
  projection_scad <-WInfL1(X=x, Y=post_mu, penalty="scad.net", solver = "cone",
                           nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                           gamma = gamma)
  testthat::expect_equal(projection_scad$penalty, "scad") #should be pretty close
  
  projection_scad <- WInfL1(X=x, Y=post_mu, penalty="group.scad", solver = "cone",
                            nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                            gamma = gamma)
  testthat::expect_equal(projection_scad$penalty, "scad") #should be pretty close
  
  # debugonce(WInfL1)
  projection_lasso <- WInfL1(X=x, Y=post_mu, penalty="elastic.net", solver = "cone",
                             nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                             gamma = gamma,)
  testthat::expect_equal(projection_lasso$penalty, "lasso") #should be pretty close
  
  projection_lasso <- WInfL1(X=x, Y=post_mu, penalty="group.lasso", solver = "cone",
                             nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                             gamma = gamma)
  
  if(rlang::is_installed("gurobi")) {
    
    check_gurobi()
    projection_mcp <- WInfL1(X=x, Y=post_mu, penalty="mcp.net", solver = "gurobi",
                             nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                             gamma = gamma)
    testthat::expect_equal(projection_mcp$penalty, "mcp") #should be pretty close
    
    # debugonce(WInfL1)
    projection_mcp <- WInfL1(X=x, Y=post_mu, penalty="group.mcp", solver = "gurobi",
                             nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                             gamma = gamma)
    testthat::expect_equal(projection_mcp$penalty, "mcp") #should be pretty close
    
    projection_scad <-WInfL1(X=x, Y=post_mu, penalty="scad.net", solver = "gurobi",
                             nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                             gamma = gamma)
    testthat::expect_equal(projection_scad$penalty, "scad") #should be pretty close
    
    projection_scad <- WInfL1(X=x, Y=post_mu, penalty="group.scad", solver = "gurobi",
                              nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                              gamma = gamma)
    testthat::expect_equal(projection_scad$penalty, "scad") #should be pretty close
    
    # debugonce(WInfL1)
    projection_lasso <- WInfL1(X=x, Y=post_mu, penalty="elastic.net", solver = "gurobi",
                               nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                               gamma = gamma,)
    testthat::expect_equal(projection_lasso$penalty, "lasso") #should be pretty close
    
    projection_lasso <- WInfL1(X=x, Y=post_mu, penalty="group.lasso", solver = "gurobi",
                               nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                               gamma = gamma)
  }
  
  if(rlang::is_installed("Rmosek")) {
    check_mosek()
    projection_mcp <- WInfL1(X=x, Y=post_mu, penalty="mcp.net", solver = "mosek",
                             nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                             gamma = gamma)
    testthat::expect_equal(projection_mcp$penalty, "mcp") #should be pretty close
    
    
    projection_mcp <- WInfL1(X=x, Y=post_mu, penalty="group.mcp",solver = "mosek",
                             nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                             gamma = gamma)
    testthat::expect_equal(projection_mcp$penalty, "mcp") #should be pretty close
    
    
    testthat::expect_equal(projection_lasso$penalty, "lasso") #should be pretty close
    
    projection_scad <-WInfL1(X=x, Y=post_mu, penalty="scad.net", solver = "mosek",
                             nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                             gamma = gamma)
    testthat::expect_equal(projection_scad$penalty, "scad") #should be pretty close
    
    
    projection_scad <- WInfL1(X=x, Y=post_mu, penalty="group.scad", solver = "mosek",
                              nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                              gamma = gamma)
    testthat::expect_equal(projection_scad$penalty, "scad") #should be pretty close
    
    
    # debugonce(WInfL1)
    projection_lasso <- WInfL1(X=x, Y=post_mu, penalty="elastic.net", solver = "mosek",
                               nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                               gamma = gamma)
    testthat::expect_equal(projection_lasso$penalty, "lasso") #should be pretty close
    
    
    
    # projection_lasso <- WInfL1(X=x, Y=post_mu, penalty="lasso",
    #                          nlambda = 1, lambda.min.ratio = lambda.min.ratio,
    #                          gamma = gamma, alg = "ip")
    
    projection_lasso <- WInfL1(X=x, Y=post_mu, penalty="group.lasso",  solver = "mosek",
                               nlambda = nlambda, lambda.min.ratio = lambda.min.ratio,
                               gamma = gamma)
    testthat::expect_equal(projection_lasso$penalty, "lasso") #should be pretty close
    
    # projection_lasso <- W1L1(X=x, Y=post_mu, penalty="lasso",
    #                          nlambda = 1, lambda.min.ratio = lambda.min.ratio,
    #                          gamma = gamma, alg = "ip")
  }
  
  check_gurobi()
  check_mosek()
  
})

