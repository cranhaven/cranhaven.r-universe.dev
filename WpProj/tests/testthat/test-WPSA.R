test_that("WPSA code works for covar", {
  set.seed(111)
  
  ##### Testing R Functions ####
  n <- 128
  p <- 10
  s <- 99
  
  x <- matrix( rnorm( p * n ), nrow = n, ncol = p )
  x_ <- t(x)
  beta <- (1:10)/10
  y <- x %*% beta + rnorm(n)
  post_beta <- matrix(beta, nrow=p, ncol=s) + rnorm(p*s, 0, 0.1)
  post_mu <- x %*% post_beta
  
  xtx <- crossprod(x)/n #* wt + diag(1,p,p) * (1 - wt)
  xty <- crossprod(x, post_mu)/n #* wt + post_beta * (1 - wt)
  
  transport.method <- "exact"
  prop.meth <- "covariance"
  
  otopt <- list(
    same = TRUE,
    method = "selection.variable",
    transport.method = transport.method,
    epsilon = 0.05,
    niter = 100
  )
  
  suffStat_star <- sufficientStatistics(x, post_mu, t(post_beta), otopt)
  xtx_star <- suffStat_star$XtX #* wt + diag(post_beta_norm) * (1-wt)
  xty_star <- suffStat_star$XtY #* wt + post_beta_norm * (1-wt)
  
  sv <-  WPSA(X=x, Y=t(post_mu), t(post_beta), 
              force = 1, power = 2, nvars = 2:(p-1),
              # groups = NULL,
              maxit=5, temps = 5,
              max.time = 100,
              options = list(method = c("selection.variable"),
                             energy.distribution = "boltzman",
                             transport.method = "exact",
                             cooling.schedule = "Geman-Geman",
                             proposal.method = prop.meth)
  )
  testthat::expect_equal(sv$optimal[[2]]$index, c(1,9,10))
  
  testthat::skip_on_cran()
  
  sv1 <-  WpProj:::WPSA(X=x, Y=t(post_mu), t(post_beta), 
               force = 1, power = 2, nvar = 3,
               # groups = NULL,
               maxit=10, temps = 10,
               max.time = 30,
               proposal = WpProj:::proposal.fun,
               options = list(method = c("selection.variable"),
                              energy.distribution = "boltzman",
                              transport.method = "univariate.approximation.pwr",
                              cooling.schedule = "Geman-Geman",
                              proposal.method = prop.meth)
  )
  testthat::expect_equal(sv1$optimal$index, c(1,9,10))
  
  sv2 <-  WPSA(X=x, Y=t(post_mu), t(post_beta), 
               force = 1, power = 2, nvar = 3,
               # groups = NULL,
               maxit=10, temps = 10,
               max.time = 30,
               proposal = proposal.fun,
               options = list(method = c("selection.variable"),
                              energy.distribution = "boltzman",
                              transport.method = "hilbert",
                              cooling.schedule = "Geman-Geman",
                              proposal.method = prop.meth)
  )
  testthat::expect_equal(sv2$optimal$index, c(1,9,10))
  
  sv3 <-  WPSA(X=x, Y=t(post_mu), t(post_beta), 
               force = 1, power = 2, nvar = 3,
               # groups = NULL,
               maxit=10, temps = 10,
               max.time = 30,
               proposal = proposal.fun,
               options = list(method = c("selection.variable"),
                              energy.distribution = "boltzman",
                              transport.method = "rank",
                              cooling.schedule = "Geman-Geman",
                              proposal.method = prop.meth)
  )
  testthat::expect_equal(sv3$optimal$index, c(1,9,10))
  
  sv4 <-  WPSA(X=x, Y=t(post_mu), t(post_beta), 
               force = 1, power = 2, nvar = 3,
               # groups = NULL,
               maxit=10, temps = 10,
               max.time = 30,
               proposal = proposal.fun,
               options = list(method = c("selection.variable"),
                              energy.distribution = "boltzman",
                              transport.method = "sinkhorn",
                              cooling.schedule = "Geman-Geman",
                              proposal.method = prop.meth)
  )
  testthat::expect_equal(sv4$optimal$index, c(1,9,10))
  
  sv5 <-  WPSA(X=x, Y=t(post_mu), t(post_beta), 
               force = 1, power = 2, nvar = 3,
               # groups = NULL,
               maxit=10, temps = 10,
               max.time = 30,
               proposal = proposal.fun,
               options = list(method = c("selection.variable"),
                              energy.distribution = "boltzman",
                              transport.method = "greenkhorn",
                              cooling.schedule = "Geman-Geman",
                              proposal.method = prop.meth)
  )
  testthat::expect_equal(sv5$optimal$index, c(1,9,10))
  # no randkhorn
  # sv6 <-  WPSA(X=x, Y=t(post_mu), t(post_beta), 
  #              force = 1, power = 2, nvar = 3,
  #              # groups = NULL,
  #              maxit=10, temps = 10,
  #              max.time = 30,
  #              proposal = proposal.fun,
  #              options = list(method = c("selection.variable"),
  #                             energy.distribution = "boltzman",
  #                             transport.method = "randkhorn",
  #                             cooling.schedule = "Geman-Geman",
  #                             proposal.method = prop.meth)
  # )
  # testthat::expect_equal(sv6$optimal$index, c(1,9,10))
  
  # sv7 <-  WPSA(X=x, Y=t(post_mu), t(post_beta), 
  #              force = 1, power = 2, nvar = 3,
  #              # groups = NULL,
  #              maxit=10, temps = 10,
  #              max.time = 30,
  #              proposal = proposal.fun,
  #              options = list(method = c("selection.variable"),
  #                             energy.distribution = "boltzman",
  #                             transport.method = "gandkhorn",
  #                             cooling.schedule = "Geman-Geman",
  #                             proposal.method = prop.meth)
  # )
  # testthat::expect_equal(sv7$optimal$index, c(1,9,10))
})

testthat::test_that("WPSA code works for random", {
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
  
  transport.method <- "exact"
  prop.meth <- "random"
  
  otopt <- list(
    same = TRUE,
    method = "selection.variable",
    transport.method = transport.method,
    epsilon = 0.05,
    niter = 100
  )
  
  suffStat_star <- WpProj:::sufficientStatistics(x, post_mu, t(post_beta), otopt)
  xtx_star <- suffStat_star$XtX #* wt + diag(post_beta_norm) * (1-wt)
  xty_star <- suffStat_star$XtY #* wt + post_beta_norm * (1-wt)
  
  testthat::expect_silent(sv <-  WpProj:::WPSA(X=x, Y=t(post_mu), t(post_beta), 
              force = 1, power = 2, nvar = 3,
              # groups = NULL,
              maxit=5, temps = 5,
              max.time = 10,
              proposal = WpProj:::proposal.fun,
              options = list(method = c("selection.variable"),
                             energy.distribution = "boltzman",
                             transport.method = "exact",
                             cooling.schedule = "Geman-Geman",
                             proposal.method = prop.meth)
  ))
  testthat::skip_on_cran()
  testthat::expect_true(all(sv$optimal$index %in% c(1,8,9,10)))
  
  sv1 <-  WPSA(X=x, Y=t(post_mu), t(post_beta), 
              force = 1, power = 2, nvar = 3,
              # groups = NULL,
              maxit=10, temps = 10,
              max.time = 30,
              proposal = WpProj:::proposal.fun,
              options = list(method = c("selection.variable"),
                             energy.distribution = "boltzman",
                             transport.method = "univariate.approximation.pwr",
                             cooling.schedule = "Geman-Geman",
                             proposal.method = prop.meth)
  )
  testthat::expect_equal(sv1$optimal$index, c(1,9,10))
  
  sv2 <-  WPSA(X=x, Y=t(post_mu), t(post_beta), 
              force = 1, power = 2, nvar = 3,
              # groups = NULL,
              maxit=10, temps = 10,
              max.time = 30,
              proposal = proposal.fun,
              options = list(method = c("selection.variable"),
                             energy.distribution = "boltzman",
                             transport.method = "hilbert",
                             cooling.schedule = "Geman-Geman",
                             proposal.method = prop.meth)
  )
  testthat::expect_equal(sv2$optimal$index, c(1,9,10))
  
  sv3 <-  WPSA(X=x, Y=t(post_mu), t(post_beta), 
              force = 1, power = 2, nvar = 3,
              # groups = NULL,
              maxit=10, temps = 10,
              max.time = 30,
              proposal = proposal.fun,
              options = list(method = c("selection.variable"),
                             energy.distribution = "boltzman",
                             transport.method = "rank",
                             cooling.schedule = "Geman-Geman",
                             proposal.method = prop.meth)
  )
  testthat::expect_equal(sv3$optimal$index, c(1,9,10))
  
  sv4 <-  WPSA(X=x, Y=t(post_mu), t(post_beta), 
               force = 1, power = 2, nvar = 3,
               # groups = NULL,
               maxit=10, temps = 10,
               max.time = 30,
               proposal = proposal.fun,
               options = list(method = c("selection.variable"),
                              energy.distribution = "boltzman",
                              transport.method = "sinkhorn",
                              cooling.schedule = "Geman-Geman",
                              proposal.method = prop.meth)
  )
  testthat::expect_equal(sv4$optimal$index, c(1,9,10))
  
  sv5 <-  WPSA(X=x, Y=t(post_mu), t(post_beta), 
               force = 1, power = 2, nvar = 3,
               # groups = NULL,
               maxit=10, temps = 10,
               max.time = 30,
               proposal = proposal.fun,
               options = list(method = c("selection.variable"),
                              energy.distribution = "boltzman",
                              transport.method = "greenkhorn",
                              cooling.schedule = "Geman-Geman",
                              proposal.method = prop.meth)
  )
  testthat::expect_equal(sv5$optimal$index, c(1,9,10))
  # no rand or gandkhorn
  # sv6 <-  WPSA(X=x, Y=t(post_mu), t(post_beta), 
  #              force = 1, power = 2, nvar = 3,
  #              # groups = NULL,
  #              maxit=10, temps = 10,
  #              max.time = 30,
  #              proposal = proposal.fun,
  #              options = list(method = c("selection.variable"),
  #                             energy.distribution = "boltzman",
  #                             transport.method = "randkhorn",
  #                             cooling.schedule = "Geman-Geman",
  #                             proposal.method = prop.meth)
  # )
  # testthat::expect_equal(sv6$optimal$index, c(1,9,10))
  
  # sv7 <-  WPSA(X=x, Y=t(post_mu), t(post_beta), 
  #              force = 1, power = 2, nvar = 3,
  #              # groups = NULL,
  #              maxit=10, temps = 10,
  #              max.time = 30,
  #              proposal = proposal.fun,
  #              options = list(method = c("selection.variable"),
  #                             energy.distribution = "boltzman",
  #                             transport.method = "gandkhorn",
  #                             cooling.schedule = "Geman-Geman",
  #                             proposal.method = prop.meth)
  # )
  # testthat::expect_equal(sv7$optimal$index, c(1,9,10))
})

testthat::test_that("WPSA timing works", {
  set.seed(111)
  
  ##### Testing R Functions ####
  n <- 100
  p <- 10
  s <- 1000
  
  x <- matrix( rnorm( p * n ), nrow = n, ncol = p )
  x_ <- t(x)
  beta <- (1:10)/10
  y <- x %*% beta + rnorm(n)
  post_beta <- matrix(beta, nrow=p, ncol=s) + rnorm(p*s, 0, 0.1)
  post_mu <- x %*% post_beta
  
  xtx <- crossprod(x)/n #* wt + diag(1,p,p) * (1 - wt)
  xty <- crossprod(x, post_mu)/n #* wt + post_beta * (1 - wt)
  
  transport.method <- "exact"
  prop.meth <- "covariance"
  otopt <- list(
    same = TRUE,
    method = "selection.variable",
    transport.method = transport.method,
    epsilon = 0.05,
    niter = 100
  )
  
  suffStat_star <- sufficientStatistics(x, post_mu, t(post_beta), otopt)
  xtx_star <- suffStat_star$XtX #* wt + diag(post_beta_norm) * (1-wt)
  xty_star <- suffStat_star$XtY #* wt + post_beta_norm * (1-wt)
  
  sv <-  WPSA(X=x, Y=t(post_mu), t(post_beta), 
              force = 1, power = 2, nvar = 2:(p-1),
              # groups = NULL,
              maxit=10, temps = 1000,
              max.time = 1,
              proposal = proposal.fun,
              options = list(method = c("selection.variable"),
                             energy.distribution = "boltzman",
                             transport.method = "exact",
                             cooling.schedule = "Geman-Geman",
                             proposal.method = "covariance"),
              display.progress = FALSE
  )
  testthat::expect_equal(sv$message, "Hit max time exploring model sizes")
  testthat::skip_on_cran()
  sv <-  WPSA(X=x, Y=t(post_mu), t(post_beta), 
              force = 1, power = 2, nvar = 2:(p-1),
              # groups = NULL,
              maxit=5, temps = 5,
              max.time = 300,
              proposal = proposal.fun,
              options = list(method = c("selection.variable"),
                             energy.distribution = "boltzman",
                             transport.method = "univariate.approximation.pwr",
                             cooling.schedule = "exponential",
                             proposal.method = "covariance"),
              display.progress = FALSE
  )
  testthat::expect_equal(sv$message, "completed")
})

testthat::test_that("WPSA projection", {
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
  
  transport.method <- "hilbert"
  prop.meth <- "random"
  
  otopt <- list(
    same = TRUE,
    method = "selection.variable",
    transport.method = transport.method,
    epsilon = 0.05,
    niter = 100
  )
  
  suffStat_star <- WpProj:::sufficientStatistics(x, post_mu, t(post_beta), otopt)
  xtx_star <- suffStat_star$XtX #* wt + diag(post_beta_norm) * (1-wt)
  xty_star <- suffStat_star$XtY #* wt + post_beta_norm * (1-wt)
  
  sv <-  WPSA(X=x, Y=t(post_mu), t(post_beta), 
              force = 1, power = 2, nvar = 3,
              # groups = NULL,
              maxit=5, temps = 5,
              max.time = 10,
              proposal = WpProj:::proposal.fun,
              options = list(method = c("projection"),
                             energy.distribution = "boltzman",
                             transport.method = "exact",
                             cooling.schedule = "Geman-Geman",
                             proposal.method = prop.meth)
  )
  testthat::expect_true(all(sv$optimal$index %in% c(1,8,9,10)))
  testthat::skip_on_cran()
  sv1 <-  WPSA(X=x, Y=t(post_mu), t(post_beta), 
               force = 1, power = 2, nvar = 3,
               # groups = NULL,
               maxit=10, temps = 10,
               max.time = 30,
               proposal = WpProj:::proposal.fun,
               options = list(method = c("projection"),
                              energy.distribution = "boltzman",
                              transport.method = "univariate.approximation.pwr",
                              cooling.schedule = "Geman-Geman",
                              proposal.method = prop.meth)
  )
  testthat::expect_true(all(sv$optimal$index %in% c(1,8,9,10)))
  
  sv2 <-  WPSA(X=x, Y=t(post_mu), t(post_beta), 
               force = 1, power = 2, nvar = 3,
               # groups = NULL,
               maxit=10, temps = 10,
               max.time = 30,
               proposal = proposal.fun,
               options = list(method = c("projection"),
                              energy.distribution = "boltzman",
                              transport.method = "hilbert",
                              cooling.schedule = "Geman-Geman",
                              proposal.method = prop.meth)
  )
  testthat::expect_equal(sv2$optimal$index, c(1,9,10))
  
  sv3 <-  WPSA(X=x, Y=t(post_mu), t(post_beta), 
               force = 1, power = 2, nvar = 3,
               # groups = NULL,
               maxit=10, temps = 10,
               max.time = 30,
               proposal = proposal.fun,
               options = list(method = c("projection"),
                              energy.distribution = "boltzman",
                              transport.method = "rank",
                              cooling.schedule = "Geman-Geman",
                              proposal.method = prop.meth)
  )
  testthat::expect_equal(sv3$optimal$index, c(1,9,10))
  
  sv4 <-  WPSA(X=x, Y=t(post_mu), t(post_beta), 
               force = 1, power = 2, nvar = 3,
               # groups = NULL,
               maxit=10, temps = 10,
               max.time = 30,
               proposal = proposal.fun,
               options = list(method = c("projection"),
                              energy.distribution = "boltzman",
                              transport.method = "sinkhorn",
                              cooling.schedule = "Geman-Geman",
                              proposal.method = prop.meth)
  )
  testthat::expect_equal(sv4$optimal$index, c(1,9,10))
  
  # sv5 <-  WPSA(X=x, Y=t(post_mu), t(post_beta), 
  #              force = 1, power = 2, nvar = 3,
  #              # groups = NULL,
  #              maxit=10, temps = 10,
  #              max.time = 30,
  #              proposal = proposal.fun,
  #              options = list(method = c("projection"),
  #                             energy.distribution = "boltzman",
  #                             transport.method = "greenkhorn",
  #                             cooling.schedule = "Geman-Geman",
  #                             proposal.method = prop.meth)
  # )
  # testthat::expect_equal(sv5$optimal$index, c(1,9,10))
  # 
  # sv6 <-  WPSA(X=x, Y=t(post_mu), t(post_beta), 
  #              force = 1, power = 2, nvar = 3,
  #              # groups = NULL,
  #              maxit=10, temps = 10,
  #              max.time = 30,
  #              proposal = proposal.fun,
  #              options = list(method = c("projection"),
  #                             energy.distribution = "boltzman",
  #                             transport.method = "randkhorn",
  #                             cooling.schedule = "Geman-Geman",
  #                             proposal.method = prop.meth)
  # )
  # testthat::expect_equal(sv6$optimal$index, c(1,9,10))
  
  # sv7 <-  WPSA(X=x, Y=t(post_mu), t(post_beta), 
  #              force = 1, power = 2, nvar = 3,
  #              # groups = NULL,
  #              maxit=10, temps = 10,
  #              max.time = 30,
  #              proposal = proposal.fun,
  #              options = list(method = c("projection"),
  #                             energy.distribution = "boltzman",
  #                             transport.method = "gandkhorn",
  #                             cooling.schedule = "Geman-Geman",
  #                             proposal.method = prop.meth)
  # )
  # testthat::expect_equal(sv7$optimal$index, c(1,9,10))
})
