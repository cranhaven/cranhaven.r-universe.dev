test_that("distance compare gives correct values for wass", {
  
  set.seed(84370158)
  
  n <- 128
  p <- 10
  s <- 100
  
  x <- matrix( rnorm( p * n ), nrow = n, ncol = p )
  x_ <- t(x)
  beta <- (1:p)/p
  y <- x %*% beta + rnorm(n)
  post_beta <- matrix(beta, nrow=p, ncol=s) + rnorm(p*s, 0, 0.1)
  post_mu <- x %*% post_beta
  transp <- "exact"
  model.size <- c(2,4,8)
  
  test <- WpProj(X = x, eta = post_mu, theta = post_beta, method = "binary program",
                 solver = "ecos")
  
  proj <- WpProj(x, post_mu, post_beta)
  sel <- WpProj(x, post_mu, post_beta, method = "binary program")
  
  out <- list(test, proj, sel)
  # debugonce(distCompare)
  dist <- distCompare(out, list(parameters = post_beta, predictions = post_mu),power = 2, quantity = c("parameters", "predictions"))
  compost <- unlist(sapply(out, function(o) sapply(o$theta, function(tt)  WpProj::wasserstein(tt, post_beta, 2, 2, "colwise","exact"))))
  compredictions <- unlist(sapply(out, function(o) sapply(o$fitted.values, function(tt)  WpProj::wasserstein(tt, post_mu, 2, 2, "colwise","exact"))))
  
  testthat::expect_equal(dist$parameters$dist, compost)
  testthat::expect_equal(dist$predictions$dist, compredictions)
})

testthat::test_that("distance compare gives correct values for mse", {
  
  set.seed(84370158)
  
  n <- 128
  p <- 10
  s <- 100
  
  x <- matrix( rnorm( p * n ), nrow = n, ncol = p )
  x_ <- t(x)
  beta <- (1:p)/p
  y <- x %*% beta + rnorm(n)
  post_beta <- matrix(beta, nrow=p, ncol=s) + rnorm(p*s, 0, 0.1)
  post_mu <- x %*% post_beta
  mu <- x %*% beta
  transp <- "exact"
  model.size <- c(2,4,8)
  
  test <- WpProj(X = x, eta = post_mu, theta = post_beta, method = "binary program",
                 solver = "ecos")
  
  proj <- WpProj(x, post_mu, post_beta)
  sel <- WpProj(x, post_mu, post_beta, method = "binary program")
  
  out <- list(test, proj, sel)
  # debugonce(distCompare)
  mse <- distCompare(out, list(parameters = beta, predictions =  mu),power = 2, quantity = c("parameters", "predictions"), method = "mse")
  compost <- unlist(sapply(out, function(o) sapply(o$theta, function(tt)  WpProj::wasserstein(tt, as.matrix(beta), 2, 2, "colwise","exact"))))^2/p
  compredictions <- unlist(sapply(out, function(o) sapply(o$fitted.values, function(tt)  WpProj::wasserstein(tt,  as.matrix(mu), 2, 2, "colwise","exact"))))^2/n
  
  testthat::expect_equal(mse$parameters$dist, compost)
  testthat::expect_equal(mse$predictions$dist, compredictions)
})

testthat::test_that("distance compare gives correct group names", {

  set.seed(84370158)

  n <- 128
  p <- 10
  s <- 100

  x <- matrix( rnorm( p * n ), nrow = n, ncol = p )
  x_ <- t(x)
  beta <- (1:p)/p
  y <- x %*% beta + rnorm(n)
  post_beta <- matrix(beta, nrow=p, ncol=s) + rnorm(p*s, 0, 0.1)
  post_mu <- x %*% post_beta
  mu <- x %*% beta
  transp <- "exact"
  model.size <- c(2,4,8)

  test <- WpProj(X = x, eta = post_mu, theta = post_beta, method = "binary program",
                 solver = "ecos")
  
  proj <- WpProj(x, post_mu, post_beta)
  sel <- WpProj(x, post_mu, post_beta, method = "binary program")

  out <- list(Test=test, Projection = proj, Selection = sel)
  # debugonce(distCompare)
  mse <- distCompare(out, list(parameters = beta, predictions =  mu),power = 2, quantity = c("parameters", "predictions"), method = "mse")
  compost <- unlist(sapply(out, function(o) sapply(o$theta, function(tt)  WpProj::wasserstein(tt, as.matrix(beta), 2, 2, "colwise","exact"))))^2/p
  compredictions <- unlist(sapply(out, function(o) sapply(o$fitted.values, function(tt)  WpProj::wasserstein(tt,  as.matrix(mu), 2, 2, "colwise","exact"))))^2/n

  expectnames <- c(rep('Test',10), rep('Projection',11),
                   rep('Selection',11))
  testthat::expect_equivalent(mse$parameters$dist, compost)
  testthat::expect_equivalent(mse$predictions$dist, compredictions)
  testthat::expect_equal(as.character(mse$parameters$groups), expectnames)
  testthat::expect_equal(as.character(mse$predictions$groups), expectnames)
})
