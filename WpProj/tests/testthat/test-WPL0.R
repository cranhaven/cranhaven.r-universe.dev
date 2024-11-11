test_that("WPL0 with exact transport", {
  set.seed(84370158)
  
  n <- 32
  p <- 5
  s <- 21
  
  x <- matrix( stats::rnorm( p * n ), nrow = n, ncol = p )
  x_ <- t(x)
  beta <- (1:p)/p
  y <- x %*% beta + stats::rnorm(n)
  post_beta <- matrix(beta, nrow=p, ncol=s) + stats::rnorm(p*s, 0, 0.1)
  post_mu <- x %*% post_beta
  transp <- "exact"

  
  l0 <- WPL0(X = x, Y = NULL, theta = post_beta, power = 2,
            method = c("selection.variable"),
            transport.method = transp,
            epsilon = 0.05, OTmaxit = 100,
            parallel = NULL)
  
  testthat::skip_on_cran()
  
  l0.1 <- WPL0(X = x, Y = NULL, theta = post_beta, power = 2,
                     method = c("projection"),
                     transport.method = transp,
                     epsilon = 0.05, OTmaxit = 100,
                     parallel = NULL)
  
  nc <- parallel::detectCores()-1
  if (nc > 2) nc <- 2
    cl <- parallel::makeCluster(nc)
    doParallel::registerDoParallel(cl)
    l0.2 <- WPL0(X = x, Y = NULL, theta = post_beta, power = 2,
                 method = c("selection.variable"),
                 transport.method = transp,
                 epsilon = 0.05, OTmaxit = 100,
                 parallel = cl)
    l0.3 <- WPL0(X = x, Y = NULL, theta = post_beta, power = 2,
                 method = c("projection"),
                 transport.method = transp,
                 epsilon = 0.05, OTmaxit = 100,
                 parallel = cl)
    parallel::stopCluster(cl)
    
    testthat::expect_equal(l0$min_combination, l0.2$min_combination)
    testthat::expect_equal(l0.1$min_combination, l0.3$min_combination)
  
  
})

testthat::test_that("WPL0 with sinkhorn transport", {
  set.seed(84370158)
  
  n <- 32
  p <- 5
  s <- 21
  
  x <- matrix( stats::rnorm( p * n ), nrow = n, ncol = p )
  x_ <- t(x)
  beta <- (1:p)/p
  y <- x %*% beta + stats::rnorm(n)
  post_beta <- matrix(beta, nrow=p, ncol=s) + stats::rnorm(p*s, 0, 0.1)
  post_mu <- x %*% post_beta
  transp <- "sinkhorn"
  
  
  l0 <- WPL0(X = x, Y = NULL, theta = post_beta, power = 2,
             method = c("selection.variable"),
             transport.method = transp,
             epsilon = 0.05, OTmaxit = 100,
             parallel = NULL)
  
  testthat::skip_on_cran()
  
  l0.1 <- WPL0(X = x, Y = NULL, theta = post_beta, power = 2,
               method = c("projection"),
               transport.method = transp,
               epsilon = 0.05, OTmaxit = 100,
               parallel = NULL)
  
  nc <- parallel::detectCores()-1
  if (nc > 2) nc <- 2
  cl <- parallel::makeCluster(nc)
  doParallel::registerDoParallel(cl)
  l0.2 <- WPL0(X = x, Y = NULL, theta = post_beta, power = 2,
               method = c("selection.variable"),
               transport.method = transp,
               epsilon = 0.05, OTmaxit = 100,
               parallel = cl)
  l0.3 <- WPL0(X = x, Y = NULL, theta = post_beta, power = 2,
               method = c("projection"),
               transport.method = transp,
               epsilon = 0.05, OTmaxit = 100,
               parallel = cl)
  parallel::stopCluster(cl)
  
  testthat::expect_equal(l0$min_combination, l0.2$min_combination)
  testthat::expect_equal(l0.1$min_combination, l0.3$min_combination)
  
})

testthat::test_that("WPL0 with greenkhorn transport", {
  set.seed(84370158)
  
  n <- 32
  p <- 5
  s <- 21
  
  x <- matrix( stats::rnorm( p * n ), nrow = n, ncol = p )
  x_ <- t(x)
  beta <- (1:p)/p
  y <- x %*% beta + stats::rnorm(n)
  post_beta <- matrix(beta, nrow=p, ncol=s) + stats::rnorm(p*s, 0, 0.1)
  post_mu <- x %*% post_beta
  transp <- "greenkhorn"
  
  
  l0 <- WPL0(X = x, Y = NULL, theta = post_beta, power = 2,
             method = c("selection.variable"),
             transport.method = transp,
             epsilon = 0.05, OTmaxit = 100,
             parallel = NULL)
  testthat::skip_on_cran()
  l0.1 <- WPL0(X = x, Y = NULL, theta = post_beta, power = 2,
               method = c("projection"),
               transport.method = transp,
               epsilon = 0.05, OTmaxit = 100,
               parallel = NULL)
  
  nc <- parallel::detectCores()-1
  if (nc > 2) nc <- 2
  cl <- parallel::makeCluster(nc)
  doParallel::registerDoParallel(cl)
  l0.2 <- WPL0(X = x, Y = NULL, theta = post_beta, power = 2,
               method = c("selection.variable"),
               transport.method = transp,
               epsilon = 0.05, OTmaxit = 100,
               parallel = cl)
  l0.3 <- WPL0(X = x, Y = NULL, theta = post_beta, power = 2,
               method = c("projection"),
               transport.method = transp,
               epsilon = 0.05, OTmaxit = 100,
               parallel = cl)
  parallel::stopCluster(cl)
  
  testthat::expect_equal(l0$min_combination, l0.2$min_combination)
  testthat::expect_equal(l0.1$min_combination, l0.3$min_combination)
  
})

# testthat::test_that("WPL0 with randkhorn transport", {
#   set.seed(84370158)
#   
#   n <- 128
#   p <- 5
#   s <- 99
#   
#   x <- matrix( stats::rnorm( p * n ), nrow = n, ncol = p )
#   x_ <- t(x)
#   beta <- (1:p)/p
#   y <- x %*% beta + stats::rnorm(n)
#   post_beta <- matrix(beta, nrow=p, ncol=s) + stats::rnorm(p*s, 0, 0.1)
#   post_mu <- x %*% post_beta
#   transp <- "randkhorn"
#   
#   
#   l0 <- WPL0(X = x, Y = NULL, theta = post_beta, power = 2,
#              method = c("selection.variable"),
#              transport.method = transp,
#              epsilon = 0.05, OTmaxit = 100,
#              parallel = NULL)
#   
#   l0.1 <- WPL0(X = x, Y = NULL, theta = post_beta, power = 2,
#                method = c("projection"),
#                transport.method = transp,
#                epsilon = 0.05, OTmaxit = 100,
#                parallel = NULL)
#   
#   cl <- parallel::makeCluster(parallel::detectCores()-1)
#   doParallel::registerDoParallel(cl)
#   l0.2 <- WPL0(X = x, Y = NULL, theta = post_beta, power = 2,
#                method = c("selection.variable"),
#                transport.method = transp,
#                epsilon = 0.05, OTmaxit = 100,
#                parallel = cl)
#   doParallel::registerDoParallel(cl)
#   l0.3 <- WPL0(X = x, Y = NULL, theta = post_beta, power = 2,
#                method = c("projection"),
#                transport.method = transp,
#                epsilon = 0.05, OTmaxit = 100,
#                parallel = cl)
#   parallel::stopCluster(cl)
#   
#   testthat::expect_equal(l0$min_combination, l0.2$min_combination)
#   testthat::expect_equal(l0.1$min_combination, l0.3$min_combination)
#   
# })
# 
# testthat::test_that("WPL0 with gandkhorn transport", {
#   set.seed(84370158)
#   
#   n <- 128
#   p <- 5
#   s <- 99
#   
#   x <- matrix( stats::rnorm( p * n ), nrow = n, ncol = p )
#   x_ <- t(x)
#   beta <- (1:p)/p
#   y <- x %*% beta + stats::rnorm(n)
#   post_beta <- matrix(beta, nrow=p, ncol=s) + stats::rnorm(p*s, 0, 0.1)
#   post_mu <- x %*% post_beta
#   transp <- "gandkhorn"
#   
#   
#   l0 <- WPL0(X = x, Y = NULL, theta = post_beta, power = 2,
#              method = c("selection.variable"),
#              transport.method = transp,
#              epsilon = 0.05, OTmaxit = 100,
#              parallel = NULL)
#   
#   l0.1 <- WPL0(X = x, Y = NULL, theta = post_beta, power = 2,
#                method = c("projection"),
#                transport.method = transp,
#                epsilon = 0.05, OTmaxit = 100,
#                parallel = NULL)
#   
#   cl <- parallel::makeCluster(parallel::detectCores()-1)
#   doParallel::registerDoParallel(cl)
#   l0.2 <- WPL0(X = x, Y = NULL, theta = post_beta, power = 2,
#                method = c("selection.variable"),
#                transport.method = transp,
#                epsilon = 0.05, OTmaxit = 100,
#                parallel = cl)
#   doParallel::registerDoParallel(cl)
#   l0.3 <- WPL0(X = x, Y = NULL, theta = post_beta, power = 2,
#                method = c("projection"),
#                transport.method = transp,
#                epsilon = 0.05, OTmaxit = 100,
#                parallel = cl)
#   parallel::stopCluster(cl)
#   
#   testthat::expect_equal(l0$min_combination, l0.2$min_combination)
#   testthat::expect_equal(l0.1$min_combination, l0.3$min_combination)
#   
# })

testthat::test_that("WPL0 with hilbert transport", {
  set.seed(84370158)
  
  n <- 32
  p <- 5
  s <- 21
  
  x <- matrix( stats::rnorm( p * n ), nrow = n, ncol = p )
  x_ <- t(x)
  beta <- (1:p)/p
  y <- x %*% beta + stats::rnorm(n)
  post_beta <- matrix(beta, nrow=p, ncol=s) + stats::rnorm(p*s, 0, 0.1)
  post_mu <- x %*% post_beta
  transp <- "hilbert"
  
  
  l0 <- WpProj:::WPL0(X = x, Y = NULL, theta = post_beta, power = 2,
             method = c("selection.variable"),
             transport.method = transp,
             epsilon = 0.05, OTmaxit = 100,
             parallel = NULL)
  
  l0.1 <- WpProj:::WPL0(X = x, Y = NULL, theta = post_beta, power = 2,
               method = c("projection"),
               transport.method = transp,
               epsilon = 0.05, OTmaxit = 100,
               parallel = NULL)
  
  nc <- parallel::detectCores()-1
  if (nc > 2) nc <- 2
  cl <- parallel::makeCluster(nc)
  doParallel::registerDoParallel(cl)
  l0.2 <- WPL0(X = x, Y = NULL, theta = post_beta, power = 2,
               method = c("selection.variable"),
               transport.method = transp,
               epsilon = 0.05, OTmaxit = 100,
               parallel = cl)
  l0.3 <- WPL0(X = x, Y = NULL, theta = post_beta, power = 2,
               method = c("projection"),
               transport.method = transp,
               epsilon = 0.05, OTmaxit = 100,
               parallel = cl)
  parallel::stopCluster(cl)
  
  testthat::expect_equal(l0$min_combination, l0.2$min_combination)
  testthat::expect_equal(l0.1$min_combination, l0.3$min_combination)
  
})

testthat::test_that("WPL0 with rank transport", {
  set.seed(84370158)
  
  n <- 128
  p <- 5
  s <- 99
  
  x <- matrix( stats::rnorm( p * n ), nrow = n, ncol = p )
  x_ <- t(x)
  beta <- (1:p)/p
  y <- x %*% beta + stats::rnorm(n)
  post_beta <- matrix(beta, nrow=p, ncol=s) + stats::rnorm(p*s, 0, 0.1)
  post_mu <- x %*% post_beta
  transp <- "rank"
  
  
  l0 <- WPL0(X = x, Y = NULL, theta = post_beta, power = 2,
             method = c("selection.variable"),
             transport.method = transp,
             epsilon = 0.05, OTmaxit = 100,
             parallel = NULL)
  
  l0.1 <- WPL0(X = x, Y = NULL, theta = post_beta, power = 2,
               method = c("projection"),
               transport.method = transp,
               epsilon = 0.05, OTmaxit = 100,
               parallel = NULL)
  
  nc <- parallel::detectCores()-1
  if (nc > 2) nc <- 2
  cl <- parallel::makeCluster(nc)
  doParallel::registerDoParallel(cl)
  l0.2 <- WPL0(X = x, Y = NULL, theta = post_beta, power = 2,
               method = c("selection.variable"),
               transport.method = transp,
               epsilon = 0.05, OTmaxit = 100,
               parallel = cl)
  l0.3 <- WPL0(X = x, Y = NULL, theta = post_beta, power = 2,
               method = c("projection"),
               transport.method = transp,
               epsilon = 0.05, OTmaxit = 100,
               parallel = cl)
  parallel::stopCluster(cl)
  
  testthat::expect_equal(l0$min_combination, l0.2$min_combination)
  testthat::expect_equal(l0.1$min_combination, l0.3$min_combination)
  
})

testthat::test_that("WPL0 with univariate.approximation.pwr transport", {
  set.seed(84370158)
  
  n <- 32
  p <- 5
  s <- 21
  
  x <- matrix( stats::rnorm( p * n ), nrow = n, ncol = p )
  x_ <- t(x)
  beta <- (1:p)/p
  y <- x %*% beta + stats::rnorm(n)
  post_beta <- matrix(beta, nrow=p, ncol=s) + stats::rnorm(p*s, 0, 0.1)
  post_mu <- x %*% post_beta
  transp <- "univariate.approximation.pwr"
  
  
  l0 <- WPL0(X = x, Y = NULL, theta = post_beta, power = 2,
             method = c("selection.variable"),
             transport.method = transp,
             epsilon = 0.05, OTmaxit = 100,
             parallel = NULL)
  
  l0.1 <- WPL0(X = x, Y = NULL, theta = post_beta, power = 2,
               method = c("projection"),
               transport.method = transp,
               epsilon = 0.05, OTmaxit = 100,
               parallel = NULL)
  
  nc <- parallel::detectCores()-1
  if (nc > 2) nc <- 2
  cl <- parallel::makeCluster(nc)
  doParallel::registerDoParallel(cl)
  l0.2 <- WPL0(X = x, Y = NULL, theta = post_beta, power = 2,
               method = c("selection.variable"),
               transport.method = transp,
               epsilon = 0.05, OTmaxit = 100,
               parallel = cl)
  l0.3 <- WPL0(X = x, Y = NULL, theta = post_beta, power = 2,
               method = c("projection"),
               transport.method = transp,
               epsilon = 0.05, OTmaxit = 100,
               parallel = cl)
  parallel::stopCluster(cl)
  
  testthat::expect_equal(l0$min_combination, l0.2$min_combination)
  testthat::expect_equal(l0.1$min_combination, l0.3$min_combination)
  
})
