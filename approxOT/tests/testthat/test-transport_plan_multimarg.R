testthat::test_that("multimarg cost, l2", {
  set.seed(23423)
  n <- 100
  d <- 10
  x <- matrix(rnorm(n*d), n , d)
  y <- matrix(rnorm(n*d), n , d)
  z <- matrix(rnorm(n*d), n , d)
  
  data <- list(x,y,z)
  datat <- lapply(data, t)
  indexes <- lapply(1:3, function(i) sample.int(n,n))
  mass <- rep(1/n,n)
  p <- ground_p <- 2
  
  cost <- approxOT:::multi_marg_final_cost_(indexes, datat, mass = mass,
                                   M = n, D = d, p = p, ground_p = ground_p)
  
  dists <- sqrt(weighted.mean(rowSums(abs(data[[1]][indexes[[1]],] - data[[2]][indexes[[2]],])^p) + 
                      rowSums(abs(data[[1]][indexes[[1]],] - data[[3]][indexes[[3]],])^p) + 
                      rowSums(abs(data[[2]][indexes[[2]],] - data[[3]][indexes[[3]],])^p),
                      w = mass))
  
  testthat::expect_equivalent(dists, cost)
  
})

testthat::test_that("multimarg cost, l1", {
  set.seed(23423)
  n <- 100
  d <- 10
  x <- matrix(rnorm(n*d), n , d)
  y <- matrix(rnorm(n*d), n , d)
  z <- matrix(rnorm(n*d), n , d)
  
  data <- list(x,y,z)
  datat <- lapply(data, t)
  indexes <- lapply(1:3, function(i) sample.int(n,n))
  mass <- rep(1/n,n)
  p <- ground_p <- 1
  
  cost <- approxOT:::multi_marg_final_cost_(indexes, datat, mass = mass,
                                 M = n, D = d, p = p, ground_p = ground_p)
  
  dists <- (weighted.mean(rowSums(abs(data[[1]][indexes[[1]],] - data[[2]][indexes[[2]],])^p) + 
                                rowSums(abs(data[[1]][indexes[[1]],] - data[[3]][indexes[[3]],])^p) + 
                                rowSums(abs(data[[2]][indexes[[2]],] - data[[3]][indexes[[3]],])^p),
                              w = mass))
  
  testthat::expect_equivalent(dists, cost)
  
})

testthat::test_that("multimarg cost, l3", {
  set.seed(23423)
  n <- 100
  d <- 10
  x <- matrix(rnorm(n*d), n , d)
  y <- matrix(rnorm(n*d), n , d)
  z <- matrix(rnorm(n*d), n , d)
  
  data <- list(x,y,z)
  datat <- lapply(data, t)
  indexes <- lapply(1:3, function(i) sample.int(n,n))
  mass <- rep(1/n,n)
  p <- ground_p <- 3
  
  cost <- approxOT:::multi_marg_final_cost_(indexes, datat, mass = mass,
                                 M = n, D = d, p = p, ground_p = ground_p)
  
  dists <- (weighted.mean(rowSums(abs(data[[1]][indexes[[1]],] - data[[2]][indexes[[2]],])^p) + 
                            rowSums(abs(data[[1]][indexes[[1]],] - data[[3]][indexes[[3]],])^p) + 
                            rowSums(abs(data[[2]][indexes[[2]],] - data[[3]][indexes[[3]],])^p),
                          w = mass))^(1/3)
  
  testthat::expect_equivalent(dists, cost)
  
})

testthat::test_that("multimarg transportation equal, hilbert", {
  set.seed(23423)
  n <- 100
  d <- 10
  x <- matrix(rnorm(n*d), n , d)
  y <- matrix(rnorm(n*d), n , d)
  z <- matrix(rnorm(n*d), n , d)
  
  data <- list(x,y,z)
  indexes <- lapply(data, approxOT::hilbert.projection)
  mass <- rep(1/n,n)
  p <- ground_p <- 2
  
  dists <- sqrt(weighted.mean(rowSums(abs(data[[1]][indexes[[1]],] - data[[2]][indexes[[2]],])^p) + 
                                rowSums(abs(data[[1]][indexes[[1]],] - data[[3]][indexes[[3]],])^p) + 
                                rowSums(abs(data[[2]][indexes[[2]],] - data[[3]][indexes[[3]],])^p),
                              w = mass))
  
  # debugonce(transport_plan_multimarg)
  tplan <- approxOT::transport_plan_multimarg(data, p = p, ground_p = ground_p,
                           observation.orientation = "rowwise",
                           method = "hilbert")
  
  testthat::expect_equivalent(dists, tplan$cost)
  testthat::expect_equivalent(indexes, tplan$tplan$indexes)
  
})

testthat::test_that("multimarg transportation equal, univariate", {
  set.seed(01212)
  n <- 100
  d <- 1
  x <- matrix(rnorm(n*d), n , d)
  y <- matrix(rnorm(n*d), n , d)
  z <- matrix(rnorm(n*d), n , d)
  
  data <- list(x,y,z)
  indexes <- lapply(data, order)
  mass <- rep(1/n,n)
  p <- ground_p <- 2
  
  dists <- sqrt(weighted.mean((abs(data[[1]][indexes[[1]],] - data[[2]][indexes[[2]],])^p) + 
                                (abs(data[[1]][indexes[[1]],] - data[[3]][indexes[[3]],])^p) + 
                                (abs(data[[2]][indexes[[2]],] - data[[3]][indexes[[3]],])^p),
                              w = mass))
  
  # debugonce(transport_plan_multimarg)
  tplan <- transport_plan_multimarg(data, p = p, ground_p = ground_p,
                                    observation.orientation = "rowwise",
                                    method = "univariate")
  
  testthat::expect_equivalent(dists, tplan$cost)
  testthat::expect_equivalent(indexes, tplan$tplan$indexes)
  
})

testthat::test_that("multimarg transportation unequal, hilbert", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("transport")
  set.seed(23423)
  n <- 100
  d <- 10
  x <- matrix(rnorm((n + 11)*d), n + 11 , d)
  y <- matrix(rnorm(n*d), n, d)
  z <- matrix(rnorm((n +455)*d), n + 455, d)
  
  data <- list(x,y,z)
  datat <- lapply(data, t)
  
  indexes <- lapply(data, approxOT::hilbert.projection)
  mass <- lapply(data, function(d) rep(1/nrow(d), nrow(d)))
  p <- ground_p <- 2
  
  t12 <- general_1d_transport(datat[[1]], datat[[2]])
  t13 <- general_1d_transport(datat[[1]], datat[[3]])
  t23 <- general_1d_transport(datat[[2]], datat[[3]])
  
  c12 <- cost_calc(datat[[1]], datat[[2]], ground_p = ground_p)
  c13 <- cost_calc(datat[[1]], datat[[3]], ground_p = ground_p)
  c23 <- cost_calc(datat[[2]], datat[[3]], ground_p = ground_p)
  
  w12 <- transport::wasserstein(mass[[1]], mass[[2]], p = p, tplan = data.frame(t12), costm = c12)
  w13 <- transport::wasserstein(mass[[1]], mass[[3]], p = p, tplan =  data.frame(t13), costm = c13)
  w23 <- transport::wasserstein(mass[[2]], mass[[3]], p = p, tplan =  data.frame(t23), costm = c23)
  
  # debugonce(transport_plan_multimarg)
  tplan <- transport_plan_multimarg(data, p = p, ground_p = ground_p,
                                    observation.orientation = "rowwise",
                                    method = "hilbert")
  
  testthat::expect_equivalent((w12^p + w23^p + w13^p)^(1/p), tplan$cost)
  testthat::expect_equivalent(indexes, lapply(tplan$tplan$indexes, unique))
  
})

testthat::test_that("multimarg transportation unequal, univariate", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("transport")
  set.seed(23423)
  n <- 100
  d <- 1
  x <- matrix(rnorm((n + 11)*d), n + 11 , d)
  y <- matrix(rnorm(n*d), n, d)
  z <- matrix(rnorm((n +455)*d), n +455, d)
  
  data <- list(x,y,z)
  datat <- lapply(data, t)
  
  indexes <- lapply(data, order)
  mass <- lapply(data, function(d) rep(1/nrow(d), nrow(d)))
  p <- ground_p <- 2
  
  t12 <- general_1d_transport(datat[[1]], datat[[2]], method = "univariate")
  t13 <- general_1d_transport(datat[[1]], datat[[3]], method = "univariate")
  t23 <- general_1d_transport(datat[[2]], datat[[3]], method = "univariate")
  
  c12 <- cost_calc(datat[[1]], datat[[2]], ground_p = ground_p)
  c13 <- cost_calc(datat[[1]], datat[[3]], ground_p = ground_p)
  c23 <- cost_calc(datat[[2]], datat[[3]], ground_p = ground_p)
  
  w12 <- transport::wasserstein(mass[[1]], mass[[2]], p = p, tplan = data.frame(t12), costm = c12)
  w13 <- transport::wasserstein(mass[[1]], mass[[3]], p = p, tplan = data.frame(t13), costm = c13)
  w23 <- transport::wasserstein(mass[[2]], mass[[3]], p = p, tplan = data.frame(t23), costm = c23)
  
  # debugonce(transport_plan_multimarg)
  tplan <- transport_plan_multimarg(data, p = p, ground_p = ground_p,
                                    observation.orientation = "rowwise",
                                    method = "univariate")
  
  testthat::expect_equivalent((w12^p + w23^p + w13^p)^(1/p), tplan$cost)
  testthat::expect_equivalent(indexes, lapply(tplan$tplan$indexes, unique))
  
})
