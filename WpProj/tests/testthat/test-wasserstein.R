testthat::test_that("wasserstein gives 0 for same distribution", {
  set.seed(11289374)
  n <- 21
  d <- 5
  x <- matrix(stats::rnorm(n*d), nrow=d, ncol=n)
  y <- stats::rnorm(n)
  niter <- 1e2
  # y <- matrix(stats::rnorm(n*d), nrow=d, ncol=n)
  exact <- WpProj::wasserstein(X = x, Y = x, p = 2,
                               ground_p = 2, observation.orientation = "colwise",
                               method = "exact")
  hilbert <- WpProj::wasserstein(X = x, Y = x, p = 2,
                                          ground_p = 2, observation.orientation = "colwise",
                                          method = "hilbert")
  rank <- WpProj::wasserstein(X = x, Y = x, p = 2,
                                          ground_p = 2, observation.orientation = "colwise",
                                          method = "rank")
  sinkhorn <- WpProj::wasserstein(X = x, Y = x, p = 2,
                                          ground_p = 2, observation.orientation = "colwise",
                                          method = "sinkhorn", niter = niter)
  # sinkhorn2 <- WpProj::wasserstein(X = x, Y = x, p = 2,
  #                                          ground_p = 2, observation.orientation = "colwise",
  #                                          method = "sinkhorn2", niter = niter)
  greenkhorn <- WpProj::wasserstein(X = x, Y = x, p = 2,
                                           ground_p = 2, observation.orientation = "colwise",
                                           method = "greenkhorn", niter = niter)
  # randkhorn <- WpProj::wasserstein(X = x, Y = x, p = 2,
  #                                          ground_p = 2, observation.orientation = "colwise",
  #                                          method = "randkhorn", niter = niter)
  # gandkhorn <- WpProj::wasserstein(X = x, Y = x, p = 2,
  #                                           ground_p = 2, observation.orientation = "colwise",
  #                                           method = "gandkhorn", niter = niter)
  uni.pwr <- WpProj::wasserstein(X = x, Y = x, p = 2,
                                           ground_p = 2, observation.orientation = "colwise",
                                           method = "univariate.approximation.pwr")
  # uni.app <- WpProj::wasserstein(X = x, Y = x, p = 2,
  #                                                     ground_p = 2, observation.orientation = "colwise",
  #                                                     method = "univariate.approximation")
  testthat::expect_equal(exact, 0)
  testthat::expect_equal(hilbert, 0)
  testthat::expect_lt(sinkhorn, 0.17)
  # testthat::expect_lt(sinkhorn2, 0.05) #compare espen bernton and pierre jacob's funct to one from Greenkhorn paper
  testthat::expect_lt(greenkhorn, 0.17)
  # testthat::expect_lt(randkhorn, 0.05)
  # testthat::expect_lt(gandkhorn, 0.06)
  testthat::expect_equal(uni.pwr, 0)
  # testthat::expect_equal(uni.app, 0)
  
  exact.uni <- WpProj::wasserstein(X = y, Y = y, p = 2,
                                        ground_p = 2, observation.orientation = "colwise",
                                        method = "exact")
  hilbert.uni <- WpProj::wasserstein(X = y, Y = y, p = 2,
                                          ground_p = 2, observation.orientation = "colwise",
                                          method = "hilbert")
  rank.uni <- WpProj::wasserstein(X = y, Y = y, p = 2,
                                              ground_p = 2, observation.orientation = "colwise",
                                              method = "hilbert")
  sinkhorn.uni <- WpProj::wasserstein(X = y, Y = y, p = 2,
                                           ground_p = 2, observation.orientation = "colwise",
                                           method = "sinkhorn", niter = niter, epsilon = 2.4)
  # sinkhorn2.uni <- WpProj::wasserstein(X = y, Y = y, p = 2,
  #                                              ground_p = 2, observation.orientation = "colwise",
  #                                              method = "sinkhorn2", niter = niter)
  greenkhorn.uni <- WpProj::wasserstein(X = y, Y = y, p = 2,
                                               ground_p = 2, observation.orientation = "colwise",
                                               method = "greenkhorn", niter = niter)
  # randkhorn.uni <- WpProj::wasserstein(X = y, Y = y, p = 2,
  #                                              ground_p = 2, observation.orientation = "colwise",
  #                                              method = "randkhorn", niter = niter)
  # gandkhorn.uni <- WpProj::wasserstein(X = y, Y = y, p = 2,
  #                                              ground_p = 2, observation.orientation = "colwise",
  #                                              method = "gandkhorn", niter = niter)
  
  uni.pwr.uni <- WpProj::wasserstein(X = y, Y = y, p = 2,
                                          ground_p = 2, observation.orientation = "colwise",
                                          method = "univariate.approximation.pwr")
  # uni.app.uni <- WpProj::wasserstein(X = y, Y = y, p = 2,
  #                                         ground_p = 2, observation.orientation = "colwise",
  #                                         method = "univariate.approximation")
  testthat::expect_equal(exact.uni, 0)
  testthat::expect_equal(hilbert.uni, 0)
  testthat::expect_equal(rank.uni, 0)
  testthat::expect_lt(sinkhorn.uni, 1.4)
  # testthat::expect_lt(sinkhorn2.uni, 0.15)
  testthat::expect_lt(greenkhorn.uni, 0.3)
  # testthat::expect_lt(randkhorn.uni, 0.15)
  # testthat::expect_lt(gandkhorn.uni, 0.5)
  testthat::expect_equal(uni.pwr.uni, 0)
  # testthat::expect_equal(uni.app.uni, 0)
})

testthat::test_that("wasserstein matches transport package for shortsimplex", {
  testthat::skip_if_not_installed("transport")
  set.seed(11289374)
  n <- 21
  d <- 5
  x <- matrix(stats::rnorm(n*d), nrow=d, ncol=n)
  y <- matrix(stats::rnorm(n*d), nrow=d, ncol=n)
  z <- stats::rnorm(n)
  w <- stats::rnorm(n)
  # y <- matrix(stats::rnorm(n*d), nrow=d, ncol=n)
  exact <- WpProj::wasserstein(X = x, Y = y, p = 2,
                                        ground_p = 2, observation.orientation = "colwise",
                                        method = "exact")
  exact.trans <- transport::wasserstein(a = rep(1,n), b = rep(1,n), p = 2, 
                                        tplan = NULL,
                                        costm = cost_calc(x,y, 2),
                                        method = "shortsimplex"
                                        )
  uni <- WpProj::wasserstein(X = z, Y = w, 2, 2, "colwise", "univariate")
  uni.trans <- transport::wasserstein1d(a = z, b = w, 2)
  
  testthat::expect_equal(exact, exact.trans)
  testthat::expect_equal(uni, uni.trans)
  
  # check for rowwise orientation
  exact.row <- WpProj::wasserstein(X = t(x), Y = t(y), p = 2,
                                        ground_p = 2, observation.orientation = "rowwise",
                                        method = "exact")
  uni.row <- WpProj::wasserstein(X = t(t(z)), Y = t(t(w)), 2, 2, "rowwise", "univariate")

  testthat::expect_equal(exact.row, exact.trans)
  testthat::expect_equal(uni.row, uni.trans)
})

testthat::test_that("wasserstein from sp matches transport package",{
  testthat::skip_if_not_installed("transport")
  set.seed(32857)
  A <- matrix(stats::rnorm(100*104),nrow=104,ncol=100)
  B <- matrix(stats::rnorm(100*104),nrow=104,ncol=100)
  at <- t(A)
  bt <- t(B)
  cost <- cost_calc(at,bt,2)
  mass_a <- rep(1/ncol(at),ncol(at))
  mass_b <- rep(1/ncol(bt),ncol(bt))
  
  tplan <- transport_plan_given_C(mass_a, mass_b, 2, cost, "exact")
  
  loss <- wasserstein_(tplan$mass, cost, p = 2, tplan$from, tplan$to)
  
  loss_t <- transport::wasserstein(transport::pp(A),transport::pp(B),p=2, method = "shortsimplex")
  loss_t_def <- transport::wasserstein(mass_a,mass_b, tplan = data.frame(tplan), costm = cost, p=2, method = "shortsimplex")
  testthat::expect_equivalent(loss, loss_t)
  testthat::expect_equivalent(loss, loss_t_def)
  # microbenchmark::microbenchmark(transport::wasserstein(mass_a,mass_b, tplan = data.frame(tplan), costm = cost, p=2, method = "shortsimplex"), unit="us")
  # microbenchmark::microbenchmark(wasserstein_(tplan$mass, cost, p = 2, tplan$from, tplan$to), unit = "us")
  # microbenchmark::microbenchmark(sinkhorn_(mass_a, mass_b, cost^2, 0.05*median(cost^2), 100), unit="ms")
  
  C <- t(A[1:10,,drop = FALSE])
  D <- t(B[1:2,,drop = FALSE])
  
  cost2 <- cost_calc(C,D,2)
  mass_c <- rep(1/ncol(C),ncol(C))
  mass_d <- rep(1/ncol(D),ncol(D))
  tplan2 <- transport_plan_given_C(mass_c, mass_d, 2, cost2, "exact")
  loss <- wasserstein_(tplan2$mass, cost2, p = 2, tplan2$from, tplan2$to)
  loss_t_def <- transport::wasserstein(mass_c,mass_d, tplan = data.frame(tplan2), costm = cost2, p=2, method = "shortsimplex")
  testthat::expect_equivalent(loss, loss_t_def)
})

testthat::test_that("make sure wass less than all other transports", {
  set.seed(32857)
  A <- matrix(stats::rnorm(100*124),nrow=124,ncol=100)
  B <- matrix(stats::rnorm(100*124),nrow=124,ncol=100)
  at <- t(A)
  bt <- t(B)
  cost <- cost_calc(at,bt,2)
  mass_a <- rep(1/ncol(at),ncol(at))
  mass_b <- rep(1/ncol(bt),ncol(bt))
  
  tplan <- transport_plan_given_C(mass_a, mass_b, 2, cost, "exact")
  
  loss <- wasserstein_(tplan$mass, cost, p = 2, tplan$from, tplan$to)
  hilbert <- wasserstein(at,bt, 2, 2, "colwise", "hilbert")
  rank <- wasserstein(at,bt, 2, 2, "colwise", "rank")
  uap <- wasserstein(at,bt, 2, 2, "colwise", "univariate.approximation.pwr")
  sink <- wasserstein(at,bt, 2, 2, "colwise", "sinkhorn")
  grnk <- wasserstein(at,bt, 2, 2, "colwise", "greenkhorn")
  # rand <- wasserstein(at,bt, 2, 2, "colwise", "randkhorn")
  # gand <- wasserstein(at,bt, 2, 2, "colwise", "gandkhorn")
  
  testthat::expect_lt(loss, hilbert)
  testthat::expect_lt(loss, rank)
  testthat::expect_lt(loss, sink)
  testthat::expect_lt(loss, grnk)
  # testthat::expect_lt(loss, rand)
  # testthat::expect_lt(loss, gand)
  testthat::expect_gt(loss, uap)
})

testthat::test_that("make sure sinkhorn outputs agree and are less than wass", {
  set.seed(32857)
  A <- matrix(stats::rnorm(100*104),nrow=104,ncol=100)
  B <- matrix(stats::rnorm(100*104),nrow=104,ncol=100)
  at <- t(A)
  bt <- t(B)
  cost <- cost_calc(at,bt,2)
  mass_a <- rep(1/ncol(at),ncol(at))
  mass_b <- rep(1/ncol(bt),ncol(bt))
  
  tplan <- transport_plan_given_C(mass_a, mass_b, 2, cost, "exact")
  
  loss <- wasserstein_(tplan$mass, cost, p = 2, tplan$from, tplan$to)
  sink <- wasserstein(A, B, 2, 2, "colwise", "sinkhorn")
  # sinkcost1 <- sink$distances^(1/2)
  # sinkcost2 <- sum(sink$transportmatrix * cost^2)^(1/2)
  # testthat::expect_equivalent(sinkcost1, sinkcost2)
  
  testthat::expect_lt(loss, sink)
})

testthat::test_that("give error when p < 1", {
  set.seed(32857)
  A <- matrix(stats::rnorm(100*124),nrow=124,ncol=100)
  B <- matrix(stats::rnorm(100*124),nrow=124,ncol=100)
  ground_p <- 2
  p <- 0
  
  testthat::expect_error(wasserstein(at,bt, p = p, ground_p = ground_p, "colwise", "hilbert"))
  testthat::expect_error(wasserstein(at,bt, p = p, ground_p = ground_p, "colwise", "rank"))
  testthat::expect_error(wasserstein(at,bt, p = p, ground_p = ground_p, "colwise", "univariate.approximation.pwr"))
  testthat::expect_error(wasserstein(at,bt, p = p, ground_p = ground_p, "colwise", "sinkhorn"))
  testthat::expect_error(wasserstein(at,bt, p = p, ground_p = ground_p, "colwise", "greenkhorn"))
  # testthat::expect_error(wasserstein(at,bt, p = p, ground_p = ground_p, "colwise", "randkhorn"))
  # testthat::expect_error(wasserstein(at,bt, p = p, ground_p = ground_p, "colwise", "gandkhorn"))
})
