testthat::test_that("wasserstein gives 0 for same distribution", {
  set.seed(11289374)
  n <- 100
  d <- 50
  x <- matrix(rnorm(n*d), nrow=d, ncol=n)
  y <- rnorm(n)
  niter <- 1e2
  # y <- matrix(rnorm(n*d), nrow=d, ncol=n)
  exact <- approxOT::wasserstein(X = x, Y = x, p = 2,
                               ground_p = 2, observation.orientation = "colwise",
                               method = "shortsimplex")
  hilbert <- approxOT::wasserstein(X = x, Y = x, p = 2,
                                          ground_p = 2, observation.orientation = "colwise",
                                          method = "hilbert")
  rank <- approxOT::wasserstein(X = x, Y = x, p = 2,
                                          ground_p = 2, observation.orientation = "colwise",
                                          method = "rank")
  sinkhorn <- approxOT::wasserstein(X = x, Y = x, p = 2,
                                          ground_p = 2, observation.orientation = "colwise",
                                          method = "sinkhorn", niter = niter)
  # sinkhorn2 <- approxOT::wasserstein(X = x, Y = x, p = 2,
  #                                          ground_p = 2, observation.orientation = "colwise",
  #                                          method = "sinkhorn2", niter = niter)
  greenkhorn <- approxOT::wasserstein(X = x, Y = x, p = 2,
                                           ground_p = 2, observation.orientation = "colwise",
                                           method = "greenkhorn", niter = niter)
  # randkhorn <- approxOT::wasserstein(X = x, Y = x, p = 2,
  #                                          ground_p = 2, observation.orientation = "colwise",
  #                                          method = "randkhorn", niter = niter)
  # gandkhorn <- approxOT::wasserstein(X = x, Y = x, p = 2,
  #                                           ground_p = 2, observation.orientation = "colwise",
  #                                           method = "gandkhorn", niter = niter)
  uni.pwr <- approxOT::wasserstein(X = x, Y = x, p = 2,
                                           ground_p = 2, observation.orientation = "colwise",
                                           method = "univariate.approximation.pwr")
  uni.app <- approxOT::wasserstein(X = x, Y = x, p = 2,
                                                      ground_p = 2, observation.orientation = "colwise",
                                                      method = "univariate.approximation")
  testthat::expect_equal(exact, 0)
  testthat::expect_equal(hilbert, 0)
  testthat::expect_lt(sinkhorn, 0.05)
  # testthat::expect_lt(sinkhorn2, 0.05) #compare espen bernton and pierre jacob's funct to one from Greenkhorn paper
  testthat::expect_lt(greenkhorn, 0.06)
  # testthat::expect_lt(randkhorn, 0.05)
  # testthat::expect_lt(gandkhorn, 0.06)
  testthat::expect_equal(uni.pwr, 0)
  testthat::expect_equal(uni.app, 0)
  
  exact.uni <- approxOT::wasserstein(X = y, Y = y, p = 2,
                                        ground_p = 2, observation.orientation = "colwise",
                                        method = "shortsimplex")
  hilbert.uni <- approxOT::wasserstein(X = y, Y = y, p = 2,
                                          ground_p = 2, observation.orientation = "colwise",
                                          method = "hilbert")
  rank.uni <- approxOT::wasserstein(X = y, Y = y, p = 2,
                                              ground_p = 2, observation.orientation = "colwise",
                                              method = "hilbert")
  sinkhorn.uni <- approxOT::wasserstein(X = y, Y = y, p = 2,
                                           ground_p = 2, observation.orientation = "colwise",
                                           method = "sinkhorn", niter = niter, epsilon = 2.4)
  # sinkhorn2.uni <- approxOT::wasserstein(X = y, Y = y, p = 2,
  #                                              ground_p = 2, observation.orientation = "colwise",
  #                                              method = "sinkhorn2", niter = niter)
  testthat::expect_silent(greenkhorn.uni <- approxOT::wasserstein(X = y, Y = y, p = 2,
                                               ground_p = 2, observation.orientation = "colwise",
                                               method = "greenkhorn", niter = niter))
  # randkhorn.uni <- approxOT::wasserstein(X = y, Y = y, p = 2,
  #                                              ground_p = 2, observation.orientation = "colwise",
  #                                              method = "randkhorn", niter = niter)
  # gandkhorn.uni <- approxOT::wasserstein(X = y, Y = y, p = 2,
  #                                              ground_p = 2, observation.orientation = "colwise",
  #                                              method = "gandkhorn", niter = niter)
  
  uni.pwr.uni <- approxOT::wasserstein(X = y, Y = y, p = 2,
                                          ground_p = 2, observation.orientation = "colwise",
                                          method = "univariate.approximation.pwr")
  uni.app.uni <- approxOT::wasserstein(X = y, Y = y, p = 2,
                                          ground_p = 2, observation.orientation = "colwise",
                                          method = "univariate.approximation")
  testthat::expect_equal(exact.uni, 0)
  testthat::expect_equal(hilbert.uni, 0)
  testthat::expect_equal(rank.uni, 0)
  testthat::expect_lt(sinkhorn.uni, 1.4)
  # testthat::expect_lt(sinkhorn2.uni, 0.15)
  testthat::expect_lt(greenkhorn.uni, 0.3)
  # testthat::expect_lt(randkhorn.uni, 0.15)
  # testthat::expect_lt(gandkhorn.uni, 0.5)
  testthat::expect_equal(uni.pwr.uni, 0)
  testthat::expect_equal(uni.app.uni, 0)
})

testthat::test_that("wasserstein matches transport package for shortsimplex", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("transport")
  set.seed(11289374)
  n <- 100
  d <- 50
  x <- matrix(rnorm(n*d), nrow=d, ncol=n)
  y <- matrix(rnorm(n*d), nrow=d, ncol=n)
  z <- rnorm(n)
  w <- rnorm(n)
  # y <- matrix(rnorm(n*d), nrow=d, ncol=n)
  exact <- approxOT::wasserstein(X = x, Y = y, p = 2,
                                        ground_p = 2, observation.orientation = "colwise",
                                        method = "shortsimplex")
  exact.trans <- transport::wasserstein(a = rep(1,n), b = rep(1,n), p = 2, 
                                        tplan = NULL,
                                        costm = cost_calc(x,y, 2),
                                        method = "shortsimplex"
                                        )
  uni <- approxOT::wasserstein(X = z, Y = w, p = 2, ground_p = 2, 
                               observation.orientation = "colwise",
                               method = "univariate")
  uni.trans <- transport::wasserstein1d(a = z, b = w, p = 2)
  
  testthat::expect_equal(exact, exact.trans)
  testthat::expect_equal(uni, uni.trans)
  
  # check for rowwise orientation
  exact.row <- approxOT::wasserstein(X = t(x), Y = t(y), p = 2,
                                        ground_p = 2, observation.orientation = "rowwise",
                                        method = "shortsimplex")
  uni.row <- approxOT::wasserstein(X = t(t(z)), Y = t(t(w)), p = 2, ground_p = 2, 
                                   observation.orientation = "rowwise", 
                                   method = "univariate")

  testthat::expect_equal(exact.row, exact.trans)
  testthat::expect_equal(uni.row, uni.trans)
})

testthat::test_that("wasserstein matches transport package for networkflow", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("transport")
  set.seed(11289374)
  n <- 100
  d <- 50
  x <- matrix(rnorm(n*d), nrow=d, ncol=n)
  y <- matrix(rnorm(n*d), nrow=d, ncol=n)
  z <- rnorm(n)
  w <- rnorm(n)
  # y <- matrix(rnorm(n*d), nrow=d, ncol=n)
  exact <- approxOT::wasserstein(X = x, Y = y, p = 2,
                                 ground_p = 2, observation.orientation = "colwise",
                                 method = "networkflow")
  exact.trans <- transport::wasserstein(a = rep(1,n), b = rep(1,n), p = 2, 
                                        tplan = NULL,
                                        costm = cost_calc(x,y, 2),
                                        method = "networkflow"
  )
  uni <- approxOT::wasserstein(X = z, Y = w, p = 2, ground_p = 2, 
                               observation.orientation = "colwise", 
                               method = "univariate")
  uni.trans <- transport::wasserstein1d(a = z, b = w, 2)
  
  testthat::expect_equal(exact, exact.trans)
  testthat::expect_equal(uni, uni.trans)
  
  # check for rowwise orientation
  exact.row <- approxOT::wasserstein(X = t(x), Y = t(y), p = 2,
                                     ground_p = 2, observation.orientation = "rowwise",
                                     method = "networkflow")
  uni.row <- approxOT::wasserstein(X = t(t(z)), Y = t(t(w)), 
                                   p = 2, ground_p = 2, 
                                   observation.orientation = "rowwise", 
                                   method = "univariate")
  
  testthat::expect_equal(exact.row, exact.trans)
  testthat::expect_equal(uni.row, uni.trans)
})

testthat::test_that("wasserstein matches transport package",{
  testthat::skip_if_not_installed("transport")
  testthat::skip_on_cran()
  set.seed(32857)
  A <- matrix(rnorm(1000*1024),nrow=1024,ncol=1000)
  B <- matrix(rnorm(1000*1024),nrow=1024,ncol=1000)
  at <- t(A)
  bt <- t(B)
  cost <- cost_calc(at,bt,2)
  mass_a <- rep(1/ncol(at),ncol(at))
  mass_b <- rep(1/ncol(bt),ncol(bt))
  
  tplan <- transport_plan_given_C(mass_a, mass_b, 2, cost, "shortsimplex")
  
  loss <- approxOT:::wasserstein_(tplan$mass, cost, p = 2, tplan$from, tplan$to)
  
  loss_t <- transport::wasserstein(transport::pp(A),transport::pp(B),p=2, method = "shortsimplex")
  loss_t_def <- transport::wasserstein(mass_a,mass_b, tplan = data.frame(tplan), costm = cost, p=2, method = "shortsimplex")
  testthat::expect_equivalent(loss, loss_t)
  testthat::expect_equivalent(loss, loss_t_def)
  # microbenchmark::microbenchmark(transport::wasserstein(mass_a,mass_b, tplan = data.frame(tplan), costm = cost, p=2, method = "shortsimplex"), unit="us")
  # microbenchmark::microbenchmark(wasserstein_(tplan$mass, cost, p = 2, tplan$from, tplan$to), unit = "us")
  # microbenchmark::microbenchmark(sinkhorn_(mass_a, mass_b, cost^2, 0.05*median(cost^2), 100), unit="ms")
  
  C <- t(A[1:100,,drop = FALSE])
  D <- t(B[1:2,,drop = FALSE])
  
  cost2 <- cost_calc(C,D,2)
  mass_c <- rep(1/ncol(C),ncol(C))
  mass_d <- rep(1/ncol(D),ncol(D))
  tplan2 <- transport_plan_given_C(mass_c, mass_d, 2, cost2, "shortsimplex")
  loss <- approxOT:::wasserstein_(tplan2$mass, cost2, p = 2, tplan2$from, tplan2$to)
  loss_t_def <- transport::wasserstein(mass_c,mass_d, tplan = data.frame(tplan2), costm = cost2, p=2, method = "shortsimplex")
  testthat::expect_equivalent(loss, loss_t_def)
})

testthat::test_that("make sure wass less than all other transports", {
  set.seed(32857)
  A <- matrix(rnorm(1000*1024),nrow=1024,ncol=1000)
  B <- matrix(rnorm(1000*1024),nrow=1024,ncol=1000)
  at <- t(A)
  bt <- t(B)
  cost <- cost_calc(at,bt,2)
  mass_a <- rep(1/ncol(at),ncol(at))
  mass_b <- rep(1/ncol(bt),ncol(bt))
  
  tplan <- transport_plan_given_C(mass_a, mass_b, p = 2, cost = cost, 
                                  method = "shortsimplex")
  
  loss <- approxOT:::wasserstein_(tplan$mass, cost, p = 2, tplan$from, tplan$to)
  hilbert <- wasserstein(at,bt, p = 2, ground_p = 2, observation.orientation = "colwise", method = "hilbert")
  rank <- wasserstein(at,bt, p = 2, ground_p = 2, observation.orientation = "colwise",  method = "rank")
  uap <- wasserstein(at,bt, p = 2, ground_p = 2, observation.orientation = "colwise",  method = "univariate.approximation.pwr")
  sink <- wasserstein(at,bt, p = 2, ground_p = 2, observation.orientation = "colwise",  method = "sinkhorn")
  sinkl <- wasserstein(at,bt, p = 2, ground_p = 2, observation.orientation = "colwise",  method = "sinkhorn_log")
  grnk <- wasserstein(at,bt, p = 2, ground_p = 2, observation.orientation = "colwise",  method = "greenkhorn")
  # rand <- wasserstein(at,bt, p = 2, ground_p = 2, observation.orientation = "colwise",  method = "randkhorn")
  # gand <- wasserstein(at,bt, p = 2, ground_p = 2, observation.orientation = "colwise",  method = "gandkhorn")
  
  testthat::expect_lt(loss, hilbert)
  testthat::expect_lt(loss, rank)
  testthat::expect_lt(loss, sink)
  testthat::expect_lt(loss, sinkl)
  testthat::expect_lt(loss, grnk)
  # testthat::expect_lt(loss, rand)
  # testthat::expect_lt(loss, gand)
  testthat::expect_gt(loss, uap)
})

testthat::test_that("make sure sinkhorn outputs agree and are more than wass", {
  set.seed(32857)
  A <- matrix(rnorm(1000*1024),nrow=1024,ncol=1000)
  B <- matrix(rnorm(1000*1024),nrow=1024,ncol=1000)
  at <- t(A)
  bt <- t(B)
  cost <- cost_calc(at,bt,2)
  mass_a <- rep(1/ncol(at),ncol(at))
  mass_b <- rep(1/ncol(bt),ncol(bt))
  
  tplan <- transport_plan_given_C(mass_a, mass_b, 2, cost, "exact")
  
  loss <- approxOT:::wasserstein_(tplan$mass, cost_ = cost, p = 2, tplan$from, tplan$to)
  sink <- wasserstein(X = A, Y = B, 
                      a = mass_a, b = mass_b, 
                      p = 2, ground_p = 2, cost = cost,
                      observation.orientation = "colwise", method = "sinkhorn")
  sinklog <- wasserstein(X = A, Y = B, epsilon = 0.05,
                      a = mass_a, b = mass_b, 
                      p = 2, ground_p = 2, cost = cost,
                      observation.orientation = "colwise", method = "sinkhorn_log")
  # sinkcost1 <- sink$distances^(1/2)
  # sinkcost2 <- sum(sink$transportmatrix * cost^2)^(1/2)
  # testthat::expect_equivalent(sinkcost1, sinkcost2)
  
  testthat::expect_lt(loss, sink)
  testthat::expect_lt(loss, sinklog)
})

testthat::test_that("give error when p < 1", {
  set.seed(32857)
  A <- matrix(rnorm(1000*1024),nrow=1024,ncol=1000)
  B <- matrix(rnorm(1000*1024),nrow=1024,ncol=1000)
  ground_p <- 2
  p <- 0
  
  testthat::expect_error(wasserstein(at,bt, p = p, ground_p = ground_p, "colwise", "hilbert"))
  testthat::expect_error(wasserstein(at,bt, p = p, ground_p = ground_p, "colwise", "rank"))
  testthat::expect_error(wasserstein(at,bt, p = p, ground_p = ground_p, "colwise", "univariate.approximation.pwr"))
  testthat::expect_error(wasserstein(at,bt, p = p, ground_p = ground_p, "colwise", "sinkhorn"))
  testthat::expect_error(wasserstein(at,bt, p = p, ground_p = ground_p, "colwise", "sinkhorn_log"))
  testthat::expect_error(wasserstein(at,bt, p = p, ground_p = ground_p, "colwise", "greenkhorn"))
  # testthat::expect_error(wasserstein(at,bt, p = p, ground_p = ground_p, "colwise", "randkhorn"))
  # testthat::expect_error(wasserstein(at,bt, p = p, ground_p = ground_p, "colwise", "gandkhorn"))
})

testthat::test_that("sliced wasserstein correct", {
  set.seed(2342)
  n <- 128
  nsim <- 1e3
  d <- 10
  p <- 1
  x <- matrix(rnorm(n*d), n, d)
  y <- matrix(rnorm(n*d), n, d)
  z <- matrix(rnorm(n*d), n, d)
  
  theta <- matrix(rnorm(nsim*d), d, nsim)
  theta <- sweep(theta, MARGIN = 2, STATS = apply(theta,2,function(x) sqrt(sum(x^2))), FUN = "/")
  
  xp <- x %*% theta
  yp <- y %*% theta
  zp <- z %*% theta
  u <- sort(runif(nsim))
  val2m <- sapply(1:nsim, function(i) {
    a <- quantile(xp[,i], probs = u)
    b <- quantile(yp[,i], probs = u)
    
    return(mean(abs(a - b)^p))
  })
  val2 <- mean(val2m)^(1/p)
  val2.check <- approxOT::wasserstein(X = x, Y = y, p = 1, ground_p = 1, observation.orientation = "rowwise",
                                      method = "sliced", nsim = nsim)
  
  testthat::expect_lt(val2.check - val2, 0.01)
  
})

testthat::test_that("unbiased sinkhorn works", {
  set.seed(11289374)
  n <- 100
  d <- 5
  x <- matrix(rnorm(n*d), nrow=d, ncol=n)
  y <- matrix(rnorm(n*d), nrow=d, ncol=n) + matrix(5, d,n)
  
  
  exact <- approxOT::wasserstein(X = x, Y = y, p = 2,
                                 ground_p = 2, observation.orientation = "colwise",
                                 method = "networkflow")
  sink_xy <- approxOT::wasserstein(X = x, Y = y, p = 2,
                                         ground_p = 2, observation.orientation = "colwise",
                                         method = "sinkhorn")
  
  sink_xx <- approxOT::wasserstein(X = x, Y = x, p = 2,
                                   ground_p = 2, observation.orientation = "colwise",
                                   method = "sinkhorn")
  
  sink_yy <- approxOT::wasserstein(X = y, Y = y, p = 2,
                                   ground_p = 2, observation.orientation = "colwise",
                                   method = "sinkhorn")
  
  sink_ue <- sqrt(sink_xy^2 - 0.5 * (sink_xx^2 + sink_yy^2))
  
  sink_u <- approxOT::wasserstein(X = x, Y = y, p = 2,
                                   ground_p = 2, observation.orientation = "colwise",
                                   method = "sinkhorn",
                                   unbiased = TRUE)
  sink_ux <- approxOT::wasserstein(X = x, Y = x, p = 2,
                                   ground_p = 2, observation.orientation = "colwise",
                                   method = "sinkhorn",
                                   unbiased = TRUE)
  sink_uy <- approxOT::wasserstein(X = y, Y = y, p = 2,
                                   ground_p = 2, observation.orientation = "colwise",
                                   method = "sinkhorn",
                                   unbiased = TRUE)
  # c(exact = exact, sinkhorn = sink_xy, sink_ue = sink_ue, sink_u = sink_u,
    # sink_ux = sink_ux, sink_uy = sink_uy)
  testthat::expect_lte(exact, sink_xy)
  testthat::expect_lte(exact, sink_ue)
  testthat::expect_gte(exact, sink_u)
  
  testthat::expect_equal(0, sink_ux)
  testthat::expect_equal(0, sink_uy)
})

testthat::test_that("sinkhorn_log works for all lambda",{
  set.seed(32857)
  A <- matrix(rnorm(1000*1024),nrow=1024,ncol=1000)
  B <- matrix(rnorm(1000*1024),nrow=1024,ncol=1000)
  at <- t(A)
  bt <- t(B)
  cost <- cost_calc(at,bt,2)
  mass_a <- rep(1/ncol(at),ncol(at))
  mass_b <- rep(1/ncol(bt),ncol(bt))
  
  tplan <- transport_plan_given_C(mass_a, mass_b, 2, cost, "exact")
  
  loss <- approxOT:::wasserstein_(tplan$mass, cost_ = cost, p = 2, tplan$from, tplan$to)
  testthat::expect_warning(sink <- wasserstein(X = A, Y = B, epsilon = 1e-5,
                      a = mass_a, b = mass_b, niter = 2,
                      p = 2, ground_p = 2, cost = cost,
                      observation.orientation = "colwise", method = "sinkhorn"))
  sinklog <- wasserstein(X = A, Y = B, epsilon = 1e-5,
                         a = mass_a, b = mass_b, 
                         p = 2, ground_p = 2, cost = cost,
                         niter = 2,
                         observation.orientation = "colwise", method = "sinkhorn_log")
  testthat::expect_false(is.nan(sink))
  testthat::expect_false(is.nan(sinklog))
  testthat::expect_equal(sink, sinklog)
  
  sink <- wasserstein(X = A, Y = B, epsilon = 100000,
                      a = mass_a, b = mass_b, niter = 100,
                      p = 2, ground_p = 2, cost = cost,
                      observation.orientation = "colwise", method = "sinkhorn")
  sinklog <- wasserstein(X = A, Y = B, epsilon = 100000,
                         a = mass_a, b = mass_b, 
                         p = 2, ground_p = 2, cost = cost,
                         niter = 100,
                         observation.orientation = "colwise", method = "sinkhorn_log")
  testthat::expect_false(is.nan(sink))
  testthat::expect_false(is.nan(sinklog))
  testthat::expect_equal(sink,sinklog)
  
})