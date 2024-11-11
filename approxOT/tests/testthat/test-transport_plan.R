testthat::test_that("transport univariate works", {
  set.seed(12308947)
  n <- 100
  x <- sort(rnorm(n))
  new.ord <- sample.int(n,n,FALSE)
  y <- x[new.ord]
  order.y <- order(y)
  
  trans <- transport_plan(x, y, p = 2, ground_p = 2, 
                          observation.orientation = "colwise", 
                          method = "univariate")
  trans.sort <- transport_plan(x, y, p = 2, ground_p = 2, 
                               observation.orientation = "colwise", 
                               method = "univariate", is.A.sorted = TRUE)
  trans.row <- transport_plan(t(t(x)), t(t(y)), p = 2, ground_p = 2, 
                              observation.orientation = "rowwise", 
                              method = "univariate")
  trans.sort.row <- transport_plan(t(t(x)), t(t(y)), p = 2, ground_p = 2, 
                                   observation.orientation = "rowwise", 
                                   method = "univariate", is.A.sorted = TRUE)
  
  testthat::expect_equal(trans$tplan$to, 1:n)
  testthat::expect_equal(trans$tplan$from, new.ord)
  testthat::expect_equal(trans$cost,0)
  testthat::expect_equal(trans.sort$tplan$to, 1:n)
  testthat::expect_equal(trans.sort$tplan$from, new.ord)
  testthat::expect_equal(trans.sort$cost, 0)
  testthat::expect_equal(trans.row$tplan$to, 1:n)
  testthat::expect_equal(trans.row$tplan$from, new.ord)
  testthat::expect_equal(trans.row$cost,0)
  testthat::expect_equal(trans.sort.row$tplan$to, 1:n)
  testthat::expect_equal(trans.sort.row$tplan$from, new.ord)
  testthat::expect_equal(trans.sort.row$cost,0)
})

testthat::test_that("transport hilbert works", {
  set.seed(203987)
  n <- 100
  d <- 10
  x <- matrix(rnorm(d*n), nrow=d, ncol=n)
  y <- matrix(rnorm(d*n), nrow=d, ncol=n)
  
  #get hilbert sort orders for x in backwards way
  transx <- transport_plan(X=x, Y=x, ground_p = 2, p = 2, 
                           observation.orientation =  "colwise", 
                           method = "hilbert", is.X.sorted = TRUE)
  
  #get hilbert sort orders for y in backwards way
  transy <- transport_plan(X=y, Y=y, p = 2, ground_p = 2, 
                           observation.orientation =  "colwise", 
                           method = "hilbert", is.X.sorted = TRUE)
  
  
  xsort <- x[,order(transx$tplan$from)]
  ysort <- y[,order(transy$tplan$from)]
  
  
  #sort y on unsorted x
  trans <- transport_plan(X=x, Y=y,  ground_p = 2, 
                          observation.orientation =  "colwise", 
                          method = "hilbert")
  
  #sort y on sorted x
  trans.sortx <- transport_plan(X=xsort, Y=y,  ground_p = 2, 
                          observation.orientation =  "colwise", 
                          method = "hilbert", is.X.sorted = TRUE)
  
  #check on sorted y
  trans.sorty <- transport_plan(X=x, Y=ysort,  ground_p = 2, 
                                  observation.orientation =  "colwise", 
                                  method = "hilbert", is.X.sorted = FALSE)
  #check on sorted y
  trans.sortxony <- transport_plan(X=ysort, Y=x,  ground_p = 2, 
                                observation.orientation =  "colwise", 
                                method = "hilbert")
  
  #check on sorted y and x does nothing
  trans.nothing <- transport_plan(X=xsort, Y=ysort,  ground_p = 2, 
                          observation.orientation =  "colwise", 
                          method = "hilbert", is.X.sorted = TRUE)
  
  testthat::expect_equal(trans$tplan$to[transx$tplan$to], transy$tplan$to)
  testthat::expect_equal(trans$tplan$to[transx$tplan$from], transx$tplan$from) #unnecessary
  
  testthat::expect_equal(trans.nothing$tplan$from, trans.nothing$tplan$to)
  testthat::expect_equal(1:n, trans.nothing$tplan$to)
  testthat::expect_equal(trans.nothing$tplan$from, 1:n)
  
  testthat::expect_equal(trans.sorty$tplan$from, order(transx$tplan$from))
  testthat::expect_equal(trans.sorty$tplan$to, 1:n)
  
  testthat::expect_equal(trans.sortx$tplan$from, transy$tplan$from)
  testthat::expect_equal(transx$tplan$to, trans.sortxony$tplan$to)
  
  
  # make sure all costs are equal
  testthat::expect_equal(trans$cost, trans.sortx$cost)
  testthat::expect_equal(trans$cost,trans.sorty$cost)
  testthat::expect_equal(trans$cost,trans.nothing$cost)
})

testthat::test_that("transport rank works", {
  set.seed(19380)
  n <- 1000
  d <- 500
  corr <- matrix(0.5, nrow=d, ncol=d)
  diag(corr) <- 1
  x <- t(chol(corr)) %*% matrix(rnorm(d*n), nrow=d, ncol=n)
  y <- t(chol(corr)) %*% matrix(rnorm(d*n), nrow=d, ncol=n)
  method <- "rank"
  
  
  # try R ranks
  x_ranks <- apply(x, 1, rank)
  y_ranks <- apply(y, 1, rank)
  
  x_idx <- order(apply(x_ranks,1,mean))
  y_idx <- order(apply(y_ranks,1,mean))
  
  Rxsort <- x[, x_idx]
  Rysort <- y[, y_idx]
  
  
  #get rank sort orders for x in backwards way
  transx <- transport_plan(X=x, Y=Rxsort, ground_p = 2, p = 2, 
                           observation.orientation =  "colwise",
                           method = method, is.X.sorted = FALSE)
  
  #get rank sort orders for y in backwards way
  transy <- transport_plan(X=y, Y=Rysort, p = 2, ground_p = 2, 
                           observation.orientation =  "colwise", 
                           method = method, is.X.sorted = FALSE)
  
  
  xsort <- x[ , transx$tplan$from]
  ysort <- y[ , transy$tplan$from]
  
  
  #sort y on unsorted x
  trans <- transport_plan(X=x, Y=y,  ground_p = 2, 
                          observation.orientation =  "colwise", 
                          method = method)
  
  #sort y on sorted x
  trans.sortx <- transport_plan(X=xsort, Y=y,  ground_p = 2, 
                                observation.orientation =  "colwise", 
                                method = method, is.X.sorted = TRUE)
  
  #check on sorted y
  trans.sorty <- transport_plan(X=x, Y=ysort,  ground_p = 2, 
                                observation.orientation =  "colwise", 
                                method = method, is.X.sorted = FALSE)
  #check on sorted y
  trans.sortxony <- transport_plan(X=ysort, Y=x,  ground_p = 2, 
                                   observation.orientation =  "colwise", 
                                   method = method)
  
  #check on sorted y and x does nothing
  trans.nothing <- transport_plan(X=xsort, Y=ysort,  ground_p = 2, 
                                  observation.orientation =  "colwise", 
                                  method = method, is.X.sorted = TRUE)
  
  # compare C order to R order # order of two obs reversed in each on some platforms
  # need to examine further but not huge issue
  # testthat::expect_equal(x_idx, transx$tplan$from)
  # testthat::expect_equal(y_idx, transy$tplan$from)
  
  # test to see if sort of x matches y
  testthat::expect_equal(trans$tplan$from[transy$tplan$from], transx$tplan$from)
  
  testthat::expect_equal(trans.sorty$tplan$from, transx$tplan$from) #unnecessary
  testthat::expect_equal(order(trans.sortx$tplan$from), transy$tplan$from) #unnecessary
  
  #make sure get return of 1:n vector
  testthat::expect_equal(trans.nothing$tplan$from, trans.nothing$tplan$to)
  testthat::expect_equal(1:n, trans.nothing$tplan$to)
  testthat::expect_equal(trans.nothing$tplan$from, 1:n)
  
  #make sure pre-sorted x returns 1:n
  testthat::expect_equal(trans.sortx$tplan$to, transy$tplan$to)
  testthat::expect_equal(transx$tplan$to, trans.sortxony$tplan$to)
  
  # make sure all costs are null
  # testthat::expect_null(trans$cost)
  # testthat::expect_null(trans.sortx$cost)
  # testthat::expect_null(trans.sorty$cost)
  # testthat::expect_null(transx$cost)
  # testthat::expect_null(transy$cost)
  # testthat::expect_null(trans.nothing$cost)
})

testthat::test_that("transport univariate.approx.pwr works", {
  set.seed(19380)
  n <- 1000
  d <- 500
  corr <- matrix(0.5, nrow=d, ncol=d)
  diag(corr) <- 1
  x <- t(chol(corr)) %*% matrix(rnorm(d*n), nrow=d, ncol=n)
  y <- t(chol(corr)) %*% matrix(rnorm(d*n), nrow=d, ncol=n)
  method <- "univariate.approximation.pwr"
  
  
  # try R order
  temp_x_idx <- t(apply(x,1,order))
  temp_y_idx <- t(apply(y,1,order))
  
  Rxsort <- t(sapply(1:d, function(i) x[i, temp_x_idx[i,]]))
  Rysort <- t(sapply(1:d, function(i) y[i, temp_y_idx[i,]]))
  
  Rcost <- mean((Rxsort - Rysort)^2)
  
  tot_idx <- matrix(1:(n*d),d,n)
  x_idx <- c(t(sapply(1:d, function(i) tot_idx[i, temp_x_idx[i,]])))
  y_idx <- c(t(sapply(1:d, function(i) tot_idx[i, temp_y_idx[i,]])))
  
  #get rank sort orders for x in backwards way
  transx <- transport_plan(X=x, Y=Rxsort, ground_p = 2, p = 2, 
                           observation.orientation =  "colwise",
                           method = method, is.X.sorted = FALSE)
  
  #get rank sort orders for y in backwards way
  transy <- transport_plan(X=y, Y=Rysort, p = 2, ground_p = 2, 
                           observation.orientation =  "colwise", 
                           method = method, is.X.sorted = FALSE)
  
  
  xsort <- matrix(x[transx$tplan$from],d,n)
  ysort <- matrix(y[transy$tplan$from],d,n)
  
  
  #sort y on unsorted x
  trans <- transport_plan(X=x, Y=y,  ground_p = 2, 
                          observation.orientation =  "colwise", 
                          method = method)
  
  #sort y on sorted x
  trans.sortx <- transport_plan(X=xsort, Y=y,  ground_p = 2, 
                                observation.orientation =  "colwise", 
                                method = method, is.X.sorted = TRUE)
  
  #check on sorted y
  trans.sorty <- transport_plan(X=x, Y=ysort,  ground_p = 2, 
                                observation.orientation =  "colwise", 
                                method = method, is.X.sorted = FALSE)
  #check on sorted y
  trans.sortxony <- transport_plan(X=ysort, Y=x,  ground_p = 2, 
                                   observation.orientation =  "colwise", 
                                   method = method)
  
  #check on sorted y and x does nothing
  trans.nothing <- transport_plan(X=xsort, Y=ysort,  ground_p = 2, 
                                  observation.orientation =  "colwise", 
                                  method = method, is.X.sorted = TRUE)
  
  # compare C order to R order
  testthat::expect_equal(x_idx, transx$tplan$from)
  testthat::expect_equal(y_idx, transy$tplan$from)
  
  # test to see if sort of x matches y
  testthat::expect_equal(trans$tplan$to[transx$tplan$to], transy$tplan$to)
  
  # see if recover original orders
  # testthat::expect_equal(trans.sortx$tplan$to[transx$tplan$from], transx$tplan$from) #unnecessary
  
  #make sure get return of 1:n vector
  testthat::expect_equal(trans.nothing$tplan$from, trans.nothing$tplan$to)
  testthat::expect_equal(1:(n*d), trans.nothing$tplan$to)
  testthat::expect_equal(trans.nothing$tplan$from, 1:(n*d))
  
  #make sure pre-sorted y returns 1:n
  testthat::expect_equal(transx$tplan$from, trans.sorty$tplan$from)
  testthat::expect_equal(trans.sorty$tplan$to, 1:(n*d))
  
  #make sure pre-sorted x returns 1:n
  testthat::expect_equal(trans.sortx$tplan$to, transy$tplan$to)
  testthat::expect_equal(transx$tplan$to, trans.sortxony$tplan$to)
  
  #see if sorted matrices are same
  testthat::expect_equal(ysort,Rysort)
  testthat::expect_equal(xsort,Rxsort)
  
  
  # make sure all costs agree
  testthat::expect_equal(trans$cost, Rcost)
})

testthat::test_that("transport_plan picks up errors", {
  n <- 100
  d <- 10
  x <- matrix(rnorm(d*n), nrow=d, ncol=n)
  y <- matrix(rnorm(d*n), nrow=d, ncol=n)
  
  testthat::expect_error(transport_plan(x=y, x=x, ground_p = 2, p=2, 
                                        observation.orientation = "colwise", method = "univariate"))
  testthat::expect_error(transport_plan(X=y, x=x, ground_p = 2, p=2, 
                                        observation.orientation = "colwise", method = "univariate"))
})

testthat::test_that("transport agrees with transport package shortsimplex", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("transport")
  set.seed(293897)
  A <- matrix(rnorm(100*256),nrow=256,ncol=100)
  B <- matrix(rnorm(100*256),nrow=256,ncol=100)
  # dist_mat <- as.matrix(dist(rbind(A,B)))[1:1024, 1025:2048]
  # dist_mat <- dist_mat^2
  # dist_check <- matrix(0,1024,1024)
  at <- t(A)
  bt <- t(B)
  # for(i in 1:1024) for(j in 1:1024) dist_check[i,j] <- sum((at[,i] - bt[,j])^2)
  # all.equal(c(dist_mat), c(dist_check))
  indexes <- approxOT:::transport_(A_ = at, B_ = bt, p = 2.0, 
                                   ground_p = 2.0, 
                                   method_ = "shortsimplex",
                                   a_sort = FALSE)
  # debugonce(transport::transport.pp)
  index_trans <- transport::transport(transport::pp(A),transport::pp(B),p=2, method = "shortsimplex")
  testthat::expect_equal(indexes$from, index_trans[["from"]])
  testthat::expect_equal(indexes$to, index_trans[["to"]])
  testthat::expect_equal(indexes$mass, index_trans[["mass"]]/256)
  
  mass_a <- rep(1/ncol(at), ncol(at))
  mass_b <- rep(1/ncol(bt), ncol(bt))
  costm <- cost_calc(at,bt,2)
  indexes2 <- approxOT:::transport_C_(mass_a, mass_b, costm^2, "shortsimplex", epsilon_ = 0.05, 
                                      niter_=100, unbiased_ = FALSE, threads_ = 1,
                                      matrix(0,0,0), matrix(0,0,0))
  # check_sink <- sinkhorn_(mass_a, mass_b, costm^2, 0.05*median(costm^2), 100)
  # sum(check_sink$transportmatrix * costm^2)
  testthat::expect_equal(indexes2$from, index_trans[["from"]])
  testthat::expect_equal(indexes2$to, index_trans[["to"]])
  testthat::expect_equal(indexes2$mass, index_trans[["mass"]]/256)
  
  C <- t(A[1:100,,drop = FALSE])
  D <- t(B[1:2,,drop = FALSE])
  
  costm <- cost_calc(C,D,2.0)
  mass_c <- rep(1/ncol(C), ncol(C))
  mass_d <- rep(1/ncol(D), ncol(D))
  
  trans_sp <- approxOT:::transport_C_(mass_c, mass_d, costm^2, method_ = "shortsimplex", 
                                      epsilon_ = 0.05, niter_ = 100L,
                                      unbiased_ = FALSE, threads_ = 1,
                                      matrix(0,0,0), matrix(0,0,0))
  # debugonce(transport::transport.default)
  trans_t <- transport::transport.default(a=mass_c, b=mass_d, costm=costm^2, method = "shortsimplex")
  testthat::expect_equal(trans_sp$from, trans_t$from)
  testthat::expect_equal(trans_sp$to, trans_t$to)
  testthat::expect_equal(trans_sp$mass, trans_t$mass)
  # microbenchmark::microbenchmark(transport::transport.default(a=mass_c, b=mass_d, costm=costm^2, method = "shortsimplex"), unit="us")
  # microbenchmark::microbenchmark(transport_C_(mass_c, mass_d, costm^2, method = "shortsimplex"), unit = "us")
  
  trans_t <- transport::transport.default(a=mass_d, b=mass_c, costm=t(costm^2), method = "shortsimplex")
  trans_sp <- approxOT:::transport_C_(mass_d, mass_c, t(costm^2), method = "shortsimplex", epsilon = 0.05, niter=100,
                                                unbiased_ = FALSE,
                           threads_ = 1,
                           matrix(0,0,0), matrix(0,0,0))
  testthat::expect_equal(trans_sp$from, trans_t$from)
  testthat::expect_equal(trans_sp$to, trans_t$to)
  testthat::expect_equal(trans_sp$mass, trans_t$mass)
})

testthat::test_that("transport agrees with transport package networkflow", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("transport")
  set.seed(293897)
  A <- matrix(rnorm(100*256),nrow=256,ncol=100)
  B <- matrix(rnorm(100*256),nrow=256,ncol=100)
  # dist_mat <- as.matrix(dist(rbind(A,B)))[1:1024, 1025:2048]
  # dist_mat <- dist_mat^2
  # dist_check <- matrix(0,1024,1024)
  at <- t(A)
  bt <- t(B)
  # for(i in 1:1024) for(j in 1:1024) dist_check[i,j] <- sum((at[,i] - bt[,j])^2)
  # all.equal(c(dist_mat), c(dist_check))
  indexes <- approxOT:::transport_(at, bt, 2.0, 2.0, 
                                   "networkflow",FALSE)
  #reorder for transport package comparison
  ords <- order(indexes$from)
  indexes <- lapply(indexes, function(i) i[ords])
  
  # debugonce(transport::transport.pp)
  index_trans <- transport::transport(transport::pp(A),transport::pp(B),p=2, method = "networkflow")
  testthat::expect_equal(indexes$from, index_trans[["from"]])
  testthat::expect_equal(indexes$to, index_trans[["to"]])
  testthat::expect_equal(indexes$mass, index_trans[["mass"]]/256)
  
  mass_a <- rep(1/ncol(at), ncol(at))
  mass_b <- rep(1/ncol(bt), ncol(bt))
  costm <- cost_calc(at,bt,2)
  indexes2 <- approxOT:::transport_C_(mass_a, mass_b, costm^2, "networkflow", epsilon_ = 0.05, niter_=256^2, 
                                      unbiased_ = FALSE, threads_ = 1,
                                      matrix(0,0,0), matrix(0,0,0))
  ords2 <- order(indexes2$from)
  indexes2 <- lapply(indexes2, function(i) i[ords2])
  # check_sink <- sinkhorn_(mass_a, mass_b, costm^2, 0.05*median(costm^2), 100)
  # sum(check_sink$transportmatrix * costm^2)
  testthat::expect_equal(indexes2$from, index_trans[["from"]])
  testthat::expect_equal(indexes2$to, index_trans[["to"]])
  testthat::expect_equal(indexes2$mass, index_trans[["mass"]]/256)
  
  
  mass_a <- rep(1/ncol(at), ncol(at))
  mass_b <- rep(1/ncol(bt), ncol(bt))
  costm <- cost_calc(at,bt,2)
  indexes2 <- approxOT:::transport_C_(mass_a, mass_b, costm^2, "networkflow", epsilon_ = 0.05, 
                                      niter_=0, unbiased_ = FALSE, threads_ = 1,
                                      matrix(0,0,0), matrix(0,0,0))
  ords2 <- order(indexes2$from)
  indexes2 <- lapply(indexes2, function(i) i[ords2])
  # check_sink <- sinkhorn_(mass_a, mass_b, costm^2, 0.05*median(costm^2), 100)
  # sum(check_sink$transportmatrix * costm^2)
  testthat::expect_equal(indexes2$from, index_trans[["from"]])
  testthat::expect_equal(indexes2$to, index_trans[["to"]])
  testthat::expect_equal(indexes2$mass, index_trans[["mass"]]/256)
  
  mass_a <- rep(1/ncol(at), ncol(at))
  mass_b <- rep(1/ncol(bt), ncol(bt))
  costm <- cost_calc(at,bt,2)
  testthat::expect_warning(indexes2 <- approxOT:::transport_C_(mass_a, mass_b, costm^2, "networkflow", 
                                                               epsilon_ = 0.05, niter_ = 10, unbiased_ = FALSE, threads_ = 1,
                                                               matrix(0,0,0), matrix(0,0,0)))
  ords2 <- order(indexes2$from)
  indexes2 <- lapply(indexes2, function(i) i[ords2])
  # check_sink <- sinkhorn_(mass_a, mass_b, costm^2, 0.05*median(costm^2), 100)
  # sum(check_sink$transportmatrix * costm^2)
  testthat::expect_lt(length(indexes2$from), 256, label = "check that iterations works")
  testthat::expect_lt(length(indexes2$to), 256, label = "check that iterations works")
  testthat::expect_lt(length(indexes2$mass), 256, label = "check that iterations works")
  testthat::expect_lt(sum(indexes2$mass), 1, label = "check that iterations works")
  
  C <- t(A[1:100,,drop = FALSE])
  D <- t(B[1:2,,drop = FALSE])
  
  costm <- cost_calc(C,D,2.0)
  mass_c <- rep(1/ncol(C), ncol(C))
  mass_d <- rep(1/ncol(D), ncol(D))
  
  trans_sp <- approxOT:::transport_C_(mass_c, mass_d, costm^2, method_ = "networkflow", 
                                      epsilon_ = 0.05, niter_ = 0,
                                      unbiased_ = FALSE,
                                      threads_ = 1, 
                           cost_matrix_A_ = matrix(0,0,0), 
                           cost_matrix_B_ = matrix(0,0,0))
  ords3 <- order(trans_sp$from)
  trans_sp <- lapply(trans_sp, function(i) i[ords3])
  # debugonce(transport::transport.default)
  trans_t <- transport::transport.default(a = mass_c, b = mass_d, costm = costm^2, method = "networkflow")
  testthat::expect_equal(trans_sp$from, trans_t$from)
  testthat::expect_equal(trans_sp$to, trans_t$to)
  testthat::expect_equal(trans_sp$mass, trans_t$mass)
  # microbenchmark::microbenchmark(transport::transport.default(a=mass_c, b=mass_d, costm=costm^2, method = "shortsimplex"), unit="us")
  # microbenchmark::microbenchmark(transport_C_(mass_c, mass_d, costm^2, method = "shortsimplex"), unit = "us")
  
  trans_t <- transport::transport.default(a=mass_d, b=mass_c, costm=t(costm^2), method = "networkflow")
  trans_sp <- approxOT:::transport_C_(mass_d, mass_c, t(costm^2), 
                                      method = "networkflow", epsilon = 0.05, niter=0,
                           unbiased_ = FALSE,
                           threads_ = 1,
                           cost_matrix_A_ = matrix(0,0,0), 
                           cost_matrix_B_ = matrix(0,0,0))
  ords4 <- order(trans_sp$from)
  trans_sp <- lapply(trans_sp, function(i) i[ords4])
  testthat::expect_equal(trans_sp$from, trans_t$from)
  testthat::expect_equal(trans_sp$to, trans_t$to)
  testthat::expect_equal(trans_sp$mass, trans_t$mass)
  
  trans_sp <- approxOT:::transport_C_(mass_d, mass_c, t(costm^2), method = "exact", epsilon = 0.05, niter=0,
                                      unbiased_ = FALSE,
                           threads_ = 1, 
                           cost_matrix_A_ = matrix(0,0,0), 
                           cost_matrix_B_ = matrix(0,0,0))
  ords4 <- order(trans_sp$from)
  trans_sp <- lapply(trans_sp, function(i) i[ords4])
  testthat::expect_equal(trans_sp$from, trans_t$from)
  testthat::expect_equal(trans_sp$to, trans_t$to)
  testthat::expect_equal(trans_sp$mass, trans_t$mass)
})

testthat::test_that("sinkhorn works", {
  set.seed(12308947)
  n <- 32
  d <- 5
  set.seed(293897)
  A <- matrix(rnorm(n*d),nrow=d,ncol=n)
  B <- matrix(rnorm(n*d),nrow=d,ncol=n)
  transp.meth <- "sinkhorn"
  niter = 1e2
  
  trans <- transport_plan(A, B, 
                          ground_p = 2, p = 2, 
                          observation.orientation = "colwise", 
                          method = transp.meth, niter = niter)
  trans.row <- transport_plan(t(A), t(B), 
                              ground_p = 2, p = 2, 
                              obsrevation.orientation = "rowwise", 
                              method = transp.meth, niter = niter)
  # transtest <- transport_plan_given_C(rep(1/n,n), rep(1/n,n),  2, cost = cost_calc(A,B,2), "sinkhorn2", niter = niter)
  # 
  testthat::expect_true((1/n) %in% tapply(trans$tplan$mass, trans$tplan$to, sum))
  testthat::expect_true((1/n) %in% tapply(trans.row$tplan$mass, trans.row$tplan$to, sum))
  # testthat::expect_lte(sum((transtest$mass-trans$tplan$mass)^2), 1e-5)
  testthat::expect_error(transtest <- transport_plan_given_C(rep(1/n,n), rep(1/n,n),  2, cost = cost_calc(A,B,2), "sinkhorn2", niter = niter))
  
})

testthat::test_that("sinkhorn_log works", {
  set.seed(12308947)
  n <- 32
  d <- 5
  set.seed(293897)
  A <- matrix(rnorm(n*d),nrow=d,ncol=n)
  B <- matrix(rnorm(n*d),nrow=d,ncol=n)
  transp.meth <- "sinkhorn_log"
  niter = 1e2
  
  trans <- transport_plan(A, B, 
                          ground_p = 2, p = 2, 
                          observation.orientation = "colwise", 
                          method = transp.meth, niter = niter)
  trans.row <- transport_plan(t(A), t(B), 
                              ground_p = 2, p = 2, 
                              obsrevation.orientation = "rowwise", 
                              method = transp.meth, niter = niter)
  transtest <- transport_plan_given_C(rep(1/n,n), rep(1/n,n),  2, cost = cost_calc(A,B,2), "sinkhorn", niter = niter)
  
  testthat::expect_true((1/n) %in% tapply(trans$tplan$mass, trans$tplan$to, sum))
  testthat::expect_true((1/n) %in% tapply(trans.row$tplan$mass, trans.row$tplan$to, sum))
  testthat::expect_lte(sum((transtest$mass-trans$tplan$mass)^2), 1e-5)
  
})

testthat::test_that("greenkhorn works", {
  set.seed(12308947)
  n <- 32
  d <- 5
  set.seed(293897)
  A <- matrix(rnorm(n*d),nrow=d,ncol=n)
  B <- matrix(rnorm(n*d),nrow=d,ncol=n)
  transp.meth <- "greenkhorn"
  niter = 1e2
  
  trans <- transport_plan(A, B, p = 2, ground_p = 2, 
                          observation.orientation = "colwise", 
                          method = transp.meth, niter = niter)
  trans.row <- transport_plan(t(A), t(B), 
                              ground_p = 2, 
                              p = 2, 
                              observation.orientation = "rowwise", 
                              method = transp.meth, niter = niter)
  transtest <- transport_plan_given_C(rep(1/n,n), rep(1/n,n),  2, cost = cost_calc(A,B,2), "sinkhorn", niter = niter)
  
  testthat::expect_true((1/n) %in% tapply(trans$tplan$mass, trans$tplan$to, sum))
  testthat::expect_true((1/n) %in% tapply(trans.row$tplan$mass, trans.row$tplan$to, sum))
  testthat::expect_lte(sum((transtest$mass-trans$tplan$mass)^2), 1e-3)
  
})

# testthat::test_that("randkhorn works", {
#   testthat::skip("Not implemented")
#   set.seed(12308947)
#   n <- 32
#   d <- 5
#   set.seed(293897)
#   A <- matrix(rnorm(n*d),nrow=d,ncol=n)
#   B <- matrix(rnorm(n*d),nrow=d,ncol=n)
#   transp.meth <- "randkhorn"
#   niter = 1e2
#   
#   trans <- transport_plan(A, B, 
#                           p = 2, 
#                           ground_p = 2, 
#                           observation.orientation = "colwise", 
#                           method = transp.meth, niter = niter)
#   trans.row <- transport_plan(t(A), t(B), 
#                               p = 2, ground_p = 2, 
#                               observation.orientation = "rowwise",
#                               method = transp.meth, niter = niter)
#   transtest <- transport_plan_given_C(rep(1/n,n), rep(1/n,n),  p = 2, cost = cost_calc(A,B,2), method = "sinkhorn", niter = niter)
#   
#   testthat::expect_true((1/n) %in% tapply(trans$tplan$mass, trans$tplan$to, sum))
#   testthat::expect_true((1/n) %in% tapply(trans.row$tplan$mass, trans.row$tplan$to, sum))
#   testthat::expect_lte(sum((transtest$mass-trans$tplan$mass)^2), 1e-6)
#   
# })

# testthat::test_that("gandkhorn works", {
#   testthat::skip("Not implemented")
#   set.seed(12308947)
#   n <- 32
#   d <- 5
#   set.seed(293897)
#   A <- matrix(rnorm(n*d),nrow=d,ncol=n)
#   B <- matrix(rnorm(n*d),nrow=d,ncol=n)
#   transp.meth <- "gandkhorn"
#   niter = 1e2
#   
#   trans <- transport_plan(A, B, 
#                           p = 2, ground_p = 2,
#                           observation.orientation = "colwise", 
#                           method = transp.meth, niter = niter)
#   trans.row <- transport_plan(t(A), t(B), 
#                               p = 2, ground_p = 2, 
#                               observation.orientation = "rowwise", 
#                               method = transp.meth, niter = niter)
#   transtest <- transport_plan_given_C(rep(1/n,n), rep(1/n,n),  p = 2, 
#                                       cost = cost_calc(A,B,2), method = "greenkhorn", niter = niter)
# 
#   testthat::expect_true((1/n) %in% tapply(trans$tplan$mass, trans$tplan$to, sum))
#   testthat::expect_true((1/n) %in% tapply(trans.row$tplan$mass, trans.row$tplan$to, sum))
#   testthat::expect_lte(sum((transtest$mass-trans$tplan$mass)^2), 3e-3)
# 
# })

testthat::test_that("transport swapping works", {
  set.seed(203987)
  n <- 100
  d <- 10
  x <- matrix(rnorm(d*n), nrow=d, ncol=n)
  y <- matrix(rnorm(d*n), nrow=d, ncol=n)
  method <- "swapping"
  
  #swap distance
  trans <- transport_plan(X=x, Y=y,  ground_p = 2,  p = 2,
                          observation.orientation =  "colwise", 
                          method = method)
  
  #check on self. should get no rearrangement
  trans.onx <- transport_plan(X=x, Y=x,  ground_p = 2, 
                                observation.orientation =  "colwise", 
                                method = method)
  #check on sorted y
  trans.ony <- transport_plan(X=y, Y=y,  ground_p = 2, 
                                   observation.orientation =  "colwise", 
                                   method = method)
  
  
  testthat::expect_equal(trans.ony$tplan$from, trans.ony$tplan$to)
  testthat::expect_equal(trans.onx$tplan$from, trans.onx$tplan$to)
  testthat::expect_equal(trans.onx$cost, trans.onx$cost)
})
