testthat::test_that("hilbert.projection works", {
  set.seed(20980)
  
  x <- matrix(rnorm(1000*10),1000,10)
  idx_x <- hilbert.projection(x)
  y <- matrix(rnorm(1000*10),1000,10)
  idx_y <- hilbert.projection(y)
  
  testtplan <- list()
  testtplan$from <- idx_x[order(idx_y)]
  testtplan$to <- 1:1000
  
  hilbert_dist <- sqrt(mean(rowSums((x[idx_x,] - y[idx_y,])^2)))
  hilbert_dist2 <- sqrt(mean(rowSums((x[testtplan$from,] - y[testtplan$to,])^2)))
  hilbert_dist3 <- sqrt(mean(rowSums((x[testtplan$from,] - y)^2)))
  tplan <- approxOT::transport_plan(x,y, p = 2, ground_p = 2, 
                                    observation.orientation = "rowwise",
                                    method = "hilbert")
  wass <- sqrt(mean(rowSums((x[tplan$tplan$from, ] - y[tplan$tplan$to,])^2)))
  testthat::expect_equal(tplan$tplan$from, testtplan$from)
  testthat::expect_equal(tplan$tplan$to, testtplan$to)
  testthat::expect_equal(hilbert_dist, wass)
  testthat::expect_equal(hilbert_dist2, wass)
  testthat::expect_equal(hilbert_dist3, wass)
  
})

testthat::test_that("hilbert.projection 1d wass", {
  set.seed(234234)
  n1 <- 100
  n2 <- 200
  d<- 1
  
  x <- matrix(rnorm(n1*d),n1,d)
  idx_x <- hilbert.projection(x)
  y <- matrix(rnorm(n2*d),n2,d)
  idx_y <- hilbert.projection(y)
  
  testthat::expect_equal(idx_x, order(x))
  testthat::expect_equal(idx_y, order(y))
  
})
