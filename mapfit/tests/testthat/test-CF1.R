test_that("density", {
  library(Matrix)
  tt <- seq(0, 10, length.out=10)
  alpha <- c(0.6, 0.4)
  rate <- c(3, 4)

  p <- cf1(alpha=alpha, rate=rate)
  f <- function(t, alpha, xi, Q) {
    (alpha %*% Matrix::expm(Q*t) %*% xi)[1]
  }
  
  result <- dphase(tt, p)
  expected <- sapply(tt, f, alpha=p$alpha(), Q=p$Q(), xi=p$xi())
  expect_equal(result, expected)
})

test_that("estimate_point", {
  RNGkind(kind = "Mersenne-Twister")
  set.seed(1234)
  wsample <- rweibull(100, shape=2, scale=1)
  tres <- system.time(result <- phfit.point(ph=cf1(5), x=wsample))
  print(result)
  print(tres)
})

test_that("estimate_group", {
  RNGkind(kind = "Mersenne-Twister")
  set.seed(1234)
  wsample <- rweibull(100, shape=2, scale=1)
  h.res <- hist(wsample, breaks="fd", plot=FALSE)
  tres <- system.time(result <- phfit.group(ph=cf1(5), counts=h.res$counts, breaks=h.res$breaks))
  print(result)
  print(tres)
})
