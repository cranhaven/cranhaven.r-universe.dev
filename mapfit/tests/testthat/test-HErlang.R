test_that("moment", {
  mixrate <- c(0.6, 0.4)
  rate <- c(3, 4)
  shape <- c(1, 2)

  p1 <- herlang(shape=shape, mixrate=mixrate, rate=rate)  
  p2 <- as.gph(p1)
  expected <- ph.moment(10, p1)
  result <- ph.moment(10, p2)
  expect_equal(result, expected)
})

test_that("density", {
  tt <- seq(0, 10, length.out=10)
  mixrate <- c(0.6, 0.4)
  rate <- c(3, 4)
  shape <- c(1, 2)

  p <- herlang(shape=shape, mixrate=mixrate, rate=rate)
  gph <- as.gph(p)
  f <- function(t, alpha, xi, Q) {
    (alpha %*% expm(Q*t) %*% xi)[1]
  }
  
  result <- dphase(tt, p)
  expected <- sapply(tt, f, alpha=gph$alpha(), Q=gph$Q(), xi=gph$xi())
  expect_equal(result, expected)
})

test_that("ccdf", {
  tt <- seq(0, 10, length.out=10)
  mixrate <- c(0.6, 0.4)
  rate <- c(3, 4)
  shape <- c(1, 2)
  
  p <- herlang(shape=shape, mixrate=mixrate, rate=rate)
  gph <- as.gph(p)
  f <- function(t, alpha, xi, Q) {
    vone <- rep(1, length(alpha))
    (alpha %*% expm(Q*t) %*% vone)[1]
  }
  
  result <- pphase(tt, p, lower.tail = F)
  expected <- sapply(tt, f, alpha=gph$alpha(), Q=gph$Q(), xi=gph$xi())
  expect_equal(result, expected)
})

test_that("estimate_point", {
  RNGkind(kind = "Mersenne-Twister")
  set.seed(1234)
  wsample <- rweibull(100, shape=2, scale=1)
  tres <- system.time(result <- phfit.point(ph=herlang(5), x=wsample))
  print(result)
  print(tres)
})

test_that("estimate_group", {
  RNGkind(kind = "Mersenne-Twister")
  set.seed(1234)
  wsample <- rweibull(100, shape=2, scale=1)
  h.res <- hist(wsample, breaks="fd", plot=FALSE)
  tres <- system.time(result <- phfit.group(ph=herlang(5), counts=h.res$counts, breaks=h.res$breaks))
  print(result)
  print(tres)
})
