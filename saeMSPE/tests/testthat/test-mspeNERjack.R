library(testthat)
library(saeMSPE)

test_that("mspeNERjack works correctly", {
  Ni <- 1000
  sigmaX <- 1.5
  m <- 5
  beta <- c(0.5, 1)
  sigma_v2 <- 0.8
  sigma_e2 <- 1
  ni <- sample(seq(1, 10), m, replace = TRUE)
  n <- sum(ni)
  p <- length(beta)
  
  pop.model <- function(Ni, sigmaX, beta, sigma_v2, sigma_e2, m) {
    x <- rnorm(m * Ni, 1, sqrt(sigmaX))
    v <- rnorm(m, 0, sqrt(sigma_v2))
    y <- numeric(m * Ni)
    theta <- numeric(m)
    kk <- 1
    for (i in 1:m) {
      sumx <- 0
      for (j in 1:Ni) {
        sumx <- sumx + x[kk]
        y[kk] <- beta[1] + beta[2] * x[kk] + v[i] + rnorm(1, 0, sqrt(sigma_e2))
        kk <- kk + 1
      }
      meanx <- sumx / Ni
      theta[i] <- beta[1] + beta[2] * meanx + v[i]
    }
    group <- rep(seq(m), each = Ni)
    data <- data.frame(y = y, group = group, x1 = x)
    return(list(data = data, theta = theta))
  } 
  
  sampleXY <- function(Ni, ni, m, Population) {
    Indx <- c()
    for (i in 1:m) {
      Indx <- c(Indx, sample(c(((i - 1) * Ni + 1):(i * Ni)), ni[i]))
    }
    Sample <- Population[Indx, ]
    return(Sample)
  } 
  
  Population <- pop.model(Ni, sigmaX, beta, sigma_v2, sigma_e2, m)$data
  XY <- sampleXY(Ni, ni, m, Population)
  
  formula <- y ~ x1
  data <- XY
  
  Xmean <- matrix(NA, m, p)
  for (tt in 1:m) {
    Xmean[tt, ] <- colMeans(Population[which(Population$group == tt), "x1", drop = FALSE])
  }
  result <- mspeNERjack(ni, formula, data, Xmean, method = 1)
  
  expect_true(is.list(result))
  expect_true(is.vector(result$MSPE))
  expect_true(is.vector(result$bhat))
  expect_true(is.numeric(result$sigvhat2))
  expect_true(is.numeric(result$sigehat2))
  
})