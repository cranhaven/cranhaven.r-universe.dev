
context("improveSmallScales")

source(system.file("tests/comparisons/singleStat.R", package = "stepR"))

testJump <- function(grid, time, data, filter, correlations, leftValue, rightValue) {
  len <- length(data)
  m <- min(len, length(correlations) - 1L)
  
  if (len == 1) {
    A <- matrix(correlations[1], 1, 1)
  } else {
    A <- matrix(0, len, len)
    for (i in 1:(len - 1)) {
      A[i, i] <- correlations[1]
      A[i, i + 1:min(m, len - i)] <- correlations[2:min(m + 1, len - i + 1)]
      A[i + 1:min(m, len - i), i] <- correlations[2:min(m + 1, len - i + 1)]
    }
    A[len, len] <- correlations[1]  
  }
  
  costs <- numeric(length(grid))
  
  for (i in seq(along = grid)) {
    mu <- lowpassFilter::getConvolutionJump(time, grid[i], leftValue, rightValue, filter) 
    costs[i] <- sum((data - mu) * solve(A, data - mu))
  }
  
  grid[which.min(costs)]
}

testPeak <- function(gridLeft, gridRight, time, data, filter, correlations, leftValue, rightValue, tol) {
  len <- length(data)
  m <- min(len, length(correlations) - 1L)
  
  if (len == 1) {
    A <- matrix(correlations[1], 1, 1)
  } else {
    A <- matrix(0, len, len)
    for (i in 1:(len - 1)) {
      A[i, i] <- correlations[1]
      A[i, i + 1:min(m, len - i)] <- correlations[2:min(m + 1, len - i + 1)]
      A[i + 1:min(m, len - i), i] <- correlations[2:min(m + 1, len - i + 1)]
    }
    A[len, len] <- correlations[1]  
  }
  
  costs <- numeric(length(gridLeft) * length(gridRight))
  costs <- rep(NA, length(gridLeft) * length(gridRight))
  cp1 <- integer(length(gridLeft) * length(gridRight))
  cp2 <- integer(length(gridLeft) * length(gridRight))
  value <- numeric(length(gridLeft) * length(gridRight))
  index <- 0
  
  for (left in gridLeft) {
    for (right in gridRight[gridRight > left + tol]) {
      index <- index + 1
      cp1[index] <- left
      cp2[index] <- right
      
      Fleft <- filter$truncatedStepfun(time - left)
      Fright <- filter$truncatedStepfun(time - right)
      w <- Fleft - Fright
      
      sol <- solve(A, w)
      
      value[index] <- sum((data - leftValue * (1 - Fleft) - rightValue * Fright) * sol) / sum(w * sol)
      
      convolvedSignal <- lowpassFilter::getConvolutionPeak(time, left, right, value[index], leftValue, rightValue, filter)
      costs[index] <- sum((data - convolvedSignal) * solve(A, data - convolvedSignal))
    }
  }
  
  index <- which.min(costs[!is.na(costs)])
  list(left = cp1[index], right = cp2[index], value = value[index])
}

statAll <- function(y, filter, fit, singleStat, len, add, ...) {
  stat <- rep(-Inf, length(y))
  
  start <- fit$leftEnd[1] * filter$sr + filter$len - 1L
  end <- fit$rightEnd[1] * filter$sr - len - filter$len + 1L
  if (start <= end) {
    for (li in start:end) {
      # print(li)
      ri <- li + len
      obs <- y[(li + 1):(ri + filter$len - 1)]
      time <- (li + 1):(ri + filter$len - 1) / filter$sr
      
      stat[li] <- singleStat(obs = obs, time = time, filter = filter,
                             left = li / filter$sr, right = ri / filter$sr,
                             leftValue = fit$value[1], rightValue = fit$value[1], 
                             leftVar = fit$var[1], rightVar = fit$var[1], cp = 0, ...)
      
    }
  }
  
  for (inSeg in seq(along = fit$leftEnd)[-1]) {
    start <- max(fit$rightEnd[inSeg - 1L] * filter$sr - len - filter$len + 2L, start)
    end <- min(fit$leftEnd[inSeg] * filter$sr + filter$len - 2L,
               fit$rightEnd[inSeg] * filter$sr - len - filter$len + 1L)
    
    if (start <= end) {
      for (li in start:end) {
        # print(li)
        ri <- li + len
        obs <- y[(li + 1):(ri + filter$len - 1)]
        time <- (li + 1):(ri + filter$len - 1) / filter$sr
        stat[li] <-  singleStat(obs = obs, time = time, filter = filter,
                                left = li / filter$sr, right = ri / filter$sr,
                                leftValue = fit$value[inSeg - 1L], rightValue = fit$value[inSeg], 
                                leftVar = fit$var[inSeg - 1L], rightVar = fit$var[inSeg],
                                cp = fit$leftEnd[inSeg] / filter$sr, ...)
      }
    }
    
    start <- fit$leftEnd[inSeg] * filter$sr + filter$len - 1L
    end <- fit$rightEnd[inSeg] * filter$sr - len - filter$len + 1L
    if (start <= end) {
      for (li in start:end) {
        # print(li)
        ri <- li + len
        obs <- y[(li + 1):(ri + filter$len - 1)]
        time <- (li + 1):(ri + filter$len - 1) / filter$sr
        stat[li] <-  singleStat(obs = obs, time = time, filter = filter,
                                left = li / filter$sr, right = ri / filter$sr,
                                leftValue = fit$value[inSeg], rightValue = fit$value[inSeg], 
                                leftVar = fit$var[inSeg], rightVar = fit$var[inSeg], cp = 0, ...)
      }
    }
  }
  
  stat
}

test_that("a single long segment is handled correctly", {
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 100)
  testdata <- lowpassFilter::randomGeneration(n = 100, filter = testfilter, signal = rep(0, 100), noise = 1, seed = "no")
  testfit <- stepR::stepblock(value = mean(testdata), leftEnd = 0, rightEnd = 1, x0 = 0)
  
  ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                            method = "2Param", lengths = 1, q = rep(1e9, 1))
  compare <- stepR::stepblock(leftEnd = 0, rightEnd = 1,
                              value = median(testdata[testfilter$len:(100 - testfilter$len)]), x0 = 0)
  class(compare) <- c("localDeconvolution", class(compare))
  attr(compare, "noDeconvolution") <- integer(0)
  compareq <- 1e9
  attr(compareq, "n") <- 100L
  attr(compare, "q") <- compareq
  
  expect_equal(ret, compare, tolerance = 1e-14)
})

test_that("a single short segment is handled correctly", {
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 10)
  testdata <- lowpassFilter::randomGeneration(n = 10, filter = testfilter, signal = rep(0, 10), noise = 1, seed = "no")
  testfit <- stepR::stepblock(value = mean(testdata), leftEnd = 0, rightEnd = 1, x0 = 0)
  
  ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                            method = "2Param", lengths = 1, q = rep(1e9, 1),
                            suppressWarningNoDeconvolution = TRUE)
  compare <- stepR::stepblock(leftEnd = 0, rightEnd = 1,
                              value = median(testdata), x0 = 0)
  class(compare) <- c("localDeconvolution", class(compare))
  attr(compare, "noDeconvolution") <- 1L
  compareq <- 1e9
  attr(compareq, "n") <- 10L
  attr(compare, "q") <- compareq
  
  expect_equal(ret, compare, tolerance = 1e-14)
})

test_that("a single jump is deconvolved correctly", {
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 100)
  testdata <- lowpassFilter::randomGeneration(n = 100, filter = testfilter, noise = 1, seed = "no",
                               signal = lowpassFilter::getConvolutionJump(1:100 / testfilter$sr, 0.5, 0, 1, testfilter))
  testfit <- stepR::stepblock(value = c(0, 1), leftEnd = c(0, 0.5), rightEnd = c(0.5, 1), x0 = 0)
  
  leftValue <- median(testdata[testfilter$len:(50 - testfilter$len)])
  rightValue <- median(testdata[(50 + testfilter$len):(100 - testfilter$len)])
  cor <- testfilter$acf
  cor[1] <- 2
  cp <- testJump(39:50 / testfilter$sr, 40:60 / testfilter$sr, testdata[40:60], testfilter, cor,
                 leftValue, rightValue)
  leftValue <- median(testdata[testfilter$len:(cp * testfilter$sr - 1)])
  rightValue <- median(testdata[(cp * testfilter$sr + testfilter$len + 1):(100 - testfilter$len)])
  compare1 <- stepR::stepblock(leftEnd = c(0, cp), rightEnd = c(cp, 1),
                               value = c(leftValue, rightValue), x0 = 0)
  class(compare1) <- c("localDeconvolution", class(compare1))
  indices <- (cp * testfilter$sr):(cp * testfilter$sr + testfilter$len)
  cp <- testJump(seq(cp - 1 / testfilter$sr, cp + 1 / testfilter$sr, 0.1 / testfilter$sr),
                 indices / testfilter$sr, testdata[indices], testfilter, cor, leftValue, rightValue)
  compare2 <- stepR::stepblock(leftEnd = c(0, cp), rightEnd = c(cp, 1),
                               value = c(leftValue, rightValue), x0 = 0)
  class(compare2) <- c("localDeconvolution", class(compare2))
  cp <- testJump(seq(cp - 0.1 / testfilter$sr, cp + 0.1 / testfilter$sr, 0.01 / testfilter$sr),
                 indices / testfilter$sr, testdata[indices], testfilter, cor, leftValue, rightValue)
  compare3 <- stepR::stepblock(leftEnd = c(0, cp), rightEnd = c(cp, 1),
                               value = c(leftValue, rightValue), x0 = 0)
  class(compare3) <- c("localDeconvolution", class(compare3))
  compare <- list(compare1, compare2, compare3)
  attr(compare, "noDeconvolution") <- integer(0)
  attr(compare3, "noDeconvolution") <- integer(0)
  compareq <- 1e9
  attr(compareq, "n") <- 100L
  attr(compare, "q") <- compareq
  attr(compare3, "q") <- compareq
  
  ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                            method = "2Param", lengths = 1, q = rep(1e9, 1))
  retall <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                               method = "2Param", lengths = 1, q = rep(1e9, 1), output = "everyGrid")
  
  test <- retall[[3]]
  attr(test, "noDeconvolution") <- integer(0)
  attr(test, "q") <- attr(retall, "q")
  expect_identical(ret, test)
  expect_equal(ret, compare3)
  expect_equal(retall, compare)
})

test_that("a short segment at the beginning is handled correctly", {
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 100)
  testdata <- lowpassFilter::randomGeneration(n = 100, filter = testfilter, noise = 1, seed = "no",
                               signal = lowpassFilter::getConvolutionJump(1:100 / testfilter$sr, 0.05, 0, 1, testfilter))
  testfit <- stepR::stepblock(value = c(0, 1), leftEnd = c(0, 0.05), rightEnd = c(0.05, 1), x0 = 0)
  
  ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                            method = "2Param", lengths = 1, q = rep(1e9, 1),
                            suppressWarningNoDeconvolution = TRUE)
  compare <- stepR::stepblock(leftEnd = c(0, 0.05), rightEnd = c(0.05, 1),
                              value = c(median(testdata[1:5]), median(testdata[16:89])), x0 = 0)
  class(compare) <- c("localDeconvolution", class(compare))
  attr(compare, "noDeconvolution") <- 1L
  compareq <- 1e9
  attr(compareq, "n") <- 100L
  attr(compare, "q") <- compareq
  
  expect_equal(ret, compare, tolerance = 1e-14)
})

test_that("a short segment at the end is handled correctly", {
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 100)
  testdata <- lowpassFilter::randomGeneration(n = 100, filter = testfilter, noise = 1, seed = "no",
                               signal = lowpassFilter::getConvolutionJump(1:100 / testfilter$sr, 0.95, 0, 1, testfilter))
  testfit <- stepR::stepblock(value = c(0, 1), leftEnd = c(0, 0.95), rightEnd = c(0.95, 1), x0 = 0)
  
  ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                            method = "2Param", lengths = 1, q = rep(1e9, 1),
                            suppressWarningNoDeconvolution = TRUE)
  compare <- stepR::stepblock(leftEnd = c(0, 0.95), rightEnd = c(0.95, 1),
                              value = c(median(testdata[11:84]), median(testdata[96:100])), x0 = 0)
  class(compare) <- c("localDeconvolution", class(compare))
  attr(compare, "noDeconvolution") <- 2L
  compareq <- 1e9
  attr(compareq, "n") <- 100L
  attr(compare, "q") <- compareq
  
  expect_equal(ret, compare, tolerance = 1e-14)
})

test_that("two short segments are handled correctly", {
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 30)
  testdata <- lowpassFilter::randomGeneration(n = 30, filter = testfilter, noise = 1, seed = "no",
                               signal = lowpassFilter::getConvolutionJump(1:30 / testfilter$sr, 0.5, 0, 1, testfilter))
  testfit <- stepR::stepblock(value = c(0, 1), leftEnd = c(0, 0.5), rightEnd = c(0.5, 1), x0 = 0)
  
  ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                            method = "2Param", lengths = 1, q = rep(1e9, 1),
                            suppressWarningNoDeconvolution = TRUE)
  compare <- stepR::stepblock(leftEnd = c(0), rightEnd = c(1),
                              value = c(median(testdata)), x0 = 0)
  class(compare) <- c("localDeconvolution", class(compare))
  attr(compare, "noDeconvolution") <- 1L
  compareq <- 1e9
  attr(compareq, "n") <- 30L
  attr(compare, "q") <- compareq
  
  expect_equal(ret, compare, tolerance = 1e-14)
})

test_that("two short segments at beginning and at end are handled correctly", {
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 100)
  testdata <- lowpassFilter::randomGeneration(n = 100, filter = testfilter, noise = 1, seed = "no",
                               signal = lowpassFilter::getConvolutionJump(1:100 / testfilter$sr, 0.5, 0, 1, testfilter))
  testfit <- stepR::stepblock(value = c(0, 1, 0, 1, 0), leftEnd = c(0, 0.02, 0.05, 0.95, 0.98),
                              rightEnd = c(0.02, 0.05, 0.95, 0.98, 1), x0 = 0)
  
  ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                            method = "2Param", lengths = 1, q = rep(1e9, 1),
                            suppressWarningNoDeconvolution = TRUE)
  compare <- stepR::stepblock(leftEnd = c(0, 0.05, 0.95), rightEnd = c(0.05, 0.95, 1),
                              value = c(median(testdata[1:5]), median(testdata[16:84]),
                                        median(testdata[96:100])), x0 = 0)
  class(compare) <- c("localDeconvolution", class(compare))
  attr(compare, "noDeconvolution") <- c(1L, 3L)
  compareq <- 1e9
  attr(compareq, "n") <- 100L
  attr(compare, "q") <- compareq
  
  expect_equal(ret, compare, tolerance = 1e-14)
})

test_that("a single peak formed by two close jumps is deconvolved correctly", {
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 100)
  testdata <- lowpassFilter::randomGeneration(n = 100, filter = testfilter, noise = 1, seed = "no",
                               signal = lowpassFilter::getConvolutionPeak(1:100 / testfilter$sr, 0.5, 0.53, 10, 0, 0,
                                                           testfilter))
  testfit <- stepR::stepblock(value = c(0, 1, 0), leftEnd = c(0, 0.5, 0.53),
                              rightEnd = c(0.5, 0.53, 1), x0 = 0)
  
  leftValue <- median(testdata[testfilter$len:(50 - testfilter$len)])
  rightValue <- median(testdata[(53 + testfilter$len):(100 - testfilter$len)])
  cor <- testfilter$acf
  cor[1] <- 2
  ret <- testPeak(39:50 / testfilter$sr, 42:53 / testfilter$sr, 40:63 / testfilter$sr, 
                  testdata[40:63], testfilter, cor, leftValue, rightValue, 1e-6 / testfilter$sr)
  compare1 <- stepR::stepblock(leftEnd = c(0, ret$left, ret$right), rightEnd = c(ret$left, ret$right, 1),
                               value = c(leftValue, ret$value, rightValue), x0 = 0)
  class(compare1) <- c("localDeconvolution", class(compare1))
  ret <- testPeak(seq(ret$left - 1 / testfilter$sr, ret$left + 1 / testfilter$sr, 0.1 / testfilter$sr),
                  seq(ret$right - 1 / testfilter$sr, ret$right + 1 / testfilter$sr, 0.1 / testfilter$sr),
                  40:63 / testfilter$sr, testdata[40:63], testfilter, cor, leftValue, rightValue,
                  1e-6 / testfilter$sr)
  compare2 <- stepR::stepblock(leftEnd = c(0, ret$left, ret$right), rightEnd = c(ret$left, ret$right, 1),
                               value = c(leftValue, ret$value, rightValue), x0 = 0)
  class(compare2) <- c("localDeconvolution", class(compare2))
  ret <- testPeak(seq(ret$left - 0.1 / testfilter$sr, ret$left + 0.1 / testfilter$sr, 0.01 / testfilter$sr),
                  seq(ret$right - 0.1 / testfilter$sr, ret$right + 0.1 / testfilter$sr, 0.01 / testfilter$sr),
                  40:63 / testfilter$sr, testdata[40:63], testfilter, cor, leftValue, rightValue,
                  1e-6 / testfilter$sr)
  compare3 <- stepR::stepblock(leftEnd = c(0, ret$left, ret$right), rightEnd = c(ret$left, ret$right, 1),
                               value = c(leftValue, ret$value, rightValue), x0 = 0)
  class(compare3) <- c("localDeconvolution", class(compare3))
  compare <- list(compare1, compare2, compare3)
  attr(compare, "noDeconvolution") <- integer(0)
  attr(compare3, "noDeconvolution") <- integer(0)
  compareq <- 1e9
  attr(compareq, "n") <- 100L
  attr(compare, "q") <- compareq
  attr(compare3, "q") <- compareq
  
  ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                            method = "2Param", lengths = 1, q = rep(1e9, 1))
  retall <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                               method = "2Param", lengths = 1, q = rep(1e9, 1), output = "everyGrid")
  
  test <- retall[[3]]
  attr(test, "noDeconvolution") <- integer(0)
  attr(test, "q") <- attr(retall, "q")
  expect_identical(ret, test)
  expect_equal(ret, compare3)
  expect_equal(retall, compare)
}) 

test_that("a single peak formed by three close jumps is deconvolved correctly", {
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 100)
  testdata <- lowpassFilter::randomGeneration(n = 100, filter = testfilter, noise = 1, seed = "no",
                               signal = lowpassFilter::getConvolutionPeak(1:100 / testfilter$sr, 0.5, 0.53, 10, 0, 0,
                                                           testfilter))
  testfit <- stepR::stepblock(value = c(0, 1, 2, 0), leftEnd = c(0, 0.48, 0.5, 0.53),
                              rightEnd = c(0.48, 0.5, 0.53, 1), x0 = 0)
  
  leftValue <- median(testdata[testfilter$len:(48 - testfilter$len)])
  rightValue <- median(testdata[(53 + testfilter$len):(100 - testfilter$len)])
  cor <- testfilter$acf
  cor[1] <- 2
  ret <- testPeak(37:48 / testfilter$sr, 42:53 / testfilter$sr, 38:63 / testfilter$sr, 
                  testdata[38:63], testfilter, cor, leftValue, rightValue, 1e-6 / testfilter$sr)
  compare1 <- stepR::stepblock(leftEnd = c(0, ret$left, ret$right), rightEnd = c(ret$left, ret$right, 1),
                               value = c(leftValue, ret$value, rightValue), x0 = 0)
  class(compare1) <- c("localDeconvolution", class(compare1))
  ret <- testPeak(seq(ret$left - 1 / testfilter$sr, ret$left + 1 / testfilter$sr, 0.1 / testfilter$sr),
                  seq(ret$right - 1 / testfilter$sr, ret$right + 1 / testfilter$sr, 0.1 / testfilter$sr),
                  38:63 / testfilter$sr, testdata[38:63], testfilter, cor, leftValue, rightValue,
                  1e-6 / testfilter$sr)
  compare2 <- stepR::stepblock(leftEnd = c(0, ret$left, ret$right), rightEnd = c(ret$left, ret$right, 1),
                               value = c(leftValue, ret$value, rightValue), x0 = 0)
  class(compare2) <- c("localDeconvolution", class(compare2))
  ret <- testPeak(seq(ret$left - 0.1 / testfilter$sr, ret$left + 0.1 / testfilter$sr, 0.01 / testfilter$sr),
                  seq(ret$right - 0.1 / testfilter$sr, ret$right + 0.1 / testfilter$sr, 0.01 / testfilter$sr),
                  38:63 / testfilter$sr, testdata[38:63], testfilter, cor, leftValue, rightValue,
                  1e-6 / testfilter$sr)
  compare3 <- stepR::stepblock(leftEnd = c(0, ret$left, ret$right), rightEnd = c(ret$left, ret$right, 1),
                               value = c(leftValue, ret$value, rightValue), x0 = 0)
  class(compare3) <- c("localDeconvolution", class(compare3))
  compare <- list(compare1, compare2, compare3)
  attr(compare, "noDeconvolution") <- 2L
  attr(compare3, "noDeconvolution") <- 2L
  compareq <- 1e9
  attr(compareq, "n") <- 100L
  attr(compare, "q") <- compareq
  attr(compare3, "q") <- compareq
  
  ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                            method = "2Param", lengths = 1, q = rep(1e9, 1),
                            suppressWarningNoDeconvolution = TRUE)
  retall <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                               method = "2Param", lengths = 1, q = rep(1e9, 1), output = "everyGrid",
                               suppressWarningNoDeconvolution = TRUE)
  
  test <- retall[[3]]
  attr(test, "noDeconvolution") <- 2L
  attr(test, "q") <- attr(retall, "q")
  expect_identical(ret, test)
  expect_equal(ret, compare3)
  expect_equal(retall, compare)
}) 

test_that("a single detected peak is deconvolved correctly", {
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 200)
  testdata <- lowpassFilter::randomGeneration(n = 200, filter = testfilter, noise = 1, seed = "no",
                               signal = lowpassFilter::getConvolutionPeak(1:200 / testfilter$sr, 0.5, 0.515, 10, 0, 0,
                                                           testfilter))
  testfit <- stepR::stepblock(value = c(mean(testdata)), leftEnd = c(0),
                              rightEnd = c(1), x0 = 0)
  
  
  comparefit <- stepR::stepblock(median(testdata[testfilter$len:(200 - testfilter$len)]),
                                 leftEnd = 0, rightEnd = 1, x0 = 0)
  comparefit$var <- stepR::sdrobnorm(testdata[testfilter$len:(200 - testfilter$len)],
                                     lag = testfilter$len + 1L)^2
  
  stat <- statAll(y = testdata, filter = testfilter, fit = comparefit,
                  singleStat = singleStat2Param, len = 3, add = integer(0))
  expect_length(stat, 200)

  if (all(diff(which(stat > 40)) == 1)) {
    li <- which.max(stat)
    ri <- li + 3
    leftValue <- median(testdata[testfilter$len:(li - 6L)])
    rightValue <- median(testdata[(ri + 6L + testfilter$len):(200 - testfilter$len)])
    cor <- testfilter$acf
    cor[1] <- 2
    ret <- testPeak((li - 6L):(li + 6L) / testfilter$sr, (ri - 6L):(ri + 6L) / testfilter$sr,
                    (li - 5L):(ri + 5L + testfilter$len) / testfilter$sr,
                    testdata[(li - 5L):(ri + 5L + testfilter$len)],
                    testfilter, cor, leftValue, rightValue, 1e-6 / testfilter$sr)
    compare1 <- stepR::stepblock(leftEnd = c(0, ret$left, ret$right), rightEnd = c(ret$left, ret$right, 1),
                                 value = c(leftValue, ret$value, rightValue), x0 = 0)
    class(compare1) <- c("localDeconvolution", class(compare1))
    ret <- testPeak(seq(ret$left - 1 / testfilter$sr, ret$left + 1 / testfilter$sr, 0.1 / testfilter$sr),
                    seq(ret$right - 1 / testfilter$sr, ret$right + 1 / testfilter$sr, 0.1 / testfilter$sr),
                    (li - 5L):(ri + 5L + testfilter$len) / testfilter$sr,
                    testdata[(li - 5L):(ri + 5L + testfilter$len)],
                    testfilter, cor, leftValue, rightValue, 1e-6 / testfilter$sr)
    compare2 <- stepR::stepblock(leftEnd = c(0, ret$left, ret$right), rightEnd = c(ret$left, ret$right, 1),
                                 value = c(leftValue, ret$value, rightValue), x0 = 0)
    class(compare2) <- c("localDeconvolution", class(compare2))
    ret <- testPeak(seq(ret$left - 0.1 / testfilter$sr, ret$left + 0.1 / testfilter$sr, 0.01 / testfilter$sr),
                    seq(ret$right - 0.1 / testfilter$sr, ret$right + 0.1 / testfilter$sr, 0.01 / testfilter$sr),
                    (li - 5L):(ri + 5L + testfilter$len) / testfilter$sr,
                    testdata[(li - 5L):(ri + 5L + testfilter$len)],
                    testfilter, cor, leftValue, rightValue, 1e-6 / testfilter$sr)
    compare3 <- stepR::stepblock(leftEnd = c(0, ret$left, ret$right), rightEnd = c(ret$left, ret$right, 1),
                                 value = c(leftValue, ret$value, rightValue), x0 = 0)
    class(compare3) <- c("localDeconvolution", class(compare3))
    compare <- list(compare1, compare2, compare3)
    attr(compare, "noDeconvolution") <- integer(0)
    attr(compare3, "noDeconvolution") <- integer(0)
    compareq <- 40
    attr(compareq, "n") <- 200L
    attr(compare, "q") <- compareq
    attr(compare3, "q") <- compareq
    
    ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                              method = "2Param", lengths = 3, q = rep(40, 1))
    retall <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                 method = "2Param", lengths = 3, q = rep(40, 1), output = "everyGrid")
    
    test <- retall[[3]]
    attr(test, "noDeconvolution") <- attr(retall, "noDeconvolution")
    attr(test, "q") <- attr(retall, "q")
    expect_identical(ret, test)
    expect_equal(ret, compare3)
    expect_equal(retall, compare)
  }
}) 

test_that("a single detected peak close to the start and the end is deconvolved correctly", {
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 50)
  testdata <- lowpassFilter::randomGeneration(n = 50, filter = testfilter, noise = 1, seed = "no",
                               signal = lowpassFilter::getConvolutionPeak(1:50 / testfilter$sr, 0.5, 0.56, 10, 0, 0,
                                                           testfilter))
  testfit <- stepR::stepblock(value = c(mean(testdata)), leftEnd = c(0),
                              rightEnd = c(1), x0 = 0)
  
  ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                            method = "2Param", lengths = 3, q = rep(20, 1),
                            suppressWarningNoDeconvolution = TRUE)
  
  comparefit <- stepR::stepblock(median(testdata[testfilter$len:(50 - testfilter$len)]),
                                 leftEnd = 0, rightEnd = 1, x0 = 0)
  comparefit$var <- stepR::sdrobnorm(testdata[testfilter$len:(50 - testfilter$len)],
                                     lag = testfilter$len + 1L)^2
  if (max(statAll(y = testdata, filter = testfilter, fit = comparefit,
                  singleStat = singleStat2Param, len = 3, add = integer(0))) > 20) {
    compare <- stepR::stepblock(leftEnd = 0, rightEnd = 1,
                                value = median(testdata), x0 = 0)
    class(compare) <- c("localDeconvolution", class(compare))
    attr(compare, "noDeconvolution") <- 1L
  } else {
    compare <- stepR::stepblock(leftEnd = 0, rightEnd = 1,
                                value = comparefit$value, x0 = 0)
    class(compare) <- c("localDeconvolution", class(compare))
    attr(compare, "noDeconvolution") <- integer(0L)
  }
  compareq <- 20
  attr(compareq, "n") <- 50L
  attr(compare, "q") <- compareq
  
  expect_equal(ret, compare, tolerance = 1e-14)
}) 

test_that("a single detected peak close to the end is deconvolved correctly", {
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 100)
  testdata <- lowpassFilter::randomGeneration(n = 100, filter = testfilter, noise = 1, seed = "no",
                               signal = lowpassFilter::getConvolutionPeak(1:100 / testfilter$sr, 0.5, 0.53, 10, 0, 0,
                                                           testfilter))
  testfit <- stepR::stepblock(value = c(mean(testdata)), leftEnd = c(0),
                              rightEnd = c(1), x0 = 0)

  comparefit <- stepR::stepblock(median(testdata[testfilter$len:(100 - testfilter$len)]),
                                 leftEnd = 0, rightEnd = 1, x0 = 0)
  comparefit$var <- stepR::sdrobnorm(testdata[testfilter$len:(100 - testfilter$len)],
                                     lag = testfilter$len + 1L)^2
  comparestat <- statAll(y = testdata, filter = testfilter, fit = comparefit,
                         singleStat = singleStat2Param, len = 3, add = integer(0))
  
  if (max(comparestat) > 23) {
    li <- which.max(comparestat)
    ri <- li + 3L
    leftValue <- median(testdata[testfilter$len:(li - 6L)])
    rightValue <- median(testdata[(li - 6L + testfilter$len + 1L):length(testdata)])
    compare <- stepR::stepblock(leftEnd = c(0, (li - 6L + testfilter$len) / testfilter$sr),
                                rightEnd = c((li - 6L + testfilter$len) / testfilter$sr, 1),
                                value = c(leftValue, rightValue), x0 = 0)
    class(compare) <- c("localDeconvolution", class(compare))
    attr(compare, "noDeconvolution") <- 2L
  } else {
    compare <- stepR::stepblock(leftEnd = 0, rightEnd = 1,
                                value = comparefit$value, x0 = 0)
    class(compare) <- c("localDeconvolution", class(compare))
    attr(compare, "noDeconvolution") <- integer(0L)
  }
  compareq <- 23
  attr(compareq, "n") <- 100L
  attr(compare, "q") <- compareq
  
  ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                            method = "2Param", lengths = 3, q = rep(23, 1),
                            suppressWarningNoDeconvolution = TRUE)
  
  expect_equal(ret, compare, tolerance = 1e-14)
}) 

test_that("a single jump is correctly replaced by a detected peak", {
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 200)
  testdata <- lowpassFilter::randomGeneration(n = 200, filter = testfilter, noise = 1, seed = "no",
                                              signal = lowpassFilter::getConvolutionPeak(1:200 / testfilter$sr,
                                                                                         0.5, 0.515, 10, 0, 0,
                                                                                         testfilter))
  testfit <- stepR::stepblock(value = c(mean(testdata[1:100]), mean(testdata[101:200])),
                              leftEnd = c(0, 0.5), rightEnd = c(0.5, 1), x0 = 0)
  
  comparefit <- deconvolveLocally(fit = testfit, data = testdata, filter = testfilter, gridSize = 1 / testfilter$sr)
  comparefit$var <- c(stepR::sdrobnorm(testdata[testfilter$len:(100 - testfilter$len)],
                                     lag = testfilter$len + 1L)^2,
                      stepR::sdrobnorm(testdata[(100 + testfilter$len):(200 - testfilter$len)],
                                       lag = testfilter$len + 1L)^2)
  
  stat <- statAll(y = testdata, filter = testfilter, fit = comparefit,
                  singleStat = singleStat2Param, len = 3, add = integer(0))

  if (all(diff(which(stat > 40)) == 1)) {
    li <- which.max(stat)
    ri <- li + 3
    leftValue <- median(testdata[testfilter$len:(li - 6L)])
    rightValue <- median(testdata[(ri + 6L + testfilter$len):(200 - testfilter$len)])
    cor <- testfilter$acf
    cor[1] <- 2
    ret <- testPeak((li - 6L):(li + 6L) / testfilter$sr, (ri - 6L):(ri + 6L) / testfilter$sr,
                    (li - 5L):(ri + 5L + testfilter$len) / testfilter$sr,
                    testdata[(li - 5L):(ri + 5L + testfilter$len)],
                    testfilter, cor, leftValue, rightValue, 1e-6 / testfilter$sr)
    compare1 <- stepR::stepblock(leftEnd = c(0, ret$left, ret$right), rightEnd = c(ret$left, ret$right, 1),
                                 value = c(leftValue, ret$value, rightValue), x0 = 0)
    class(compare1) <- c("localDeconvolution", class(compare1))
    ret <- testPeak(seq(ret$left - 1 / testfilter$sr, ret$left + 1 / testfilter$sr, 0.1 / testfilter$sr),
                    seq(ret$right - 1 / testfilter$sr, ret$right + 1 / testfilter$sr, 0.1 / testfilter$sr),
                    (li - 5L):(ri + 5L + testfilter$len) / testfilter$sr,
                    testdata[(li - 5L):(ri + 5L + testfilter$len)],
                    testfilter, cor, leftValue, rightValue, 1e-6 / testfilter$sr)
    compare2 <- stepR::stepblock(leftEnd = c(0, ret$left, ret$right), rightEnd = c(ret$left, ret$right, 1),
                                 value = c(leftValue, ret$value, rightValue), x0 = 0)
    class(compare2) <- c("localDeconvolution", class(compare2))
    ret <- testPeak(seq(ret$left - 0.1 / testfilter$sr, ret$left + 0.1 / testfilter$sr, 0.01 / testfilter$sr),
                    seq(ret$right - 0.1 / testfilter$sr, ret$right + 0.1 / testfilter$sr, 0.01 / testfilter$sr),
                    (li - 5L):(ri + 5L + testfilter$len) / testfilter$sr,
                    testdata[(li - 5L):(ri + 5L + testfilter$len)],
                    testfilter, cor, leftValue, rightValue, 1e-6 / testfilter$sr)
    compare3 <- stepR::stepblock(leftEnd = c(0, ret$left, ret$right), rightEnd = c(ret$left, ret$right, 1),
                                 value = c(leftValue, ret$value, rightValue), x0 = 0)
    class(compare3) <- c("localDeconvolution", class(compare3))
    compare <- list(compare1, compare2, compare3)
    attr(compare, "noDeconvolution") <- integer(0)
    attr(compare3, "noDeconvolution") <- integer(0)
    compareq <- 40
    attr(compareq, "n") <- 200L
    attr(compare, "q") <- compareq
    attr(compare3, "q") <- compareq
    
    ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                              method = "2Param", lengths = 3, q = rep(40, 1))
    retall <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                 method = "2Param", lengths = 3, q = rep(40, 1), output = "everyGrid")
    
    test <- retall[[3]]
    attr(test, "noDeconvolution") <- attr(retall, "noDeconvolution")
    attr(test, "q") <- attr(retall, "q")
    expect_identical(ret, test)
    expect_equal(ret, compare3)
    expect_equal(retall, compare)
  }
}) 

test_that("a single jump close to a detected peak is handled correctly", {
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 200)
  testdata <- lowpassFilter::randomGeneration(n = 200, filter = testfilter, noise = 1, seed = "no",
                                              signal = lowpassFilter::getConvolutionPeak(1:200 / testfilter$sr,
                                                                                         0.5, 0.515, 10, 0, 0,
                                                                                         testfilter))
  testfit <- stepR::stepblock(value = c(mean(testdata[1:108]), mean(testdata[108:200])),
                              leftEnd = c(0, 0.54), rightEnd = c(0.54, 1), x0 = 0)
  
  comparefit <- deconvolveLocally(fit = testfit, data = testdata, filter = testfilter, gridSize = 1 / testfilter$sr)
  comparefit$var <- c(stepR::sdrobnorm(testdata[testfilter$len:(108 - testfilter$len)],
                                       lag = testfilter$len + 1L)^2,
                      stepR::sdrobnorm(testdata[(108 + testfilter$len):(200 - testfilter$len)],
                                       lag = testfilter$len + 1L)^2)
  
  stat <- statAll(y = testdata, filter = testfilter, fit = comparefit,
                  singleStat = singleStat2Param, len = 3, add = integer(0))
  expect_length(stat, 200)
  
  if (all(diff(which(stat > 40)) == 1)) {
    li <- which.max(stat)
    ri <- li + 3
    leftValue <- median(testdata[testfilter$len:(li - 6L)])
    rightValue <- median(testdata[(ri + 6L + testfilter$len):(200 - testfilter$len)])
    cor <- testfilter$acf
    cor[1] <- 2
    ret <- testPeak((li - 6L):(li + 6L) / testfilter$sr, (ri - 6L):(ri + 6L) / testfilter$sr,
                    (li - 5L):(ri + 5L + testfilter$len) / testfilter$sr,
                    testdata[(li - 5L):(ri + 5L + testfilter$len)],
                    testfilter, cor, leftValue, rightValue, 1e-6 / testfilter$sr)
    compare1 <- stepR::stepblock(leftEnd = c(0, ret$left, ret$right), rightEnd = c(ret$left, ret$right, 1),
                                 value = c(leftValue, ret$value, rightValue), x0 = 0)
    class(compare1) <- c("localDeconvolution", class(compare1))
    ret <- testPeak(seq(ret$left - 1 / testfilter$sr, ret$left + 1 / testfilter$sr, 0.1 / testfilter$sr),
                    seq(ret$right - 1 / testfilter$sr, ret$right + 1 / testfilter$sr, 0.1 / testfilter$sr),
                    (li - 5L):(ri + 5L + testfilter$len) / testfilter$sr,
                    testdata[(li - 5L):(ri + 5L + testfilter$len)],
                    testfilter, cor, leftValue, rightValue, 1e-6 / testfilter$sr)
    compare2 <- stepR::stepblock(leftEnd = c(0, ret$left, ret$right), rightEnd = c(ret$left, ret$right, 1),
                                 value = c(leftValue, ret$value, rightValue), x0 = 0)
    class(compare2) <- c("localDeconvolution", class(compare2))
    ret <- testPeak(seq(ret$left - 0.1 / testfilter$sr, ret$left + 0.1 / testfilter$sr, 0.01 / testfilter$sr),
                    seq(ret$right - 0.1 / testfilter$sr, ret$right + 0.1 / testfilter$sr, 0.01 / testfilter$sr),
                    (li - 5L):(ri + 5L + testfilter$len) / testfilter$sr,
                    testdata[(li - 5L):(ri + 5L + testfilter$len)],
                    testfilter, cor, leftValue, rightValue, 1e-6 / testfilter$sr)
    compare3 <- stepR::stepblock(leftEnd = c(0, ret$left, ret$right), rightEnd = c(ret$left, ret$right, 1),
                                 value = c(leftValue, ret$value, rightValue), x0 = 0)
    class(compare3) <- c("localDeconvolution", class(compare3))
    compare <- list(compare1, compare2, compare3)
    attr(compare, "noDeconvolution") <- integer(0)
    attr(compare3, "noDeconvolution") <- integer(0)
    compareq <- 40
    attr(compareq, "n") <- 200L
    attr(compare, "q") <- compareq
    attr(compare3, "q") <- compareq
    
    ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                              method = "2Param", lengths = 3, q = rep(40, 1))
    retall <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                 method = "2Param", lengths = 3, q = rep(40, 1), output = "everyGrid")
    
    test <- retall[[3]]
    attr(test, "noDeconvolution") <- attr(retall, "noDeconvolution")
    attr(test, "q") <- attr(retall, "q")
    expect_identical(ret, test)
    expect_equal(ret, compare3)
    expect_equal(retall, compare)
  }
}) 

test_that("fit, filter and data are tested and have to be given", {
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 100)
  testdata <- lowpassFilter::randomGeneration(n = 100, filter = testfilter, signal = rep(0, 100), noise = 1, seed = "no")
  testfit <- stepR::stepblock(value = mean(testdata), leftEnd = 0, rightEnd = 1, x0 = 0)
  
  expect_error(improveSmallScales())
  expect_error(improveSmallScales(fit = testfit))
  expect_error(improveSmallScales(fit = testfit, filter = testfilter))
  expect_error(improveSmallScales(fit = testfit, data = testdata))
  expect_error(improveSmallScales(filter = testfilter, data = testdata))
  
  expect_error(improveSmallScales(fit = list(leftEnd = 1, rightEnd = 10, value = 1),
                                 filter = testfilter, data = testdata))
  expect_error(improveSmallScales(fit = stepR::stepblock(value = mean(testdata), leftEnd = 0, rightEnd = 0.5, x0 = 0),
                                  filter = testfilter, data = testdata))
  expect_error(improveSmallScales(fit = stepR::stepblock(value = c(0, 1), leftEnd = c(0, 0.1),
                                                         rightEnd = c(0.1, 0.99), x0 = 0),
                                  filter = testfilter, data = testdata))
  expect_error(improveSmallScales(fit = testfit, filter = list(test = 1), data = testdata))
  
  expect_error(improveSmallScales(fit = testfit, filter = testfilter, data = c(testdata, "s")))
  expect_error(improveSmallScales(fit = testfit, filter = testfilter, data = NULL))
  expect_error(improveSmallScales(fit = testfit, filter = testfilter, data = c(testdata, Inf)))
  expect_error(improveSmallScales(fit = testfit, filter = testfilter, data = c(testdata, NA)))
  
  expect_identical(improveSmallScales(fit = list(fit = testfit, stepfit = testfit, q = 1,
                                                 filter = testfilter, sd = 1),
                                      data = testdata, filter = testfilter,
                                      method = "LR", lengths = 3, q = rep(10, 1)),
                   improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                      method = "LR", lengths = 3, q = rep(10, 1)))
  
  expect_identical(improveSmallScales(fit = list(fit = testfit, stepfit = testfit, q = 1,
                                                 filter = testfilter, sd = 1),
                                      data = testdata, filter = testfilter,
                                      method = "LR", lengths = 3, q = rep(10, 1), output = "every"),
                   improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                      method = "LR", lengths = 3, q = rep(10, 1), output = "every"))
})

test_that("method is tested and works", {
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 200)
  testdata <- lowpassFilter::randomGeneration(n = 200, filter = testfilter, noise = 1, seed = "no",
                        signal = lowpassFilter::getConvolutionPeak(1:200 / testfilter$sr, 0.5, 0.515, 10, 0, 0,
                                                                                         testfilter))
  testfit <- stepR::stepblock(value = c(mean(testdata)), leftEnd = c(0),
                              rightEnd = c(1), x0 = 0)
  
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  method = 1, lengths = 3, q = rep(10, 1)))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  method = "s", lengths = 3, q = rep(10, 1)))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  method = "2Ps", lengths = 3, q = rep(10, 1)))

  comparefit <- stepR::stepblock(median(testdata[testfilter$len:(200 - testfilter$len)]),
                                 leftEnd = 0, rightEnd = 1, x0 = 0)

  stat <- statAll(y = testdata, filter = testfilter, fit = comparefit,
                  singleStat = singleStatLR, len = 3, add = integer(0),
                  sd = stepR::sdrobnorm(testdata, lag = testfilter$len + 1), regu = 1)
  
  if (all(diff(which(stat > 10)) == 1)) {
    li <- which.max(stat)
    ri <- li + 3
    leftValue <- median(testdata[testfilter$len:(li - 6L)])
    rightValue <- median(testdata[(ri + 6L + testfilter$len):(200 - testfilter$len)])
    cor <- testfilter$acf
    cor[1] <- 2
    ret <- testPeak((li - 6L):(li + 6L) / testfilter$sr, (ri - 6L):(ri + 6L) / testfilter$sr,
                    (li - 5L):(ri + 5L + testfilter$len) / testfilter$sr,
                    testdata[(li - 5L):(ri + 5L + testfilter$len)],
                    testfilter, cor, leftValue, rightValue, 1e-6 / testfilter$sr)
    compare1 <- stepR::stepblock(leftEnd = c(0, ret$left, ret$right), rightEnd = c(ret$left, ret$right, 1),
                                 value = c(leftValue, ret$value, rightValue), x0 = 0)
    class(compare1) <- c("localDeconvolution", class(compare1))
    ret <- testPeak(seq(ret$left - 1 / testfilter$sr, ret$left + 1 / testfilter$sr, 0.1 / testfilter$sr),
                    seq(ret$right - 1 / testfilter$sr, ret$right + 1 / testfilter$sr, 0.1 / testfilter$sr),
                    (li - 5L):(ri + 5L + testfilter$len) / testfilter$sr,
                    testdata[(li - 5L):(ri + 5L + testfilter$len)],
                    testfilter, cor, leftValue, rightValue, 1e-6 / testfilter$sr)
    compare2 <- stepR::stepblock(leftEnd = c(0, ret$left, ret$right), rightEnd = c(ret$left, ret$right, 1),
                                 value = c(leftValue, ret$value, rightValue), x0 = 0)
    class(compare2) <- c("localDeconvolution", class(compare2))
    ret <- testPeak(seq(ret$left - 0.1 / testfilter$sr, ret$left + 0.1 / testfilter$sr, 0.01 / testfilter$sr),
                    seq(ret$right - 0.1 / testfilter$sr, ret$right + 0.1 / testfilter$sr, 0.01 / testfilter$sr),
                    (li - 5L):(ri + 5L + testfilter$len) / testfilter$sr,
                    testdata[(li - 5L):(ri + 5L + testfilter$len)],
                    testfilter, cor, leftValue, rightValue, 1e-6 / testfilter$sr)
    compare3 <- stepR::stepblock(leftEnd = c(0, ret$left, ret$right), rightEnd = c(ret$left, ret$right, 1),
                                 value = c(leftValue, ret$value, rightValue), x0 = 0)
    class(compare3) <- c("localDeconvolution", class(compare3))
    compare <- list(compare1, compare2, compare3)
    attr(compare, "noDeconvolution") <- integer(0)
    attr(compare3, "noDeconvolution") <- integer(0)
    compareq <- 10
    attr(compareq, "n") <- 200L
    attr(compare, "q") <- compareq
    attr(compare3, "q") <- compareq
    
    ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                              method = "LR", lengths = 3, q = rep(10, 1))
    retall <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                 method = "LR", lengths = 3, q = rep(10, 1), output = "everyGrid")
    
    test <- retall[[3]]
    attr(test, "noDeconvolution") <- attr(retall, "noDeconvolution")
    attr(test, "q") <- attr(retall, "q")
    expect_identical(ret, test)
    expect_equal(ret, compare3)
    expect_equal(retall, compare)
  }
})

test_that("argument lengths is tested and works", {
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 100)
  testdata <- lowpassFilter::randomGeneration(n = 100, filter = testfilter, signal = rep(0, 100), noise = 1, seed = 6)
  testfit <- stepR::stepblock(value = mean(testdata), leftEnd = 0, rightEnd = 1, x0 = 0)
  
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE,
                                  lengths = "s", q = rep(10, 1)))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE,
                                  lengths = c(1:10, NA), q = rep(10, 11)))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE,
                                  lengths = 0:10, q = rep(10, 11)))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE,
                                  lengths = -1, q = rep(10, 1)))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE,
                                  lengths = c(1L, 101L), q = rep(10, 1)))

  
  expect_warning(ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                           suppressWarningNoDeconvolution = TRUE,
                                           lengths = c(3:5, 3), q = rep(4, 3)))
  expect_identical(ret, improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                           suppressWarningNoDeconvolution = TRUE,
                                           lengths = 3:5, q = rep(4, 3)))
  expect_identical(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                      suppressWarningNoDeconvolution = TRUE,
                                      lengths = 5:3, q = rep(4, 3)),
                   improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                      suppressWarningNoDeconvolution = TRUE,
                                      lengths = 3:5, q = rep(4, 3)))
  expect_identical(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                      suppressWarningNoDeconvolution = TRUE,
                                      lengths = 3:5 + 0.5, q = rep(4, 3)),
                   improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                      suppressWarningNoDeconvolution = TRUE,
                                      lengths = 3:5, q = rep(4, 3)))
  
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 100)
  testdata <- lowpassFilter::randomGeneration(n = 100, filter = testfilter, noise = 1, seed = "no",
                          signal = lowpassFilter::getConvolutionPeak(1:100 / testfilter$sr, 0.5, 0.53, 10, 0, 0,
                                                                                         testfilter))
  testfit <- stepR::stepblock(value = c(0, 1, 2, 0), leftEnd = c(0, 0.48, 0.5, 0.53),
                              rightEnd = c(0.48, 0.5, 0.53, 1), x0 = 0)
  
  testq <- getCritVal(n = length(testdata), family = "LR", filter = testfilter, alpha = 0.04,
                      r = 10, options = list(load = list(), simulation = "matrixIncreased"),
                      lengths = 21L, nq = 127)
  expect_identical(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                      method = "LR", lengths = 21, options = list(load = list()),
                                      r = 10L, suppressWarningNoDeconvolution = TRUE),
                   improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                      method = "LR", lengths = 21, q = testq,
                                      suppressWarningNoDeconvolution = TRUE))
})

test_that("argument q works and is tested", {
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 100)
  testdata <- lowpassFilter::randomGeneration(n = 100, filter = testfilter, signal = rep(0, 100), noise = 1, seed = 6)
  testfit <- stepR::stepblock(value = mean(testdata), leftEnd = 0, rightEnd = 1, x0 = 0)

  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE,
                                  q = "s", lengths = 3))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE,
                                  q = as.numeric(NA), lengths = 3))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE,
                                  q = Inf, lengths = 3))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE,
                                  q = c(1, 2), lengths = 3))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE,
                                  q = c(1, 2, 3), lengths = 1:4))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE,
                                  q = c("s", 1), lengths = c(3, 4)))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE,
                                  q = c(2, as.numeric(NA)), lengths = c(3, 4)))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE,
                                  q = c(1, Inf), lengths = c(3, 4)))
  
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE,
                                  q = 1, lengths = c(4, 5), penalty = "sqrt", n = 100))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE,
                                  q = 1, lengths = c(4, 5), penalty = "sqrt", family = "2Param"))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE,
                                  q = 1, lengths = c(4, 5), penalty = "sqrt", y = testdata))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE,
                                  q = 1, lengths = c(4, 5), penalty = "sqrt", mu = "test"))
  
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE,
                                  q = 1, lengths = c(4, 5)))

  expect_identical(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                      suppressWarningNoDeconvolution = TRUE,
                                      q = 1, lengths = c(4, 5), penalty = "sqrt"),
                   improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                      suppressWarningNoDeconvolution = TRUE, lengths = c(4, 5),
                                      q = stepR::critVal(q = 1, n = length(testdata), filter = testfilter,
                                                         family = "LR", penalty = "sqrt", lengths = c(4, 5))))
  expect_identical(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                      suppressWarningNoDeconvolution = TRUE,
                                      q = 3, lengths = c(4, 5), penalty = "log", nq = 120L),
                   improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                      suppressWarningNoDeconvolution = TRUE, lengths = c(4, 5),
                                      q = stepR::critVal(q = 3, n = length(testdata), filter = testfilter,
                                                         family = "LR", penalty = "log", nq = 120L,
                                                         lengths = c(4, 5))))
  expect_identical(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                      suppressWarningNoDeconvolution = TRUE,
                                      q = as.double(1:100), lengths = c(4, 5), penalty = "sqrt"),
                   improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                      suppressWarningNoDeconvolution = TRUE,
                                      q = c(4, 5), lengths = c(4, 5)))
})

test_that("argument alpha works and is tested", {
  skip_on_cran()
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 100)
  testdata <- lowpassFilter::randomGeneration(n = 100, filter = testfilter, signal = rep(0, 100), noise = 1, seed = 6)
  testfit <- stepR::stepblock(value = mean(testdata), leftEnd = 0, rightEnd = 1, x0 = 0)
  
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE,
                                  alpha = "s", lengths = 3))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE,
                                  alpha = as.numeric(NA), lengths = 3))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE,
                                  alpha = Inf, lengths = 3))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE,
                                  alpha = c(0.1, 0.05), lengths = 3))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE,
                                  alpha = 0, lengths = 3))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE,
                                  alpha = 1, lengths = 3))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE,
                                  alpha = -0.01, lengths = 3))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE,
                                  alpha = 1.01, lengths = 3))
  
  teststat <- stepR::monteCarloSimulation(family = "LR", n = length(testdata), filter = testfilter, r = 2)
  expect_identical(improveSmallScales(data = testdata, fit = testfit, filter = testfilter, stat = teststat,
                                      method = "LR", suppressWarningNoDeconvolution = TRUE, lengths = 3),  
                   improveSmallScales(data = testdata, fit = testfit, filter = testfilter, stat = teststat,
                                      method = "LR", suppressWarningNoDeconvolution = TRUE, lengths = 3,
                                      alpha = 0.04))
  
  testq <- getCritVal(family = "LR", n = length(testdata), stat = teststat, filter = testfilter, lengths = c(4, 5))
  expect_identical(improveSmallScales(data = testdata, fit = testfit, filter = testfilter, stat = teststat,
                                      method = "LR", suppressWarningNoDeconvolution = TRUE, lengths = c(4, 5)),  
                   improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                      method = "LR", suppressWarningNoDeconvolution = TRUE, lengths = c(4, 5),
                                      q = testq))
  testq <- getCritVal(family = "LR", n = length(testdata), stat = teststat, filter = testfilter, lengths = c(4, 5),
                      alpha = 0.09)
  expect_identical(improveSmallScales(data = testdata, fit = testfit, filter = testfilter, stat = teststat,
                                      method = "LR", suppressWarningNoDeconvolution = TRUE, lengths = c(4, 5),
                                      alpha = 0.09),  
                   improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                      method = "LR", suppressWarningNoDeconvolution = TRUE, lengths = c(4, 5),
                                      q = testq))

  
  teststat <- stepR::monteCarloSimulation(family = "2Param", n = length(testdata), filter = testfilter, r = 2,
                                          lengths = c(7, 10), output = "maximum", penalty = "log")
  testq <- getCritVal(family = "2Param", n = length(testdata), filter = testfilter, stat = teststat,
                      lengths = c(7, 10), penalty = "log", alpha = 0.13)
  expect_identical(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                      suppressWarningNoDeconvolution = TRUE, lengths = c(7, 10),
                                      penalty = "log", alpha = 0.13, stat = teststat),
                   improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                      suppressWarningNoDeconvolution = TRUE, lengths = c(7, 10),
                                      q = testq))
  
  suppressMessages(testq <- getCritVal(family = "LR", n = length(testdata), nq = 123, filter = testfilter, 
                                       lengths = c(3, 6), alpha = 0.0345, r = 4, weights = c(1 / 3, 2 / 3),
                                       options = list(simulation = "matrixIncreased"), seed = 10,
                                       rand.gen = function(data) rnorm(data$n), messages = 1))
  suppressMessages(ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter, method = "LR",
                                             suppressWarningNoDeconvolution = TRUE, lengths = c(3, 6),
                                             alpha = 0.0345, r = 4, weights = c(1 / 3, 2 / 3), nq = 123,
                                             options = list(simulation = "matrixIncreased"), seed = 10,
                                             rand.gen = function(data) rnorm(data$n), messages = 1))
  expect_identical(ret,
                   improveSmallScales(data = testdata, fit = testfit, filter = testfilter, method = "LR",
                                      suppressWarningNoDeconvolution = TRUE, lengths = c(3, 6), q = testq))
  
  
  testfit <- stepR::stepblock(value = c(mean(testdata), median(testdata)),
                              leftEnd = c(0, 0.5) + 45, rightEnd = c(0.5, 1) + 45, x0 = 45)
  testlocalList <- createLocalList(filter = testfilter, method = "2Param", lengths = 5)
  suppressMessages(testq <- getCritVal(family = "2Param", n = length(testdata), filter = testfilter, 
                                       lengths = 5, alpha = 0.12, r = 2, nq = 127L,
                                       thresholdLongSegment = 15L, localValue = mean,
                                       localVar = function(data) 1,
                                       options = list(load = list())))
  
  suppressMessages(ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter, method = "2Param",
                                             suppressWarningNoDeconvolution = TRUE, lengths = 5,
                                             alpha = 0.12, r = 2, startTime = 45, 
                                             thresholdLongSegment = 15L,
                                             localValue = mean, localVar = function(data) 1,
                                             regularization = c(2, 0.5),
                                             gridSize =  c(1 / testfilter$sr, 1 / 10 / testfilter$sr),
                                             windowFactorRefinement = 2,
                                             localList = testlocalList,
                                             options = list(load = list())))
  
  expect_identical(ret,
                   improveSmallScales(data = testdata, fit = testfit, filter = testfilter, method = "2Param",
                                      suppressWarningNoDeconvolution = TRUE, lengths = 5,
                                      q = testq, startTime = 45, 
                                      report = FALSE, regularization = c(2, 0.5),
                                      thresholdLongSegment = 15L,
                                      gridSize =  c(1 / testfilter$sr, 1 / 10 / testfilter$sr),
                                      windowFactorRefinement = 2,
                                      localValue = mean, localVar = function(data) 1))

  testStepR <- new.env()
  testfit <- stepR::stepblock(value = mean(testdata), leftEnd = 0, rightEnd = 1, x0 = 0)
  teststat <- stepR::monteCarloSimulation(100L, r = 100L, family = "LR", localVal = mean,
                                          filter = testfilter)
  expect_identical(improveSmallScales(data = testdata, fit = testfit, filter = testfilter, method = "LR",
                                      r = 100L, localValue = mean,
                                      options = list(simulation = "matrix", save = list(workspace = "matrix"),
                                                     load = list(), envir = testStepR),
                                      suppressWarningNoDeconvolution = TRUE),
                   improveSmallScales(data = testdata, fit = testfit, filter = testfilter, method = "LR",
                                      r = 100L, localValue = mean, stat = teststat,
                                      suppressWarningNoDeconvolution = TRUE, options = list()))
  expect_false(exists("critValStepRTab", envir = testStepR, inherits = FALSE))
  
  testlocalList <- createLocalList(filter = testfilter, method = "LR")
  teststat <- stepR::monteCarloSimulation(100L, r = 100L, family = "LR",
                                          filter = testfilter)
  testfile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".RDS")
  expect_identical(improveSmallScales(data = testdata, fit = testfit, filter = testfilter, method = "LR",
                                      r = 100L, nq = 100L, localList = testlocalList,
                                      options = list(save = list(RDSfile = testfile), load = list()),
                                      suppressWarningNoDeconvolution = TRUE),
                   improveSmallScales(data = testdata, fit = testfit, filter = testfilter, method = "LR",
                                      r = 100L, stat = teststat,
                                      suppressWarningNoDeconvolution = TRUE, options = list(save = list())))
  expect_identical(readRDS(testfile), teststat)
  expect_identical(improveSmallScales(data = testdata, fit = testfit, filter = testfilter, method = "LR",
                                      r = 50L, nq = 100L, suppressWarningNoDeconvolution = TRUE,
                                      options = list(save = list(), load = list(RDSfile = testfile))),
                   improveSmallScales(data = testdata, fit = testfit, filter = testfilter, method = "LR",
                                      r = 100L, stat = teststat,
                                      suppressWarningNoDeconvolution = TRUE, options = list(save = list())))
  unlink(testfile)
  

  ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter, method = "LR",
                            r = 10L, suppressWarningNoDeconvolution = TRUE, lengths = c(1:10, 21))
  expect_identical(length(attr(ret, "q")), 11L)
  expect_identical(attr(attr(ret, "q"), "n"), 127L)
  
  teststat <- stepR::monteCarloSimulation(n = 100L, r = 10L, family = "LR", filter = testfilter)
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter, method = "LR", stat = teststat,
                                  r = 10L, suppressWarningNoDeconvolution = TRUE, lengths = c(1:3, 25)))
  
  teststat <- stepR::monteCarloSimulation(n = 127L, r = 10L, family = "LR", filter = testfilter, lengths = c(1:3, 21),
                                          output = "maximum", penalty = "sqrt")
  expect_identical(improveSmallScales(data = testdata, fit = testfit, filter = testfilter, method = "LR",
                                      stat = teststat, penalty = "sqrt",
                                      r = 10L, suppressWarningNoDeconvolution = TRUE, lengths = c(1:3, 21)),
                   improveSmallScales(data = testdata, fit = testfit, filter = testfilter, method = "LR", penalty = "sqrt",
                                      r = 10L, suppressWarningNoDeconvolution = TRUE, lengths = c(1:3, 21)))
  
  expect_identical(improveSmallScales(data = testdata, fit = testfit, filter = testfilter, method = "LR",
                                      stat = teststat, penalty = "sqrt",
                                      r = 10L, suppressWarningNoDeconvolution = TRUE, lengths = c(1:3, 21),
                                      options = list(simulation = "matrixIncreased",
                                                     save = list(fileSystem = c("matrix", "vector",
                                                                                "matrixIncreased", "vectorIncreased")),
                                                     dirs = "testStepR")),
                   improveSmallScales(data = testdata, fit = testfit, filter = testfilter, method = "LR", penalty = "sqrt",
                                      r = 10L, suppressWarningNoDeconvolution = TRUE, lengths = c(1:3, 21)))
  expect_false(file_test(op = "-d", file.path(R.cache::getCacheRootPath(), "testStepR")))
})

test_that("r is tested and works", {
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 100)
  testdata <- lowpassFilter::randomGeneration(n = 100, filter = testfilter, signal = rep(0, 100), noise = 1, seed = 6)
  testfit <- stepR::stepblock(value = mean(testdata), leftEnd = 0, rightEnd = 1, x0 = 0)
  
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE,
                                  alpha = 0.04, lengths = 3, r = "s", method = "LR",
                                  options = list(load = list())))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE,
                                  alpha = 0.04, lengths = 3, r = Inf, method = "LR",
                                  options = list(load = list())))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE,
                                  alpha = 0.04, lengths = 3, r = c(2, 10), method = "LR",
                                  options = list(load = list())))
  
  expect_identical(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                      suppressWarningNoDeconvolution = TRUE,
                                      alpha = 0.04, lengths = c(4, 5, 6), r = 2.5, method = "LR",
                                      options = list(load = list())),
                   improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                      suppressWarningNoDeconvolution = TRUE, method = "LR",
                                      options = list(load = list()),
                                      alpha = 0.04, lengths = c(4, 5, 6), r = 2L))
    
  teststat <- stepR::monteCarloSimulation(n = 100L, family = "LR", filter = testfilter, r = 10L)
  expect_identical(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                      suppressWarningNoDeconvolution = TRUE, stat = teststat,
                                      alpha = 0.04, lengths = c(2, 7, 16), method = "LR",
                                      options = list(load = list())),
                   improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                      suppressWarningNoDeconvolution = TRUE, method = "LR",
                                      options = list(load = list(), simulation = "matrix"),
                                      alpha = 0.04, lengths = c(2, 7, 16), r = 10L))
  
  expect_identical(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                      suppressWarningNoDeconvolution = TRUE,
                                      alpha = 0.04, lengths = 3, r = 2, method = "LR",
                                      options = list(load = list())),
                   suppressMessages(
                     improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                        suppressWarningNoDeconvolution = TRUE, method = "LR",
                                        options = list(load = list()),
                                        alpha = 0.04, lengths = 3, r = 2, regularization = 1, report = TRUE)))
})

test_that("argument startTime is tested and works", {
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 100)
  testdata <- lowpassFilter::randomGeneration(n = 100, filter = testfilter, signal = rep(0, 100), noise = 1, seed = 1)
  testfit <- stepR::stepblock(value = mean(testdata), leftEnd = 23.5465, rightEnd = 24.5465, x0 = 23.5465)
  testq <- 1e9

  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE,
                                  q = testq, lengths = 3, startTime = "s"))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE,
                                  q = testq, lengths = 3, startTime = c(0, 1)))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE,
                                  q = testq, lengths = 3, startTime = Inf))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE,
                                  q = testq, lengths = 3, startTime = as.numeric(NA)))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE,
                                  q = testq, lengths = 3, startTime = NULL))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE,
                                  q = testq, lengths = 3, startTime = 23.53))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE,
                                  q = testq, lengths = 3, startTime = 23.5565))

  ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                            method = "LR", suppressWarningNoDeconvolution = TRUE,
                            q = testq, lengths = 3, startTime = 23.5465)
  retall <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                               method = "LR", suppressWarningNoDeconvolution = TRUE, output = "everyGrid",
                               q = testq, lengths = 3, startTime = 23.5465)
  
  comparefit <- stepR::stepblock(value = mean(testdata), leftEnd = 0, rightEnd = 1, x0 = 0)
  compare <- improveSmallScales(data = testdata, fit = comparefit, filter = testfilter,
                                method = "LR", suppressWarningNoDeconvolution = TRUE,
                                q = testq, lengths = 3, startTime = 0)
  compareall <- improveSmallScales(data = testdata, fit = comparefit, filter = testfilter,
                                   method = "LR", suppressWarningNoDeconvolution = TRUE, output = "everyGrid",
                                   q = testq, lengths = 3, startTime = 0)
  compare$leftEnd <- compare$leftEnd + 23.5465
  compare$rightEnd <- compare$rightEnd + 23.5465
  attr(compare, "x0") <- 23.5465
  for (i in 1:3) {
    compareall[[i]]$leftEnd <- compareall[[i]]$leftEnd + 23.5465
    compareall[[i]]$rightEnd <- compareall[[i]]$rightEnd + 23.5465
    attr(compareall[[i]], "x0") <- 23.5465
  }
  expect_identical(ret, compare)
  expect_identical(retall, compareall)
  
  # jump
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 100)
  testdata <- lowpassFilter::randomGeneration(n = 100, filter = testfilter, noise = 1, seed = "no",
                         signal = lowpassFilter::getConvolutionJump(1:100 / testfilter$sr, 0.5, 0, 1, testfilter))
  testfit <- stepR::stepblock(value = c(0, 1), leftEnd = c(0, 0.5) - 1.23, rightEnd = c(0.5, 1) - 1.23, x0 =  -1.23)
  
  ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                            method = "2Param", suppressWarningNoDeconvolution = TRUE,
                            q = testq, lengths = 3, startTime = -1.23)
  retall <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                               method = "2Param", suppressWarningNoDeconvolution = TRUE, output = "everyGrid",
                               q = testq, lengths = 3, startTime = -1.23)
  
  comparefit <- stepR::stepblock(value = c(0, 1), leftEnd = c(0, 0.5), rightEnd = c(0.5, 1), x0 =  0)
  compare <- improveSmallScales(data = testdata, fit = comparefit, filter = testfilter,
                                method = "2Param", suppressWarningNoDeconvolution = TRUE,
                                q = testq, lengths = 3, startTime = 0)
  compareall <- improveSmallScales(data = testdata, fit = comparefit, filter = testfilter,
                                   method = "2Param", suppressWarningNoDeconvolution = TRUE, output = "everyGrid",
                                   q = testq, lengths = 3, startTime = 0)
  compare$leftEnd <- compare$leftEnd - 1.23
  compare$rightEnd <- compare$rightEnd - 1.23
  attr(compare, "x0") <- -1.23
  for (i in 1:3) {
    compareall[[i]]$leftEnd <- compareall[[i]]$leftEnd - 1.23
    compareall[[i]]$rightEnd <- compareall[[i]]$rightEnd - 1.23
    attr(compareall[[i]], "x0") <- -1.23
  }
  expect_equal(ret, compare)
  expect_equal(retall, compareall)
  
  # peak
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 100)
  testdata <- lowpassFilter::randomGeneration(n = 100, filter = testfilter, noise = 1, seed = "no",
                              signal = lowpassFilter::getConvolutionPeak(1:100 / testfilter$sr, 0.5, 0.53, 10, 0, 0,
                                              testfilter))
  testfit <- stepR::stepblock(value = c(0, 1, 0), leftEnd = c(0, 0.5, 0.53) + 0.2,
                              rightEnd = c(0.5, 0.53, 1) + 0.2, x0 = 0.2)
  
  ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                            method = "2Param", suppressWarningNoDeconvolution = TRUE,
                            q = testq, lengths = 3, startTime = 0.2)
  retall <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                               method = "2Param", suppressWarningNoDeconvolution = TRUE, output = "everyGrid",
                               q = testq, lengths = 3, startTime = 0.2)
  
  comparefit <- stepR::stepblock(value = c(0, 1, 0), leftEnd = c(0, 0.5, 0.53),
                                 rightEnd = c(0.5, 0.53, 1), x0 = 0)
  compare <- improveSmallScales(data = testdata, fit = comparefit, filter = testfilter,
                                method = "2Param", suppressWarningNoDeconvolution = TRUE,
                                q = testq, lengths = 3, startTime = 0)
  compareall <- improveSmallScales(data = testdata, fit = comparefit, filter = testfilter,
                                   method = "2Param", suppressWarningNoDeconvolution = TRUE, output = "everyGrid",
                                   q = testq, lengths = 3, startTime = 0)
  compare$leftEnd <- compare$leftEnd + 0.2
  compare$rightEnd <- compare$rightEnd + 0.2
  attr(compare, "x0") <- 0.2
  for (i in 1:3) {
    compareall[[i]]$leftEnd <- compareall[[i]]$leftEnd + 0.2
    compareall[[i]]$rightEnd <- compareall[[i]]$rightEnd + 0.2
    attr(compareall[[i]], "x0") <- 0.2
  }
  expect_equal(ret, compare)
  expect_equal(retall, compareall)
  
  
  # detected peak
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 200)
  testdata <- lowpassFilter::randomGeneration(n = 200, filter = testfilter, noise = 1, seed = "no",
                           signal = lowpassFilter::getConvolutionPeak(1:200 / testfilter$sr, 0.5, 0.515, 10, 0, 0,
                                                    testfilter))
  testfit <- stepR::stepblock(value = c(mean(testdata)), leftEnd = c(0.5),
                              rightEnd = c(1.5), x0 = 0.5)
  testq <- getCritVal(n = length(testdata), filter = testfilter, family = "LR", r = 2, lengths = 3)
  
  ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                            method = "LR", suppressWarningNoDeconvolution = TRUE,
                            q = testq, lengths = 3, startTime = 0.5)
  retall <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                               method = "LR", suppressWarningNoDeconvolution = TRUE, output = "everyGrid",
                               q = testq, lengths = 3, startTime = 0.5)
  
  comparefit <- stepR::stepblock(value = c(mean(testdata)), leftEnd = c(0),
                                 rightEnd = c(1), x0 = 0)
  compare <- improveSmallScales(data = testdata, fit = comparefit, filter = testfilter,
                                method = "LR", suppressWarningNoDeconvolution = TRUE,
                                q = testq, lengths = 3, startTime = 0)
  compareall <- improveSmallScales(data = testdata, fit = comparefit, filter = testfilter,
                                   method = "LR", suppressWarningNoDeconvolution = TRUE, output = "everyGrid",
                                   q = testq, lengths = 3, startTime = 0)
  compare$leftEnd <- compare$leftEnd + 0.5
  compare$rightEnd <- compare$rightEnd + 0.5
  attr(compare, "x0") <- 0.5
  for (i in 1:3) {
    compareall[[i]]$leftEnd <- compareall[[i]]$leftEnd + 0.5
    compareall[[i]]$rightEnd <- compareall[[i]]$rightEnd + 0.5
    attr(compareall[[i]], "x0") <- 0.5
  }
  expect_equal(ret, compare)
  expect_equal(retall, compareall)
})

test_that("thresholdLongSegment is tested and works", {
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 31)
  testdata <- lowpassFilter::randomGeneration(n = 31, filter = testfilter, signal = rep(0, 31), noise = 1, seed = "no")
  testfit <- stepR::stepblock(value = mean(testdata), leftEnd = 0, rightEnd = 1, x0 = 0)
  
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE, method = "LR",
                                  lengths = 3, q = 100, thresholdLongSegment = "s"))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE, method = "LR",
                                  lengths = 3, q = 100, thresholdLongSegment = c(10, 20)))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE, method = "LR",
                                  lengths = 3, q = 100, thresholdLongSegment = Inf))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE, method = "LR",
                                  lengths = 3, q = 100, thresholdLongSegment = 0L))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE, method = "LR",
                                  lengths = 3, q = 100, thresholdLongSegment = -1L))

  ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                            suppressWarningNoDeconvolution = TRUE, method = "LR",
                            lengths = 3, q = 100)
  expect_identical(attr(ret, "noDeconvolution"), integer(0))
  
  expect_identical(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                      suppressWarningNoDeconvolution = TRUE, method = "LR",
                                      lengths = 3, q = 100, thresholdLongSegment = 10L), ret)
  expect_identical(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                      suppressWarningNoDeconvolution = TRUE, method = "LR",
                                      lengths = 3, q = 100, thresholdLongSegment = 10), ret)
  expect_identical(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                      suppressWarningNoDeconvolution = TRUE, method = "LR",
                                      lengths = 3, q = 100, thresholdLongSegment = 10.5), ret)

  
  ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                            suppressWarningNoDeconvolution = TRUE, method = "LR",
                            lengths = 3, q = 100, thresholdLongSegment = 11L)
  expect_identical(attr(ret, "noDeconvolution"), 1L)
  

  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 30)
  testdata <- lowpassFilter::randomGeneration(n = 30, filter = testfilter, signal = rep(0, 30), noise = 1, seed = "no")
  testfit <- stepR::stepblock(value = mean(testdata), leftEnd = 0, rightEnd = 1, x0 = 0)

  ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                            suppressWarningNoDeconvolution = TRUE, method = "LR",
                            lengths = 3, q = 100)
  expect_identical(attr(ret, "noDeconvolution"), 1L)
  
  ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                            suppressWarningNoDeconvolution = TRUE, method = "LR",
                            lengths = 3, q = 100, thresholdLongSegment = 9L)
  expect_identical(attr(ret, "noDeconvolution"), integer(0))
  
  
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 46)
  testdata <- lowpassFilter::randomGeneration(n = 46, filter = testfilter, signal = rep(0, 46), noise = 1, seed = "no")
  testfit <- stepR::stepblock(value = mean(testdata), leftEnd = 0, rightEnd = 1, x0 = 0)
  
  ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                            suppressWarningNoDeconvolution = TRUE, method = "2Param",
                            lengths = 3, q = 100)
  expect_identical(attr(ret, "noDeconvolution"), integer(0))
  
  ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                            suppressWarningNoDeconvolution = TRUE, method = "2Param",
                            lengths = 3, q = 100, thresholdLongSegment = 26L)
  expect_identical(attr(ret, "noDeconvolution"), 1L)
  
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 45)
  testdata <- lowpassFilter::randomGeneration(n = 45, filter = testfilter, signal = rep(0, 45), noise = 1, seed = "no")
  testfit <- stepR::stepblock(value = mean(testdata), leftEnd = 0, rightEnd = 1, x0 = 0)
  
  ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                            suppressWarningNoDeconvolution = TRUE, method = "2Param",
                            lengths = 3, q = 100)
  expect_identical(attr(ret, "noDeconvolution"), 1L)
  
  ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                            suppressWarningNoDeconvolution = TRUE, method = "2Param",
                            lengths = 3, q = 100, thresholdLongSegment = 24L)
  expect_identical(attr(ret, "noDeconvolution"), integer(0))
})

test_that("argument localValue is tested and works", {
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 100)
  testdata <- lowpassFilter::randomGeneration(n = 100, filter = testfilter, signal = rep(0, 100), noise = 1, seed = "no")
  testfit <- stepR::stepblock(value = mean(testdata), leftEnd = 0, rightEnd = 1, x0 = 0)
  
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE, method = "LR",
                                  lengths = 3, q = 100, localValue = 1))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE, method = "LR",
                                  lengths = 3, q = 100, localValue = function() {1}))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE, method = "LR",
                                  lengths = 3, q = 100, localValue = function(x) {Inf}))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE, method = "LR",
                                  lengths = 3, q = 100, localValue = function(x) {c(1, 2)}))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE, method = "LR",
                                  lengths = 3, q = 100, localValue = function(x) {NULL}))
  
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE, method = "2Param",
                                  lengths = 3, q = 100, localValue = 1))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE, method = "2Param",
                                  lengths = 3, q = 100, localValue = function() {1}))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE, method = "2Param",
                                  lengths = 3, q = 100, localValue = function(x) {Inf}))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE, method = "2Param",
                                  lengths = 3, q = 100, localValue = function(x) {c(1, 2)}))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE, method = "2Param",
                                  lengths = 3, q = 100, localValue = function(x) {NULL}))

  ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                            suppressWarningNoDeconvolution = TRUE, method = "LR",
                            lengths = 3, q = 100)
  expect_identical(ret$value, stats::median(testdata[11:89]))
  
  ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                            suppressWarningNoDeconvolution = TRUE, method = "2Param",
                            lengths = 3, q = 100, localValue = mean)
  expect_identical(ret$value, mean(testdata[11:89]))
  
  ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                            suppressWarningNoDeconvolution = TRUE, method = "LR",
                            lengths = 3, q = 100, localValue = function(x) {1})
  expect_identical(ret$value, 1)
})

test_that("argument localVar is tested and works", {
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 100)
  testdata <- lowpassFilter::randomGeneration(n = 100, filter = testfilter, signal = rep(0, 100), noise = 1, seed = "no")
  testfit <- stepR::stepblock(value = mean(testdata), leftEnd = 0, rightEnd = 1, x0 = 0)

  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE, method = "2Param",
                                  lengths = 3, q = 100, localVar = 1))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE, method = "2Param",
                                  lengths = 3, q = 100, localVar = function() {1}))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE, method = "2Param",
                                  lengths = 3, q = 100, localVar = function(x) {Inf}))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE, method = "2Param",
                                  lengths = 3, q = 100, localVar = function(x) {c(1, 2)}))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE, method = "2Param",
                                  lengths = 3, q = 100, localVar = function(x) {NULL}))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE, method = "2Param",
                                  lengths = 3, q = 100, localVar = function(x) {-1}))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  suppressWarningNoDeconvolution = TRUE, method = "2Param",
                                  lengths = 3, q = 100, localVar = function(x) {0}))
  
  
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 200)
  testdata <- lowpassFilter::randomGeneration(n = 200, filter = testfilter, noise = 1, seed = "no",
                       signal = lowpassFilter::getConvolutionPeak(1:200 / testfilter$sr, 0.5, 0.515, 10, 0, 0,
                                                                                         testfilter))
  testfit <- stepR::stepblock(value = c(mean(testdata)), leftEnd = c(0),
                              rightEnd = c(1), x0 = 0)
  
  comparefit <- stepR::stepblock(median(testdata[testfilter$len:(200 - testfilter$len)]),
                                 leftEnd = 0, rightEnd = 1, x0 = 0)
  comparefit$var <- 1e6
  
  stat <- statAll(y = testdata, filter = testfilter, fit = comparefit,
                  singleStat = singleStat2Param, len = 3, add = integer(0))
  
  if (all(stat < 40)) {
    ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                              method = "2Param", lengths = 3, q = rep(40, 1), localVar = function(x) 1e6)
    
    expect_identical(length(ret$leftEnd), 1L)
  } else {
    if (all(diff(which(stat > 40)) == 1)) {
      li <- which.max(stat)
      ri <- li + 3
      leftValue <- median(testdata[testfilter$len:(li - 6L)])
      rightValue <- median(testdata[(ri + 6L + testfilter$len):(200 - testfilter$len)])
      cor <- testfilter$acf
      cor[1] <- 2
      ret <- testPeak((li - 6L):(li + 6L) / testfilter$sr, (ri - 6L):(ri + 6L) / testfilter$sr,
                      (li - 5L):(ri + 5L + testfilter$len) / testfilter$sr,
                      testdata[(li - 5L):(ri + 5L + testfilter$len)],
                      testfilter, cor, leftValue, rightValue, 1e-6 / testfilter$sr)
      compare1 <- stepR::stepblock(leftEnd = c(0, ret$left, ret$right), rightEnd = c(ret$left, ret$right, 1),
                                   value = c(leftValue, ret$value, rightValue), x0 = 0)
      class(compare1) <- c("localDeconvolution", class(compare1))
      ret <- testPeak(seq(ret$left - 1 / testfilter$sr, ret$left + 1 / testfilter$sr, 0.1 / testfilter$sr),
                      seq(ret$right - 1 / testfilter$sr, ret$right + 1 / testfilter$sr, 0.1 / testfilter$sr),
                      (li - 5L):(ri + 5L + testfilter$len) / testfilter$sr,
                      testdata[(li - 5L):(ri + 5L + testfilter$len)],
                      testfilter, cor, leftValue, rightValue, 1e-6 / testfilter$sr)
      compare2 <- stepR::stepblock(leftEnd = c(0, ret$left, ret$right), rightEnd = c(ret$left, ret$right, 1),
                                   value = c(leftValue, ret$value, rightValue), x0 = 0)
      class(compare2) <- c("localDeconvolution", class(compare2))
      ret <- testPeak(seq(ret$left - 0.1 / testfilter$sr, ret$left + 0.1 / testfilter$sr, 0.01 / testfilter$sr),
                      seq(ret$right - 0.1 / testfilter$sr, ret$right + 0.1 / testfilter$sr, 0.01 / testfilter$sr),
                      (li - 5L):(ri + 5L + testfilter$len) / testfilter$sr,
                      testdata[(li - 5L):(ri + 5L + testfilter$len)],
                      testfilter, cor, leftValue, rightValue, 1e-6 / testfilter$sr)
      compare3 <- stepR::stepblock(leftEnd = c(0, ret$left, ret$right), rightEnd = c(ret$left, ret$right, 1),
                                   value = c(leftValue, ret$value, rightValue), x0 = 0)
      class(compare3) <- c("localDeconvolution", class(compare3))
      compare <- list(compare1, compare2, compare3)
      attr(compare, "noDeconvolution") <- integer(0)
      attr(compare3, "noDeconvolution") <- integer(0)
      compareq <- 40
      attr(compareq, "n") <- 200L
      attr(compare, "q") <- compareq
      attr(compare3, "q") <- compareq
      
      ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                method = "2Param", lengths = 3, q = rep(40, 1), localVar = function(x) 1e6)
      retall <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                   method = "2Param", lengths = 3, q = rep(40, 1),
                                   output = "everyGrid", localVar = function(x) 1e6)
      
      test <- retall[[3]]
      attr(test, "noDeconvolution") <- attr(retall, "noDeconvolution")
      attr(test, "q") <- attr(retall, "q")
      expect_identical(ret, test)
      expect_equal(ret, compare3)
      expect_equal(retall, compare)
    }
  }
})

test_that("argument regularization is tested and works", {
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 100)
  testdata <- lowpassFilter::randomGeneration(n = 100, filter = testfilter, noise = 1, seed = "no",
                 signal = lowpassFilter::getConvolutionPeak(1:100 / testfilter$sr, 0.5, 0.53, 10, 0, 0,
                                                    testfilter))
  testfit <- stepR::stepblock(value = c(0, 1, 0), leftEnd = c(0, 0.5, 0.53),
                              rightEnd = c(0.5, 0.53, 1), x0 = 0)
  
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  method = "2Param", lengths = 1, q = rep(1e9, 1),
                                  regularization = "s"))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  method = "2Param", lengths = 1, q = rep(1e9, 1),
                                  regularization = Inf))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  method = "2Param", lengths = 1, q = rep(1e9, 1),
                                  regularization = NULL))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  method = "2Param", lengths = 1, q = rep(1e9, 1),
                                  regularization = c(1, "s", 0.2)))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  method = "2Param", lengths = 1, q = rep(1e9, 1),
                                  regularization = c(2, as.numeric(NA), 0.1)))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  method = "2Param", lengths = 1, q = rep(1e9, 1),
                                  regularization = list(c(1, 0.6, 0.1), c(1, 0.5))))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  method = "2Param", lengths = 1, q = rep(1e9, 1),
                                  regularization = list(c(1, 0.5), c(1, Inf, 0.1), 2)))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  method = "2Param", lengths = 1, q = rep(1e9, 1),
                                  regularization = c(2, 1.5, 1.25)))
  
  # regularization <- 2
  leftValue <- median(testdata[testfilter$len:(50 - testfilter$len)])
  rightValue <- median(testdata[(53 + testfilter$len):(100 - testfilter$len)])
  cor <- testfilter$acf
  cor[1] <- 3
  ret <- testPeak(39:50 / testfilter$sr, 42:53 / testfilter$sr, 40:63 / testfilter$sr, 
                  testdata[40:63], testfilter, cor, leftValue, rightValue, 1e-6 / testfilter$sr)
  compare1 <- stepR::stepblock(leftEnd = c(0, ret$left, ret$right), rightEnd = c(ret$left, ret$right, 1),
                               value = c(leftValue, ret$value, rightValue), x0 = 0)
  class(compare1) <- c("localDeconvolution", class(compare1))
  ret <- testPeak(seq(ret$left - 1 / testfilter$sr, ret$left + 1 / testfilter$sr, 0.1 / testfilter$sr),
                  seq(ret$right - 1 / testfilter$sr, ret$right + 1 / testfilter$sr, 0.1 / testfilter$sr),
                  40:63 / testfilter$sr, testdata[40:63], testfilter, cor, leftValue, rightValue,
                  1e-6 / testfilter$sr)
  compare2 <- stepR::stepblock(leftEnd = c(0, ret$left, ret$right), rightEnd = c(ret$left, ret$right, 1),
                               value = c(leftValue, ret$value, rightValue), x0 = 0)
  class(compare2) <- c("localDeconvolution", class(compare2))
  ret <- testPeak(seq(ret$left - 0.1 / testfilter$sr, ret$left + 0.1 / testfilter$sr, 0.01 / testfilter$sr),
                  seq(ret$right - 0.1 / testfilter$sr, ret$right + 0.1 / testfilter$sr, 0.01 / testfilter$sr),
                  40:63 / testfilter$sr, testdata[40:63], testfilter, cor, leftValue, rightValue,
                  1e-6 / testfilter$sr)
  compare3 <- stepR::stepblock(leftEnd = c(0, ret$left, ret$right), rightEnd = c(ret$left, ret$right, 1),
                               value = c(leftValue, ret$value, rightValue), x0 = 0)
  class(compare3) <- c("localDeconvolution", class(compare3))
  compare <- list(compare1, compare2, compare3)
  attr(compare, "noDeconvolution") <- integer(0)
  attr(compare3, "noDeconvolution") <- integer(0)
  compareq <- 1e9
  attr(compareq, "n") <- 100L
  attr(compare, "q") <- compareq
  attr(compare3, "q") <- compareq
  
  ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                            method = "2Param", lengths = 1, q = rep(1e9, 1),
                            regularization = 2)
  retall <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                            method = "2Param", lengths = 1, q = rep(1e9, 1),
                            regularization = 2, output = "everyGrid")
  
  test <- retall[[3]]
  attr(test, "noDeconvolution") <- integer(0)
  attr(test, "q") <- attr(retall, "q")
  expect_identical(ret, test)
  expect_equal(ret, compare3)
  expect_equal(retall, compare)
  
  # regularization <- c(1, 0.5, 0.25)
  leftValue <- median(testdata[testfilter$len:(50 - testfilter$len)])
  rightValue <- median(testdata[(53 + testfilter$len):(100 - testfilter$len)])
  cor <- testfilter$acf
  cor[1:3] <- cor[1:3] + c(1, 0.5, 0.25)
  ret <- testPeak(39:50 / testfilter$sr, 42:53 / testfilter$sr, 40:63 / testfilter$sr, 
                  testdata[40:63], testfilter, cor, leftValue, rightValue, 1e-6 / testfilter$sr)
  compare1 <- stepR::stepblock(leftEnd = c(0, ret$left, ret$right), rightEnd = c(ret$left, ret$right, 1),
                               value = c(leftValue, ret$value, rightValue), x0 = 0)
  class(compare1) <- c("localDeconvolution", class(compare1))
  ret <- testPeak(seq(ret$left - 1 / testfilter$sr, ret$left + 1 / testfilter$sr, 0.1 / testfilter$sr),
                  seq(ret$right - 1 / testfilter$sr, ret$right + 1 / testfilter$sr, 0.1 / testfilter$sr),
                  40:63 / testfilter$sr, testdata[40:63], testfilter, cor, leftValue, rightValue,
                  1e-6 / testfilter$sr)
  compare2 <- stepR::stepblock(leftEnd = c(0, ret$left, ret$right), rightEnd = c(ret$left, ret$right, 1),
                               value = c(leftValue, ret$value, rightValue), x0 = 0)
  class(compare2) <- c("localDeconvolution", class(compare2))
  ret <- testPeak(seq(ret$left - 0.1 / testfilter$sr, ret$left + 0.1 / testfilter$sr, 0.01 / testfilter$sr),
                  seq(ret$right - 0.1 / testfilter$sr, ret$right + 0.1 / testfilter$sr, 0.01 / testfilter$sr),
                  40:63 / testfilter$sr, testdata[40:63], testfilter, cor, leftValue, rightValue,
                  1e-6 / testfilter$sr)
  compare3 <- stepR::stepblock(leftEnd = c(0, ret$left, ret$right), rightEnd = c(ret$left, ret$right, 1),
                               value = c(leftValue, ret$value, rightValue), x0 = 0)
  class(compare3) <- c("localDeconvolution", class(compare3))
  compare <- list(compare1, compare2, compare3)
  attr(compare, "noDeconvolution") <- integer(0)
  attr(compare3, "noDeconvolution") <- integer(0)
  compareq <- 1e9
  attr(compareq, "n") <- 100L
  attr(compare, "q") <- compareq
  attr(compare3, "q") <- compareq
  
  ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                            method = "2Param", lengths = 1, q = rep(1e9, 1),
                            regularization = c(1, 0.5, 0.25))
  retall <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                               method = "2Param", lengths = 1, q = rep(1e9, 1),
                               regularization = c(1, 0.5, 0.25), output = "everyGrid")
  
  test <- retall[[3]]
  attr(test, "noDeconvolution") <- integer(0)
  attr(test, "q") <- attr(retall, "q")
  expect_identical(ret, test)
  expect_equal(ret, compare3)
  expect_equal(retall, compare)
 
  # regularization <- list(c(3), c(2, 1), c(2, 1, 0.5))
  leftValue <- median(testdata[testfilter$len:(50 - testfilter$len)])
  rightValue <- median(testdata[(53 + testfilter$len):(100 - testfilter$len)])
  cor <- testfilter$acf
  cor[1] <- 4
  ret <- testPeak(39:50 / testfilter$sr, 42:53 / testfilter$sr, 40:63 / testfilter$sr, 
                  testdata[40:63], testfilter, cor, leftValue, rightValue, 1e-6 / testfilter$sr)
  compare1 <- stepR::stepblock(leftEnd = c(0, ret$left, ret$right), rightEnd = c(ret$left, ret$right, 1),
                               value = c(leftValue, ret$value, rightValue), x0 = 0)
  class(compare1) <- c("localDeconvolution", class(compare1))
  cor <- testfilter$acf
  cor[1:2] <- cor[1:2] + c(2, 1)
  ret <- testPeak(seq(ret$left - 1 / testfilter$sr, ret$left + 1 / testfilter$sr, 0.1 / testfilter$sr),
                  seq(ret$right - 1 / testfilter$sr, ret$right + 1 / testfilter$sr, 0.1 / testfilter$sr),
                  40:63 / testfilter$sr, testdata[40:63], testfilter, cor, leftValue, rightValue,
                  1e-6 / testfilter$sr)
  compare2 <- stepR::stepblock(leftEnd = c(0, ret$left, ret$right), rightEnd = c(ret$left, ret$right, 1),
                               value = c(leftValue, ret$value, rightValue), x0 = 0)
  class(compare2) <- c("localDeconvolution", class(compare2))
  cor <- testfilter$acf
  cor[1:3] <- cor[1:3] + c(2, 1, 0.5)
  ret <- testPeak(seq(ret$left - 0.1 / testfilter$sr, ret$left + 0.1 / testfilter$sr, 0.01 / testfilter$sr),
                  seq(ret$right - 0.1 / testfilter$sr, ret$right + 0.1 / testfilter$sr, 0.01 / testfilter$sr),
                  40:63 / testfilter$sr, testdata[40:63], testfilter, cor, leftValue, rightValue,
                  1e-6 / testfilter$sr)
  compare3 <- stepR::stepblock(leftEnd = c(0, ret$left, ret$right), rightEnd = c(ret$left, ret$right, 1),
                               value = c(leftValue, ret$value, rightValue), x0 = 0)
  class(compare3) <- c("localDeconvolution", class(compare3))
  compare <- list(compare1, compare2, compare3)
  attr(compare, "noDeconvolution") <- integer(0)
  attr(compare3, "noDeconvolution") <- integer(0)
  compareq <- 1e9
  attr(compareq, "n") <- 100L
  attr(compare, "q") <- compareq
  attr(compare3, "q") <- compareq
  
  ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                            method = "2Param", lengths = 1, q = rep(1e9, 1),
                            regularization = list(c(3), c(2, 1), c(2, 1, 0.5)))
  retall <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                               method = "2Param", lengths = 1, q = rep(1e9, 1),
                               regularization = list(c(3), c(2, 1), c(2, 1, 0.5)), output = "everyGrid")
  
  test <- retall[[3]]
  attr(test, "noDeconvolution") <- integer(0)
  attr(test, "q") <- attr(ret, "q")
  expect_identical(ret, test)
  expect_equal(ret, compare3)
  expect_equal(retall, compare)
})

test_that("argument gridSize is tested and works", {
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 100)
  testdata <- lowpassFilter::randomGeneration(n = 100, filter = testfilter, noise = 1, seed = "no",
                        signal = lowpassFilter::getConvolutionPeak(1:100 / testfilter$sr, 0.5, 0.53, 10, 0, 0,
                                               testfilter))
  testfit <- stepR::stepblock(value = c(0, 1, 0), leftEnd = c(0, 0.5, 0.53),
                              rightEnd = c(0.5, 0.53, 1), x0 = 0)
  
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  method = "2Param", lengths = 1, q = rep(1e9, 1),
                                  gridSize = c(1, "s", 0.01)))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  method = "2Param", lengths = 1, q = rep(1e9, 1),
                                  gridSize = c(1, Inf, 0.01)))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  method = "2Param", lengths = 1, q = rep(1e9, 1),
                                  gridSize = c(1, as.numeric(NA), 0.01)))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  method = "2Param", lengths = 1, q = rep(1e9, 1),
                                  gridSize = c(1, 0.1) / testfilter$sr,
                                  regularization = list(2, 1, 1)))
  
  expect_warning(ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                           method = "2Param", lengths = 1, q = rep(1e9, 1),
                                           gridSize = c(0.5, 0.1, 0.01) / testfilter$sr))
  expect_identical(ret, improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                           method = "2Param", lengths = 1, q = rep(1e9, 1)))
  
  leftValue <- median(testdata[testfilter$len:(50 - testfilter$len)])
  rightValue <- median(testdata[(53 + testfilter$len):(100 - testfilter$len)])
  cor <- testfilter$acf
  cor[1] <- 2
  ret <- testPeak(39:50 / testfilter$sr, 42:53 / testfilter$sr, 40:63 / testfilter$sr, 
                  testdata[40:63], testfilter, cor, leftValue, rightValue, 1e-6 / testfilter$sr)
  compare1 <- stepR::stepblock(leftEnd = c(0, ret$left, ret$right), rightEnd = c(ret$left, ret$right, 1),
                               value = c(leftValue, ret$value, rightValue), x0 = 0)
  class(compare1) <- c("localDeconvolution", class(compare1))
  ret <- testPeak(seq(ret$left - 1 / testfilter$sr, ret$left + 1 / testfilter$sr, 0.5 / testfilter$sr),
                  seq(ret$right - 1 / testfilter$sr, ret$right + 1 / testfilter$sr, 0.5 / testfilter$sr),
                  40:63 / testfilter$sr, testdata[40:63], testfilter, cor, leftValue, rightValue,
                  1e-6 / testfilter$sr)
  compare2 <- stepR::stepblock(leftEnd = c(0, ret$left, ret$right), rightEnd = c(ret$left, ret$right, 1),
                               value = c(leftValue, ret$value, rightValue), x0 = 0)
  class(compare2) <- c("localDeconvolution", class(compare2))
  ret <- testPeak(seq(ret$left - 0.5 / testfilter$sr, ret$left + 0.5 / testfilter$sr, 0.1 / testfilter$sr),
                  seq(ret$right - 0.5 / testfilter$sr, ret$right + 0.5 / testfilter$sr, 0.1 / testfilter$sr),
                  40:63 / testfilter$sr, testdata[40:63], testfilter, cor, leftValue, rightValue,
                  1e-6 / testfilter$sr)
  compare3 <- stepR::stepblock(leftEnd = c(0, ret$left, ret$right), rightEnd = c(ret$left, ret$right, 1),
                               value = c(leftValue, ret$value, rightValue), x0 = 0)
  class(compare3) <- c("localDeconvolution", class(compare3))
  compare <- list(compare1, compare2, compare3)
  attr(compare, "noDeconvolution") <- integer(0)
  attr(compare3, "noDeconvolution") <- integer(0)
  compareq <- 1e9
  attr(compareq, "n") <- 100L
  attr(compare, "q") <- compareq
  attr(compare3, "q") <- compareq
  
  ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                            method = "2Param", lengths = 1, q = rep(1e9, 1),
                            gridSize = c(1, 0.5, 0.1) / testfilter$sr)
  retall <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                               method = "2Param", lengths = 1, q = rep(1e9, 1),
                               gridSize = c(1, 0.5, 0.1) / testfilter$sr, output = "everyGrid")
  
  test <- retall[[3]]
  attr(test, "noDeconvolution") <- integer(0)
  attr(test, "q") <- attr(retall, "q")
  expect_identical(ret, test)
  expect_equal(ret, compare3)
  expect_equal(retall, compare)
  
  
  leftValue <- median(testdata[testfilter$len:(50 - testfilter$len)])
  rightValue <- median(testdata[(53 + testfilter$len):(100 - testfilter$len)])
  cor <- testfilter$acf
  cor[1] <- 2
  ret <- testPeak(39:50 / testfilter$sr, 42:53 / testfilter$sr, 40:63 / testfilter$sr, 
                  testdata[40:63], testfilter, cor, leftValue, rightValue, 1e-6 / testfilter$sr)
  compare1 <- stepR::stepblock(leftEnd = c(0, ret$left, ret$right), rightEnd = c(ret$left, ret$right, 1),
                               value = c(leftValue, ret$value, rightValue), x0 = 0)
  class(compare1) <- c("localDeconvolution", class(compare1))
  ret <- testPeak(seq(ret$left - 1 / testfilter$sr, ret$left + 1 / testfilter$sr, 0.07 / testfilter$sr),
                  seq(ret$right - 1 / testfilter$sr, ret$right + 1 / testfilter$sr, 0.07 / testfilter$sr),
                  40:63 / testfilter$sr, testdata[40:63], testfilter, cor, leftValue, rightValue,
                  1e-6 / testfilter$sr)
  compare2 <- stepR::stepblock(leftEnd = c(0, ret$left, ret$right), rightEnd = c(ret$left, ret$right, 1),
                               value = c(leftValue, ret$value, rightValue), x0 = 0)
  class(compare2) <- c("localDeconvolution", class(compare2))
  compare <- list(compare1, compare2)
  attr(compare, "noDeconvolution") <- integer(0)
  attr(compare2, "noDeconvolution") <- integer(0)
  compareq <- 1e9
  attr(compareq, "n") <- 100L
  attr(compare, "q") <- compareq
  attr(compare2, "q") <- compareq
  
  ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                            method = "2Param", lengths = 1, q = rep(1e9, 1),
                            gridSize = c(1, 0.07) / testfilter$sr)
  retall <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                               method = "2Param", lengths = 1, q = rep(1e9, 1),
                               gridSize = c(1, 0.07) / testfilter$sr, output = "everyGrid")
  
  test <- retall[[2]]
  attr(test, "noDeconvolution") <- integer(0)
  attr(test, "q") <- attr(retall, "q")
  expect_identical(ret, test)
  expect_equal(ret, compare2)
  expect_equal(retall, compare)
})

test_that("argument windowFactorRefinement is tested and works", {
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 100)
  testdata <- lowpassFilter::randomGeneration(n = 100, filter = testfilter, noise = 1, seed = "no",
                         signal = lowpassFilter::getConvolutionPeak(1:100 / testfilter$sr, 0.5, 0.53, 10, 0, 0,
                                            testfilter))
  testfit <- stepR::stepblock(value = c(0, 1, 0), leftEnd = c(0, 0.5, 0.53),
                              rightEnd = c(0.5, 0.53, 1), x0 = 0)
  
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  method = "2Param", lengths = 1, q = rep(1e9, 1),
                                  windowFactorRefinement = "s"))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  method = "2Param", lengths = 1, q = rep(1e9, 1),
                                  windowFactorRefinement = Inf))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  method = "2Param", lengths = 1, q = rep(1e9, 1),
                                  windowFactorRefinement = NULL))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  method = "2Param", lengths = 1, q = rep(1e9, 1),
                                  windowFactorRefinement = c(1, "s")))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  method = "2Param", lengths = 1, q = rep(1e9, 1),
                                  windowFactorRefinement = c(1, as.numeric(NA))))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  method = "2Param", lengths = 1, q = rep(1e9, 1),
                                  windowFactorRefinement = c(1, 1, 1)))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  method = "2Param", lengths = 1, q = rep(1e9, 1),
                                  gridSize = c(1, 0.1) / testfilter$sr,
                                  windowFactorRefinement = c(1, 1)))
  
  leftValue <- median(testdata[testfilter$len:(50 - testfilter$len)])
  rightValue <- median(testdata[(53 + testfilter$len):(100 - testfilter$len)])
  cor <- testfilter$acf
  cor[1] <- 2
  ret <- testPeak(39:50 / testfilter$sr, 42:53 / testfilter$sr, 40:63 / testfilter$sr, 
                  testdata[40:63], testfilter, cor, leftValue, rightValue, 1e-6 / testfilter$sr)
  compare1 <- stepR::stepblock(leftEnd = c(0, ret$left, ret$right), rightEnd = c(ret$left, ret$right, 1),
                               value = c(leftValue, ret$value, rightValue), x0 = 0)
  class(compare1) <- c("localDeconvolution", class(compare1))
  ret <- testPeak(seq(ret$left - 0.1 / testfilter$sr, ret$left + 0.1 / testfilter$sr, 0.01 / testfilter$sr),
                  seq(ret$right - 0.1 / testfilter$sr, ret$right + 0.1 / testfilter$sr, 0.01 / testfilter$sr),
                  40:63 / testfilter$sr, testdata[40:63], testfilter, cor, leftValue, rightValue,
                  1e-6 / testfilter$sr)
  compare2 <- stepR::stepblock(leftEnd = c(0, ret$left, ret$right), rightEnd = c(ret$left, ret$right, 1),
                               value = c(leftValue, ret$value, rightValue), x0 = 0)
  class(compare2) <- c("localDeconvolution", class(compare2))
  ret <- testPeak(seq(ret$left - 0.01 / testfilter$sr, ret$left + 0.01 / testfilter$sr, 0.001 / testfilter$sr),
                  seq(ret$right - 0.01 / testfilter$sr, ret$right + 0.01 / testfilter$sr, 0.001 / testfilter$sr),
                  40:63 / testfilter$sr, testdata[40:63], testfilter, cor, leftValue, rightValue,
                  1e-6 / testfilter$sr)
  compare3 <- stepR::stepblock(leftEnd = c(0, ret$left, ret$right), rightEnd = c(ret$left, ret$right, 1),
                               value = c(leftValue, ret$value, rightValue), x0 = 0)
  class(compare3) <- c("localDeconvolution", class(compare3))
  compare <- list(compare1, compare2, compare3)
  attr(compare, "noDeconvolution") <- integer(0)
  attr(compare3, "noDeconvolution") <- integer(0)
  compareq <- 1e9
  attr(compareq, "n") <- 100L
  attr(compare, "q") <- compareq
  attr(compare3, "q") <- compareq
  
  ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                            method = "2Param", lengths = 1, q = rep(1e9, 1),
                            gridSize = c(1, 0.01, 0.001) / testfilter$sr,
                            windowFactorRefinement = c(0.1, 1))
  retall <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                               method = "2Param", lengths = 1, q = rep(1e9, 1),
                               gridSize = c(1, 0.01, 0.001) / testfilter$sr, windowFactorRefinement = c(0.1, 1),
                               output = "everyGrid")
  
  test <- retall[[3]]
  attr(test, "noDeconvolution") <- integer(0)
  attr(test, "q") <- attr(retall, "q")
  expect_identical(ret, test)
  expect_equal(ret, compare3)
  expect_equal(retall, compare)
})

test_that("argument output is tested", {
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 100)
  testdata <- lowpassFilter::randomGeneration(n = 100, filter = testfilter, noise = 1, seed = "no",
                signal = lowpassFilter::getConvolutionPeak(1:100 / testfilter$sr, 0.5, 0.53, 10, 0, 0,
                                       testfilter))
  testfit <- stepR::stepblock(value = c(0, 1, 0), leftEnd = c(0, 0.5, 0.53),
                              rightEnd = c(0.5, 0.53, 1), x0 = 0)
  
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  method = "2Param", lengths = 1, q = rep(1e9, 1),
                                  output = 1))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  method = "2Param", lengths = 1, q = rep(1e9, 1),
                                  output = c("only", "every")))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  method = "2Param", lengths = 1, q = rep(1e9, 1),
                                  output = "aha"))
})

test_that("argument report is tested and works", {
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 100)
  testdata <- lowpassFilter::randomGeneration(n = 100, filter = testfilter, noise = 1, seed = "no",
                     signal = lowpassFilter::getConvolutionPeak(1:100 / testfilter$sr, 0.5, 0.53, 10, 0, 0,
                              testfilter))
  testfit <- stepR::stepblock(value = c(0, 1, 0), leftEnd = c(0, 0.5, 0.53),
                              rightEnd = c(0.5, 0.53, 1), x0 = 0)
  
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  method = "2Param", lengths = 1, q = rep(1e9, 1),
                                  report = "s"))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  method = "2Param", lengths = 1, q = rep(1e9, 1),
                                  report = 1))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  method = "2Param", lengths = 1, q = rep(1e9, 1),
                                  report = NULL))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  method = "2Param", lengths = 1, q = rep(1e9, 1),
                                  report = NA))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  method = "2Param", lengths = 1, q = rep(1e9, 1),
                                  report = c(TRUE, TRUE)))
  
  expect_identical(suppressMessages(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                                       method = "2Param", lengths = 1, q = rep(1e9, 1),
                                                       report = TRUE)),
                   improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                      method = "2Param", lengths = 1, q = rep(1e9, 1)))
})

test_that("noDeconvolution warning can be suppressed and that suppressWarningNoDeconvolution is tested", {
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 10)
  testdata <- lowpassFilter::randomGeneration(n = 10, filter = testfilter, signal = rep(0, 10), noise = 1, seed = "no")
  testfit <- stepR::stepblock(value = mean(testdata), leftEnd = 0, rightEnd = 1, x0 = 0)
  
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  method = "2Param", lengths = 1, q = rep(1e9, 1),
                                  suppressWarningNoDeconvolution = c(TRUE, TRUE)))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  method = "2Param", lengths = 1, q = rep(1e9, 1),
                                  suppressWarningNoDeconvolution = 1))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  method = "2Param", lengths = 1, q = rep(1e9, 1),
                                  suppressWarningNoDeconvolution = NA))
  
  expect_warning(ret <- improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                           method = "2Param", lengths = 1, q = rep(1e9, 1)))
  expect_identical(ret, improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                           method = "2Param", lengths = 1, q = rep(1e9, 1),
                                           suppressWarningNoDeconvolution = TRUE))
})

test_that("argument localList is tested and works", {
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 100)
  testdata <- lowpassFilter::randomGeneration(n = 100, filter = testfilter, noise = 1, seed = "no",
                    signal = lowpassFilter::getConvolutionPeak(1:100 / testfilter$sr, 0.5, 0.53, 10, 0, 0,
                                            testfilter))
  testfit <- stepR::stepblock(value = c(0, 1, 0), leftEnd = c(0, 0.5, 0.53),
                              rightEnd = c(0.5, 0.53, 1), x0 = 0)
  testlengths <- c(3, 5)
  testlocalList <- createLocalList(filter = testfilter, method = "LR", lengths = testlengths)
  
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  method = "LR", lengths = testlengths, q = rep(1e9, 2),
                                  localList = 1))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  method = "LR", lengths = testlengths, q = rep(1e9, 2),
                                  localList = list()))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  method = "LR", lengths = testlengths, q = rep(1e9, 2),
                                  localList = unclass(testlocalList)))
  
  expect_identical(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                      method = "LR", lengths = testlengths, q = rep(1e9, 2),
                                      localList = testlocalList),
                   improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                      method = "LR", lengths = testlengths, q = rep(1e9, 2)))
  
  testlengths <- c(2, 11)
  testlocalList <- createLocalList(filter = testfilter, method = "2Param", lengths = testlengths)
  expect_identical(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                      method = "2Param", lengths = testlengths, q = rep(1e9, 2),
                                      localList = testlocalList),
                   improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                      method = "2Param", lengths = testlengths, q = rep(1e9, 2)))
  
  testfilter2 <- lowpassFilter::lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 50)
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter2,
                                  method = "2Param", lengths = testlengths, q = rep(1e9, 2),
                                  localList = testlocalList, suppressWarningNoDeconvolution = TRUE))
  
  testfilter2 <- lowpassFilter::lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 100)
  expect_identical(improveSmallScales(data = testdata, fit = testfit, filter = testfilter2,
                                      method = "2Param", lengths = testlengths, q = rep(1e9, 2),
                                      localList = testlocalList),
                   improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                      method = "2Param", lengths = testlengths, q = rep(1e9, 2),
                                      localList = testlocalList))
  
  testlengths <- c(8, 15)
  testlocalList <- createLocalList(filter = testfilter, method = "LR", lengths = testlengths)
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  method = "2Param", lengths = testlengths, q = rep(1e9, 2),
                                  localList = testlocalList, suppressWarningNoDeconvolution = TRUE))
  
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  method = "LR", lengths = c(2, 11), q = rep(1e9, 2),
                                  localList = testlocalList, suppressWarningNoDeconvolution = TRUE))
  
  # it is compatibel with other arguments
  testfit <- stepR::stepblock(value = c(0, 1, 0), leftEnd = c(0, 0.5, 0.53) + 0.3,
                              rightEnd = c(0.5, 0.53, 1) + 0.3, x0 = 0.3)
  expect_identical(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                      method = "LR", lengths = testlengths, q = rep(1e9, 2),
                                      startTime = 0.3, thresholdLongSegment = 13L, localValue = mean,
                                      regularization = list(c(2), c(1, 0.5)),
                                      gridSize = c(1 / testfilter$sr, 1 / 10 / testfilter$sr),
                                      windowFactorRefinement = 0.5, output = "every",
                                      localList = testlocalList),
                   improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                      method = "LR", lengths = testlengths, q = rep(1e9, 2),
                                      startTime = 0.3, thresholdLongSegment = 13L, localValue = mean,
                                      regularization = list(c(2), c(1, 0.5)),
                                      gridSize = c(1 / testfilter$sr, 1 / 10 / testfilter$sr),
                                      windowFactorRefinement = 0.5, output = "every"))
  
  testfit <- stepR::stepblock(value = c(0), leftEnd = c(0.3),
                              rightEnd = c(1) + 0.3, x0 = 0.3)
  expect_identical(suppressMessages(
    improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                      method = "LR", lengths = testlengths, q = NULL,
                                      alpha = 0.123, r = 2L, report = TRUE, nq = 120L, 
                                      options = list(load = list(), simulation = "vectorIncreased"),
                                      messages = 1, penalty = "sqrt",
                                      startTime = 0.3, thresholdLongSegment = 13L, localValue = mean,
                                      regularization = list(c(2), c(1, 0.5)),
                                      gridSize = c(1 / testfilter$sr, 1 / 10 / testfilter$sr),
                                      windowFactorRefinement = 0.5, output = "every",
                                      localList = testlocalList)),
    suppressMessages(
                   improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                      method = "LR", lengths = testlengths, q = NULL,
                                      alpha = 0.123, r = 2L, report = TRUE, nq = 120L, 
                                      options = list(load = list(), simulation = "vectorIncreased"),
                                      messages = 1, penalty = "sqrt",
                                      startTime = 0.3, thresholdLongSegment = 13L, localValue = mean,
                                      regularization = list(c(2), c(1, 0.5)),
                                      gridSize = c(1 / testfilter$sr, 1 / 10 / testfilter$sr),
                                      windowFactorRefinement = 0.5, output = "every")))
})

test_that("... is checked", {
  testfilter <- lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 100)
  testdata <- lowpassFilter::randomGeneration(n = 100, filter = testfilter, noise = 1, seed = "no",
                                              signal = 0)
  testfit <- stepR::stepblock(value = c(0, 1, 0), leftEnd = c(0, 0.5, 0.53),
                              rightEnd = c(0.5, 0.53, 1), x0 = 0)
  
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  method = "LR", lengths = 5, q = 20,
                                  n = 100))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  method = "LR", lengths = 5, q = 20,
                                  family = 100))
  expect_error(improveSmallScales(data = testdata, fit = testfit, filter = testfilter,
                                  method = "LR", lengths = 5, q = 20,
                                  correlations = 1))
})
