#########################
# Author : Gireg Willame
# June 2022.
#
# Series of tests to check the BT_devTweedie function.
#
########################

testthat::test_that("BT_devTweedie - correct inputs", {
  sampleSize <- 1000

  # Expected errors on y value.
  mu <-
    runif(sampleSize, 0, 2)
  w <- rep(1, sampleSize)
  tweedie.power <- 1
  y <-
    "Text"
  expect_error(expect_warning(BT_devTweedie(y, mu, tweedie.power, w)))
  y <-
    c("A", "B")
  expect_error(expect_warning(BT_devTweedie(y, mu, tweedie.power, w)))
  y <- NA
  expect_error(BT_devTweedie(y, mu, tweedie.power, w))
  y <- NULL
  expect_error(BT_devTweedie(y, mu, tweedie.power, w))
  y <-
    sample(c(0, 1, 2), size = sampleSize / 2, replace = T)
  expect_error(BT_devTweedie(y, mu, tweedie.power, w))
  w <-
    c(sample(c(0, 1, 2), size = sampleSize - 10, replace = T), rep("A", 10))
  expect_error(expect_warning(BT_devTweedie(y, mu, tweedie.power, w)))

  # Expected errors on mu value.
  y <-
    sample(c(0, 1, 2), size = sampleSize, replace = T)
  w <- rep(1, sampleSize)
  tweedie.power <- 1
  mu <-
    "Text"
  expect_error(expect_warning(BT_devTweedie(y, mu, tweedie.power, w)))
  mu <-
    c("A", "B")
  expect_error(expect_warning(BT_devTweedie(y, mu, tweedie.power, w)))
  mu <- NA
  expect_error(BT_devTweedie(y, mu, tweedie.power, w))
  mu <- NULL
  expect_error(BT_devTweedie(y, mu, tweedie.power, w))
  mu <-
    runif(sampleSize / 3, 0, 2)
  expect_error(BT_devTweedie(y, mu, tweedie.power, w))
  mu <-
    c(runif(sampleSize - 10, 0, 2), rep("A", 10))
  expect_error(expect_warning(BT_devTweedie(y, mu, tweedie.power, w)))

  # Expected errors on Tweedie power param.
  y <-
    sample(c(0, 1, 2), size = sampleSize, replace = T)
  mu <- runif(sampleSize, 0, 2)
  w <- rep(1, sampleSize)
  tweedie.power <-
    0.5
  expect_error(BT_devTweedie(y, mu, tweedie.power, w))
  tweedie.power <-
    c(1, 2, 3)
  expect_error(BT_devTweedie(y, mu, tweedie.power, w))
  tweedie.power <-
    NULL
  expect_error(BT_devTweedie(y, mu, tweedie.power, w))
  tweedie.power <-
    NA
  expect_error(BT_devTweedie(y, mu, tweedie.power, w))
  tweedie.power <-
    "Text"
  expect_error(BT_devTweedie(y, mu, tweedie.power, w))

  # Expected errors on weights param.
  y <-
    sample(c(0, 1, 2), size = sampleSize, replace = T)
  runif(sampleSize / 3, 0, 2)
  tweedie.power <- 1
  w <-
    "Text"
  expect_error(expect_warning(BT_devTweedie(y, mu, tweedie.power, w)))
  w <-
    c("A", "B")
  expect_error(expect_warning(BT_devTweedie(y, mu, tweedie.power, w)))
  w <- NA
  expect_error(BT_devTweedie(y, mu, tweedie.power, w))
  w <-
    rep(1, sampleSize / 2)
  expect_error(BT_devTweedie(y, mu, tweedie.power, w))
  w <-
    c(rep(1, sampleSize - 10), rep("A", 10))
  expect_error(expect_warning(BT_devTweedie(y, mu, tweedie.power, w)))
  w <-
    c(rep(1, sampleSize - 10), rep(-1, 10))
  expect_error(BT_devTweedie(y, mu, tweedie.power, w))
  w <-
    c(rep(2.54, sampleSize - 10), rep(-0.2, 10))
  expect_error(BT_devTweedie(y, mu, tweedie.power, w))

})

testthat::test_that("BT_devTweedie - correct results", {
  sampleSize <- 1000

  ####
  # Poisson case + check different values.
  ####
  y <-
    sample(c(0, 1, 2), size = sampleSize, replace = T)
  mu <-
    runif(sampleSize, 0, 2)
  w <- rep(1, sampleSize)
  tweedie.power <- 1
  expect_equal(BT_devTweedie(y, mu, tweedie.power, w),
               BT_devTweedie(y, mu, tweedie.power))

  poissonDev <- function(y, mu, w) {
    where0 <- which(y == 0)
    res <- 2 * w * (mu - y + log((y / mu) ^ (y)))
    res[where0] <- 2 * w[where0] * mu[where0]
    return(res)
  }
  gammaDev <- function(y, mu, w) {
    where0 <- which(y == 0)
    res <- 2 * w * (-log(y / mu) + (y / mu) - 1)
    res[where0] <- 2 * w[where0] * ((y[where0] / mu[where0]) - 1)
    return(res)
  }
  globalDev <- function(y, mu, w, p) {
    2 * w * ((max(y, 0) ^ (2 - p) / ((1 - p) * (2 - p))) - (y * mu ^ (1 - p) /
                                                              (1 - p)) + (mu ^ (2 - p) / (2 - p)))
  }

  w <- runif(sampleSize)
  expect_equal(BT_devTweedie(y, mu, tweedie.power, w), poissonDev(y, mu, w))
  y <- runif(sampleSize, 0, 10)
  mu <- runif(sampleSize, 0, 10)
  expect_equal(BT_devTweedie(y, mu, tweedie.power, w), poissonDev(y, mu, w))

  ####
  # Normal case.
  ####
  y <-
    sample(c(0, 1, 2), size = sampleSize, replace = T)
  mu <-
    runif(sampleSize, 0, 2)
  w <- rep(1, sampleSize)
  tweedie.power <- 0
  expect_equal(BT_devTweedie(y, mu, tweedie.power, w),
               BT_devTweedie(y, mu, tweedie.power))

  w <- runif(sampleSize)
  expect_equal(BT_devTweedie(y, mu, tweedie.power, w), w * (y - mu) ^ 2)
  y <- runif(sampleSize, 0, 10)
  mu <- runif(sampleSize, 0, 10)
  expect_equal(BT_devTweedie(y, mu, tweedie.power, w), w * (y - mu) ^ 2)

  ####
  # Gamma case.
  ####
  y <-
    sample(c(0, 1, 2), size = sampleSize, replace = T)
  mu <-
    runif(sampleSize, 0, 2)
  w <- rep(1, sampleSize)
  tweedie.power <- 2
  expect_equal(BT_devTweedie(y, mu, tweedie.power, w),
               BT_devTweedie(y, mu, tweedie.power))

  w <- runif(sampleSize)
  expect_equal(BT_devTweedie(y, mu, tweedie.power, w), gammaDev(y, mu, w))
  y <- runif(sampleSize, 0, 10)
  mu <- runif(sampleSize, 0, 10)
  expect_equal(BT_devTweedie(y, mu, tweedie.power, w), gammaDev(y, mu, w))

  ####
  # General case.
  ####
  y <-
    sample(c(0, 1, 2), size = sampleSize, replace = T)
  mu <-
    runif(sampleSize, 0, 2)
  w <- rep(1, sampleSize)
  tweedie.power <- 3.54
  expect_equal(BT_devTweedie(y, mu, tweedie.power, w),
               BT_devTweedie(y, mu, tweedie.power))

  w <- runif(sampleSize)
  expect_equal(BT_devTweedie(y, mu, tweedie.power, w),
               globalDev(y, mu, w, tweedie.power))
  y <- runif(sampleSize, 0, 100)
  mu <- runif(sampleSize, 0, 100)
  expect_equal(BT_devTweedie(y, mu, tweedie.power, w),
               globalDev(y, mu, w, tweedie.power))

  # Other tweedie power.
  y <-
    sample(c(0, 1, 2), size = sampleSize, replace = T)
  mu <-
    runif(sampleSize, 0, 2)
  w <- rep(1, sampleSize)
  tweedie.power <- 8
  expect_equal(BT_devTweedie(y, mu, tweedie.power, w),
               BT_devTweedie(y, mu, tweedie.power))

  w <- runif(sampleSize)
  expect_equal(BT_devTweedie(y, mu, tweedie.power, w),
               globalDev(y, mu, w, tweedie.power))
  y <- runif(sampleSize, 0, 100)
  mu <- runif(sampleSize, 0, 100)
  expect_equal(BT_devTweedie(y, mu, tweedie.power, w),
               globalDev(y, mu, w, tweedie.power))

  # Other tweedie power.
  y <-
    sample(c(0, 1, 2), size = sampleSize, replace = T)
  mu <-
    runif(sampleSize, 0, 2)
  w <- rep(1, sampleSize)
  tweedie.power <- 11.44795
  expect_equal(BT_devTweedie(y, mu, tweedie.power, w),
               BT_devTweedie(y, mu, tweedie.power))

  w <- runif(sampleSize)
  expect_equal(BT_devTweedie(y, mu, tweedie.power, w),
               globalDev(y, mu, w, tweedie.power))
  y <- runif(sampleSize, 0, 100)
  mu <- runif(sampleSize, 0, 100)
  expect_equal(BT_devTweedie(y, mu, tweedie.power, w),
               globalDev(y, mu, w, tweedie.power))

})
