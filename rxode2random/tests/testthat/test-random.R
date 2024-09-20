test_that("rnorm", {
  rxWithSeed(1024, {
    x <- rxnorm(n = 1e5)
    expect_equal(mean(x), 0, tolerance = 0.01)
    expect_equal(sd(x), 1, tolerance = 0.01)
  })
})

test_that("random variables work in R alone", {
  rxWithSeed(1024, {
    expect_true(is.numeric(rxcauchy()))

    p <- rxpois(2, n = 30000)
    expect_equal(mean(p), 2, tolerance = 0.01)
    expect_equal(sd(p), sqrt(2), tolerance = 0.01)

    r <- rxt(15, n = 30000)
    expect_equal(mean(r), 0, tolerance = 0.1)
    expect_equal(sd(r), sqrt(15 / (15 - 2)), tolerance = 0.1)

    r <- rxbinom(4, 0.5, n = 30000)
    expect_equal(max(r), 4)
    expect_equal(min(r), 0)
    expect_equal(mean(r), 4 * 0.5, tolerance = 1e-2)
    expect_equal(sd(r), sqrt(4 * 0.5 * 0.5), tolerance = 1e-2)

    chi <- rxchisq(15, n = 30000)
    expect_equal(mean(chi), 15, tolerance = 0.1)
    expect_equal(sd(chi), sqrt(2 * 15), tolerance = 0.1)

    xp <- rxexp(0.5, n = 30000)
    expect_equal(mean(xp), 2, tolerance = 0.1)
    expect_equal(sd(xp), sqrt(1 / (0.5 * 0.5)), tolerance = 0.1)

    f <- rxf(30, 40, n = 30000)

    sf <- function(d1, d2) {
      sqrt((2 * d2^2 * (d1 + d2 - 2)) / (d1 * (d2 - 2)^2 * (d2 - 4)))
    }

    mf <- function(d2) {
      return(d2 / (d2 - 2))
    }

    expect_equal(mean(f), mf(40), tolerance = 0.01)
    expect_equal(sd(f), sf(30, 40), tolerance = 0.1)

    x2 <- rxgamma(7.5, n = 30000)

    sgamma <- function(k, theta = 1) {
      sqrt(k / (theta^2))
    }

    ## expect_equal(sd(x2), sgamma(7.5), tolerance = 0.01)

    x2 <- rxbeta(2, 2, n = 30000)

    mbeta <- function(a, b) {
      return(a / (a + b))
    }

    sbeta <- function(a, b) {
      sqrt(a * b / ((a + b)^2 * (a + b + 1)))
    }

    expect_equal(mean(x2), mbeta(2, 2), tolerance = 0.01)
    expect_equal(sd(x2), sbeta(2, 2), tolerance = 0.01)

    x2 <- rxgeom(0.1, n = 30000)

    expect_equal(median(x2), -ceiling(1 / log2(1 - 0.1)))

    x2 <- rxpois(2, n = 30000)

    expect_equal(mean(x2), 2, tolerance = 0.01)
    expect_equal(sd(x2), sqrt(2), tolerance = 0.01)

    x2 <- rxunif(0.5, n = 30000)

    expect_equal(mean(x2), 0.5 * (0.5 + 1), tolerance = 1e-2)
    expect_equal(sd(x2), sqrt((1 - 0.5)^2 / 12), tolerance = 1e-2)

    x2 <- rxweibull(7.5, n = 30000)

    mweibull <- function(shape, scale = 1) {
      lambda <- scale
      k <- shape
      lambda * gamma(1 + 1 / k)
    }

    sweibull <- function(shape, scale = 1) {
      lambda <- scale
      k <- shape
      sqrt(lambda^2 * (gamma(1 + 2 / k)
        - (gamma(1 + 1 / k))^2))
    }

    expect_equal(mean(x2), mweibull(7.5), tolerance = 0.01)
    expect_equal(sd(x2), sweibull(7.5), tolerance = 0.01)
  })

})


