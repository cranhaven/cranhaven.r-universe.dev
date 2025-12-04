
# test_that code for Bnormal

# library(testthat)
require(wiqid)

context("Bayesian normal models")

test_that("Bnormal gives same answers",  {
  # Generate data
  set.seed(123)
  x <- rnorm(10, 1, 0.15)
  # With default (flat) priors:
  Bout <- Bnormal(x)
  expect_that(class(Bout), equals(c("mcmcOutput", 'matrix', 'array')))
  expect_that(dim(Bout), equals(c(30000, 2)))
  expect_that(names(Bout), equals(c("mu", "sigma")))
  expect_that(attr(Bout, "header"),
    equals("Model fitted in R with a Gibbs sampler"))
  expect_that(attr(Bout, "nChains"), equals(3))
  expect_equal(as.character(attr(Bout, "call")), c("Bnormal", "x"))

  expect_equivalent(round(colMeans(Bout), 4), c(1.0112, 0.1563))
  expect_equivalent(round(c(hdi(Bout)), 4), c(0.9089, 1.1118, 0.0901, 0.2406))
  xx <- x * 1000
  expect_warning(Bout <- Bnormal(xx, priors=list(muMean=0, muSD=10)), # silly prior
      "Sample mean is outside the prior range")
  set.seed(123)
  Bout <- Bnormal(x, priors=list(muMean=0, muSD=10)) # informative prior for mu
  expect_equivalent(round(colMeans(Bout), 4), c(1.0112, 0.1563))
  expect_equivalent(round(c(hdi(Bout)), 4), c(0.9088, 1.1117, 0.0901, 0.2406))
  set.seed(234)
  Bout <- Bnormal(xx, priors=list(muMean=1000))  # sensible priors
  expect_equivalent(round(colMeans(Bout), 1), c(1011.3, 156.5))
  expect_equivalent(round(c(hdi(Bout)), 1), c(908.6, 1112.8,   91.4,  242.8))
})

if(requireNamespace("rjags", quietly = TRUE)) {
  test_that("Bnormal2 gives same answers",  {
  # Generate data
  set.seed(123)
  x <- rnorm(10, 1, 0.15)
  dummy <- capture.output(Bout <- Bnormal2(x, seed=345))  # default prior
  expect_that(class(Bout), equals(c("mcmcOutput", 'matrix', 'array')))
  expect_that(dim(Bout), equals(c(30000, 2)))
  expect_that(names(Bout), equals(c("mu", "sigma")))
  expect_that(attr(Bout, "header"),
    equals("Model fitted in JAGS with 'rjags' functions"))
  expect_that(attr(Bout, "nChains"), equals(3))
  expect_equal(as.character(attr(Bout, "call")), c("Bnormal2", "x", "345"))
  expect_equivalent(round(colMeans(Bout), 4), c(1.0111, 0.1683))
  expect_equivalent(round(c(hdi(Bout)), 4), c(0.9023, 1.1228, 0.0941, 0.2667))
  xx <- x * 1000
  dummy <- capture.output(expect_warning(Bout <- Bnormal2(xx,
      priors=list(muMean=0, muSD=10)), # silly prior
      "Sample mean is outside the prior range"))
  dummy <- capture.output(Bout <- Bnormal2(x, priors=list(muMean=0, muSD=10), seed=345))  # informative prior for mu, default for sigma
  expect_equivalent(round(colMeans(Bout), 4), c(1.0110, 0.1684))
  expect_equivalent(round(c(hdi(Bout)), 4), c(0.8990, 1.1185, 0.0936, 0.2663))

  dummy <- capture.output(Bout <- Bnormal2(x, priors=list(muMean=0, muSD=10, sigmaMode=0.1, sigmaSD=0.2), seed=345))  # informative prior for mu and sigma
  expect_equivalent(round(colMeans(Bout), 4), c(1.0106, 0.1623))
  expect_equivalent(round(c(hdi(Bout)), 4), c(0.9013, 1.1138, 0.0929, 0.2486))
  })
}
