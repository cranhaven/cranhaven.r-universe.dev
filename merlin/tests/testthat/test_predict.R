testthat::context("predict")

#------------------------------------------------------------------------------
# method: test predictions


#------------------------------------------------------------------------------

# eta, fixedonly, gaussian
testthat::test_that("Test 1.1", {
  data("pbc.merlin", package = "merlin")
  comp <- rep(0, 6)
  mod <- merlin(
    model = list(logp ~ rcs(year, df = 4) + M1[id] * 1),
    levels = c("id"),
    family = "gaussian",
    data = pbc.merlin,
    timevar = "year"
  )
  pred <- predict(mod, stat = "eta", type="fixedonly")[1:6]
  testpred <- as.numeric(c(2.369497,2.372921,2.369497,2.372765,2.375152,2.379285))
  testthat::expect_equal(object = (pred - testpred), expected = comp, tolerance = 5e-2)
})

# cif, fixedonly, weibull
testthat::test_that("Test 1.2", {
  data("pbc.merlin", package = "merlin")
  comp <- rep(0, 6)
  mod <- merlin(model = Surv(stime, died) ~ trt,
                family = "weibull",
                data = pbc.merlin
  )
  pred <- predict(mod,stat = "cif", type="fixedonly")[1:6]
  testpred <- as.numeric(c(0.06384572,0.64606036,0.16414534,0.30120628,0.24045001,0.37855772))
  testthat::expect_equal(object = (pred - testpred), expected = comp, tolerance = 5e-2)
})

# hazard, fixedonly, weibull
testthat::test_that("Test 1.3", {
  data("pbc.merlin", package = "merlin")
  comp <- rep(0, 6)
  mod <- merlin(model = Surv(stime, died) ~ trt,
                family = "weibull",
                data = pbc.merlin
  )
  pred <- predict(mod,stat = "hazard", type="fixedonly")[1:6]
  testpred <- as.numeric(c(0.06488065,0.07902991,0.06969357,0.07323535,0.07188361,0.07475865))
  testthat::expect_equal(object = (pred - testpred), expected = comp, tolerance = 5e-2)
})

# mu, fixedonly, weibull
testthat::test_that("Test 1.4", {
  data("pbc.merlin", package = "merlin")
  comp <- rep(0, 6)
  mod <- merlin(model = Surv(stime, died) ~ trt,
                family = "weibull",
                data = pbc.merlin
  )
  pred <- predict(mod,stat = "mu", type="fixedonly")[1:6]
  testpred <- as.numeric(c(-2.816479,-2.816479,-2.816479,-2.816479,-2.816134,-2.816134))
  testthat::expect_equal(object = (pred - testpred), expected = comp, tolerance = 5e-2)
})

# chazard, fixedonly, weibull
testthat::test_that("Test 1.5", {
  data("pbc.merlin", package = "merlin")
  comp <- rep(0, 6)
  mod <- merlin(model = Surv(stime, died) ~ trt,
                family = "weibull",
                data = pbc.merlin
  )
  pred <- predict(mod,stat = "chazard", type="fixedonly")[1:6]
  testpred <- as.numeric(c(0.06596962,1.03840495,0.17928418,0.35836057,0.27500153,0.47565386))
  testthat::expect_equal(object = (pred - testpred), expected = comp, tolerance = 5e-2)
})

# survival, fixedonly, weibull, at option
testthat::test_that("Test 1.6", {
  data("pbc.merlin", package = "merlin")
  comp <- rep(0, 6)
  mod <- merlin(model = Surv(stime, died) ~ trt,
                family = "weibull",
                data = pbc.merlin
  )
  pred <- predict(mod, stat = "survival", type ="fixedonly", at = c(age = 50))[1:6]
  testpred <- as.numeric(c(0.9361593,0.3540189,0.8358683,0.6988211,0.7595710,0.6214786))
  testthat::expect_equal(object = (pred - testpred), expected = comp, tolerance = 5e-2)
})

# cifdifference, fixedonly, weibull
testthat::test_that("Test 1.7", {
  data("pbc.merlin", package = "merlin")
  comp <- rep(0, 6)
  mod <- merlin(model = Surv(stime, died) ~ trt,
                family = "weibull",
                data = pbc.merlin
  )
  pred <- predict(mod, stat = "cifdifference", type="fixedonly", contrast = c("trt" = 0, "trt" = 1))[1:6]
  testpred <- as.numeric(c(2.131299e-05,1.268614e-04,5.171616e-05,8.642254e-05,7.206727e-05,1.019945e-04))
  testthat::expect_equal(object = (pred - testpred), expected = comp, tolerance = 5e-2)
})

