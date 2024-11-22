testthat::context("mlsurv")
data("pbc.merlin", package = "merlin")

testthat::test_that("Exponential model", {
  fit.merlin <- merlin(model = list(Surv(stime, died) ~ trt), family = "exp", data = pbc.merlin)
  fit.mlsurv <- mlsurv(formula = survival::Surv(stime, died) ~ trt, distribution = "exp", data = pbc.merlin)
  testthat::expect_equal(object = coef(fit.mlsurv), expected = coef(fit.merlin), tolerance = 1e-3)
})

testthat::test_that("Weibull model", {
  fit.merlin <- merlin(model = list(Surv(stime, died) ~ trt), family = "wei", data = pbc.merlin)
  fit.mlsurv <- mlsurv(formula = survival::Surv(stime, died) ~ trt, distribution = "wei", data = pbc.merlin)
  testthat::expect_equal(object = coef(fit.mlsurv), expected = coef(fit.merlin), tolerance = 1e-3)
})

testthat::test_that("Gompertz model", {
  fit.merlin <- merlin(model = list(Surv(stime, died) ~ trt), family = "gom", data = pbc.merlin)
  fit.mlsurv <- mlsurv(formula = survival::Surv(stime, died) ~ trt, distribution = "gom", data = pbc.merlin)
  testthat::expect_equal(object = coef(fit.mlsurv), expected = coef(fit.merlin), tolerance = 1e-3)
})

testthat::test_that("RP(2) model", {
  fit.merlin <- merlin(model = list(Surv(stime, died) ~ trt + rcs(stime, df = 2, log = T, event = T)), family = "rp", timevar = "stime", data = pbc.merlin)
  fit.mlsurv <- mlsurv(formula = survival::Surv(stime, died) ~ trt, distribution = "rp", df = 2, data = pbc.merlin)
  testthat::expect_equal(object = coef(fit.mlsurv), expected = coef(fit.merlin), tolerance = 1e-3)
})

testthat::test_that("RP(3) model", {
  fit.merlin <- merlin(model = list(Surv(stime, died) ~ trt + rcs(stime, df = 3, log = T, event = T)), family = "rp", timevar = "stime", data = pbc.merlin)
  fit.mlsurv <- mlsurv(formula = survival::Surv(stime, died) ~ trt, distribution = "rp", df = 3, data = pbc.merlin)
  testthat::expect_equal(object = coef(fit.mlsurv), expected = coef(fit.merlin), tolerance = 1e-3)
})

testthat::test_that("loghazard (3 df) with splines model", {
  testthat::skip_on_cran()
  fit.merlin <- merlin(model = list(Surv(stime, died) ~ trt + rcs(stime, df = 3, log = T, event = T)), family = "loghazard", timevar = "stime", data = pbc.merlin)
  fit.mlsurv <- mlsurv(formula = survival::Surv(stime, died) ~ trt, distribution = "loghazard", df = 3, data = pbc.merlin)
  testthat::expect_equal(object = coef(fit.mlsurv), expected = coef(fit.merlin), tolerance = 1e-3)
})

testthat::test_that("loghazard with fractional polynomials (0, 1) model", {
  testthat::skip_on_cran()
  fit.merlin <- merlin(model = list(Surv(stime, died) ~ trt + fp(stime, powers = c(0, 1))), family = "loghazard", timevar = "stime", data = pbc.merlin)
  fit.mlsurv <- mlsurv(formula = survival::Surv(stime, died) ~ trt, distribution = "loghazard", powers = c(0, 1), rcs = FALSE, data = pbc.merlin)
  testthat::expect_equal(object = coef(fit.mlsurv), expected = coef(fit.merlin), tolerance = 1e-3)
})

testthat::test_that("logchazard model with fp", {
  testthat::skip_on_cran()
  fit.merlin <- merlin(model = list(Surv(stime, died) ~ trt + fp(stime, powers = c(0, 1))), family = "rp", timevar = "stime", data = pbc.merlin, from = rep(0.1, 4))
  fit.mlsurv <- mlsurv(formula = survival::Surv(stime, died) ~ trt, distribution = "logchazard", powers = c(0, 1), rcs = FALSE, data = pbc.merlin, from.null = rep(0.1, 3))
  testthat::expect_equal(object = coef(fit.mlsurv), expected = coef(fit.merlin), tolerance = 1e-3)
})

testthat::test_that("expect error if random effects", {
  testthat::expect_error(object = mlsurv(formula = Surv(stime, died) ~ trt + M1[id], distribution = "exp", data = pbc.merlin))
})

testthat::test_that("expect error if malformed call (e.g. dfs, distributions, etc.)", {
  testthat::expect_error(object = mlsurv(formula = Surv(stime, died) ~ trt, distribution = "gaussian", data = pbc.merlin))
  testthat::expect_error(object = mlsurv(formula = Surv(stime, died) ~ trt, distribution = "rp", data = pbc.merlin))
  testthat::expect_error(object = mlsurv(formula = Surv(stime, died) ~ trt, distribution = "loghazard", data = pbc.merlin))
  testthat::expect_error(object = mlsurv(formula = Surv(stime, died) ~ trt, distribution = "logchazard", data = pbc.merlin))
  testthat::expect_error(object = mlsurv(formula = Surv(stime, died) ~ trt, distribution = "rp", df = c(1, 2), data = pbc.merlin))
  testthat::expect_error(object = mlsurv(formula = Surv(stime, died) ~ trt, distribution = "loghazard", df = c(1, 2), data = pbc.merlin))
  testthat::expect_error(object = mlsurv(formula = Surv(stime, died) ~ trt, distribution = "logchazard", df = c(1, 2), data = pbc.merlin))
})
