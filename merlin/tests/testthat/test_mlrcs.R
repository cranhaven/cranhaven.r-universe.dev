testthat::context("mlrcs")
data("pbc.merlin", package = "merlin")

testthat::test_that("Fixed-effects only model", {
  fit.merlin <- merlin(model = list(logp ~ rcs(year, df = 4) + age + trt), family = "gaussian", data = pbc.merlin, timevar = "year")
  fit.mlsurv <- mlrcs(formula = logp ~ 1 + rcs(year, df = 4) + age + trt, data = pbc.merlin)
  testthat::expect_equal(object = coef(fit.mlsurv), expected = coef(fit.merlin), tolerance = 1e-3)
})

# testthat::test_that("One random-effect in two-level model (random-effects in list)", {
#   fit.merlin <- merlin(model = list(logp ~ rcs(year, df = 4) + age + trt + M1[id] * 1), family = "gaussian", data = pbc.merlin, timevar = "year", levels = "id")
#   fit.mlsurv <- mlrcs(formula = logp ~ 1 + rcs(year, df = 4) + age + trt, random = list(~ 1 | id), data = pbc.merlin)
#   testthat::expect_equal(object = coef(fit.mlsurv), expected = coef(fit.merlin), tolerance = 1e-3)
# })

# testthat::test_that("One random-effect in two-level model (random-effects not in list)", {
#   fit.merlin <- merlin(model = list(logp ~ rcs(year, df = 4) + age + trt + M1[id] * 1), family = "gaussian", data = pbc.merlin, timevar = "year", levels = "id")
#   fit.mlsurv <- mlrcs(formula = logp ~ 1 + rcs(year, df = 4) + age + trt, random = ~ 1 | id, data = pbc.merlin)
#   testthat::expect_equal(object = coef(fit.mlsurv), expected = coef(fit.merlin), tolerance = 1e-3)
# })

# testthat::test_that("Two random-effects in two-level model", {
#   fit.merlin <- merlin(model = list(logp ~ rcs(year, df = 4) + age + trt + M1[id] * 1 + trt:M2[id] * 1), family = "gaussian", data = pbc.merlin, timevar = "year", levels = "id")
#   fit.mlsurv <- mlrcs(formula = logp ~ 1 + rcs(year, df = 4) + age + trt, random = ~ 1 + trt | id, data = pbc.merlin)
#   testthat::expect_equal(object = coef(fit.mlsurv), expected = coef(fit.merlin), tolerance = 1e-3)
# })

# testthat::test_that("Two random-effects in three-level model", {
#   testthat::skip_on_cran()
#   pbc.merlin$region <- 1
#   pbc.merlin$region[200:396]   <- 2
#   pbc.merlin$region[397:607]   <- 3
#   pbc.merlin$region[608:803]   <- 4
#   pbc.merlin$region[804:1001]  <- 5
#   pbc.merlin$region[1002:1202] <- 6
#   pbc.merlin$region[1203:1402] <- 7
#   pbc.merlin$region[1403:1601] <- 8
#   pbc.merlin$region[1602:1799] <- 9
#   pbc.merlin$region[1800:1945] <- 10
#   fit.merlin <- merlin(model = list(logp ~ rcs(year, df = 4) + age + trt + M1[region] * 1 + M2[id] * 1 + trt:M3[id] * 1), family = "gaussian", data = pbc.merlin, timevar = "year", levels = c("region", "id"))
#   fit.mlsurv <- mlrcs(formula = logp ~ 1 + rcs(year, df = 4) + age + trt, random =list(~ 1 | region, ~ 1 + trt | id), data = pbc.merlin)
#   testthat::expect_equal(object = coef(fit.mlsurv), expected = coef(fit.merlin), tolerance = 1e-3)
# })

testthat::test_that("expect error if malformed call", {
  testthat::expect_error(object = mlrcs(formula = logp ~ 1 + age + trt, random = ~ 1 | id, data = pbc.merlin))
})
