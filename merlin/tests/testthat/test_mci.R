testthat::context("mci")
#############################################

testthat::test_that("Test 1.1", {
  data("pbc.merlin", package = "merlin")
  lmemod <- nlme::lme(logb ~ 1, random = ~ 1 | id, data = pbc.merlin)
  lmemodres <- as.numeric(c(summary(lmemod)$tTable[1, 1], log(as.numeric(nlme::VarCorr(lmemod)[c(2, 1), 2])), lmemod$logLik))
  comp <- rep(0, length(lmemodres))
  mod <- merlin::merlin(
    model = list(logb ~ M1[id] * 1),
    levels = c("id"),
    family = "gaussian",
    data = pbc.merlin,
    control = list(ip = 55, intmethod = "halton")
  )
  modres <- as.numeric(c(mod$coefficients, mod$loglikelihood))
  testthat::expect_equal(object = (abs(modres - lmemodres) / (abs(lmemodres) + 1)), expected = comp, tolerance = 5e-2)
})
