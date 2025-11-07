context("fit")
test_that("useOptimizerFalse", {
  lgcmFakeFit <- fit(lgcm, useOptimizer = FALSE, init = parameterValues)
  expect_equal(coef(lgcmFakeFit)[names(parameterValues)], parameterValues)
})

test_that("Paras in Confint", {
  # for this seed all true paras are in data set
  confInters <- confint(lgcmFit)
  confInters[1,1] <- confInters[1,1]-0.01
  for (cPar in names(trueParas)) {
    expect_true(confInters[cPar, 1] < trueParas[cPar] && confInters[cPar, 2] > trueParas[cPar])
  }
})

test_that("always reach ML estimates", {
  skip_on_cran()
  warn <- testit::has_warning(lgcmFit2 <- fit(lgcm, init = trueParas))
  if (!warn) {
    expect_equal(lgcmFit2, lgcmFit, tolerance = 0.05)
  }
})

test_that("failing fit", {
  skip_on_cran()
  failFit <- gppm(
    "muI", "aConst",
    demoLGCM, "ID", "y"
  )
  expect_error(fit(failFit), "Stan error*")
})
