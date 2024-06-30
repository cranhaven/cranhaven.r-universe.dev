test_that("mnlfa works", {

  set.seed(123)
  dataset <- simulate_moderated_nonlinear_factor_analysis(N = 2000)

  model <- "
  xi  =~ x1 + x2 + x3
  eta =~ y1 + y2 + y3
  eta ~  a*xi

  !a0
  !a1
  a := a0 + data.k*a1
  "

  mod <- mxsem(model = model,
                 data = dataset) |>
    mxTryHard()

  omxGetParameters(mod)

  testthat::expect_true(abs(omxGetParameters(mod)["a0"] - .7) < .1)
  testthat::expect_true(abs(omxGetParameters(mod)["a1"] - -.2) < .1)

  model <- "
  xi  =~ x1 + x2 + x3
  eta =~ y1 + y2 + y3
  eta ~  {a0 + data.k*a1}*xi
  "

  mod2 <- mxsem(model = model,
               data = dataset) |>
    mxTryHard()

  omxGetParameters(mod2)

  testthat::expect_true(abs(omxGetParameters(mod2)["a0"] - .7) < .1)
  testthat::expect_true(abs(omxGetParameters(mod2)["a1"] - -.2) < .1)

  testthat::expect_true(abs(omxGetParameters(mod)["a0"] - omxGetParameters(mod2)["a0"]) < .001)
  testthat::expect_true(abs(omxGetParameters(mod)["a1"] - omxGetParameters(mod2)["a1"]) < .001)

})
