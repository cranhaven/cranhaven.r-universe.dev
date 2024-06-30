test_that("implicit transformations", {


  set.seed(123)
  dataset <- simulate_moderated_nonlinear_factor_analysis(N = 2000)

  model <- "
  xi =~ x1 + x2 + x3
  eta =~ y1 + y2 + y3
  eta ~ a*xi

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



  set.seed(123)
  dataset <- simulate_moderated_nonlinear_factor_analysis(N = 2000)

  model_implicit <- "
  xi =~ x1 + x2 + x3
  eta =~ y1 + y2 + y3
  eta ~ {a0 + data.k*a1}*xi
  "

  mod_implicit <- mxsem(model = model_implicit,
                        data = dataset) |>
    mxTryHard()

  omxGetParameters(mod_implicit)

  testthat::expect_true(abs(omxGetParameters(mod_implicit)["a0"] - .7) < .1)
  testthat::expect_true(abs(omxGetParameters(mod_implicit)["a1"] - -.2) < .1)

  testthat::expect_true(abs(logLik(mod_implicit) - logLik(mod)) < 1e-4)

  model <- '
  # latent variable definitions
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + a1*y2 + b1*y3 + c*y4
     dem65 =~ y5 + a2*y6 + b2*y7 + c*y8

  # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60

  # residual correlations
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8

'

  fit_base <- mxsem(model, data = OpenMx::Bollen) |>
    mxTryHard()

  model_transform <- '
  # latent variable definitions
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + a1*y2 + b1*y3 + c*y4
     dem65 =~ y5 + {a1 + delta_a}*y6 + b2*y7 + c*y8

  # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60

  # residual correlations
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8

    !delta_b
    b2 := b1 + delta_b
'

  fit_transform <- mxsem(model_transform, data = OpenMx::Bollen) |>
    mxTryHard()

  testthat::expect_true(abs(logLik(fit_base) - logLik(fit_transform)) < 1e-4)
  testthat::expect_true(abs(omxGetParameters(fit_base)["a2"] -
                              sum(omxGetParameters(fit_transform)[c("a1", "delta_a")])) < 1e-4)

  model_transform <- '
  # latent variable definitions
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + a1*y2 + b1*y3 + c*y4
     dem65 =~ y5 + {a1 +
     b2 +
          delta_a}*y6 + b2*y7 + c*y8

  # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60

  # residual correlations
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8

    !delta_b
    b2 := b1 + delta_b
'

  fit_transform <- mxsem(model_transform, data = OpenMx::Bollen) |>
    mxTryHard()

  testthat::expect_true(abs(logLik(fit_base) - logLik(fit_transform)) < 1e-4)
  testthat::expect_true(abs(omxGetParameters(fit_base)["a2"] -
                              sum(omxGetParameters(fit_transform)[c("a1", "delta_a", "b1", "delta_b")])) < 1e-4)

  model_transform <- '
  # latent variable definitions
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + a1*y2 + b1*y3 + c*y4
     dem65 =~ y5 + {a2:=a1 +
     b2 +
          delta_a}*y6 + b2*y7 + c*y8

  # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60

  # residual correlations
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8

    !delta_b
    b2 := b1 + delta_b
'

  fit_transform <- mxsem(model_transform, data = OpenMx::Bollen) |>
    mxTryHard()

  testthat::expect_true(abs(logLik(fit_base) - logLik(fit_transform)) < 1e-4)
  testthat::expect_true(abs(omxGetParameters(fit_base)["a2"] -
                              sum(omxGetParameters(fit_transform)[c("a1", "delta_a", "b1", "delta_b")])) < 1e-4)


  # test numeric values in transformations
  model1 <- '
  # latent variable definitions
     dem60 =~ y1 + y2 + y3 + y4
  # predict the latent variance with an intercept and a slope using x1
     dem60 ~~ {latent_var := exp(0.0 + lv_1 * data.x1)} * dem60
'

  fit1 <- mxsem(model = model1,
                data  = OpenMx::Bollen) |>
    mxTryHard()

  model2 <- '
  # latent variable definitions
     dem60 =~ y1 + y2 + y3 + y4
  # predict the latent variance with an intercept and a slope using x1
     dem60 ~~ {latent_var := exp(lv_1 * data.x1)} * dem60
'

  fit2 <- mxsem(model = model2,
                data  = OpenMx::Bollen) |>
    mxTryHard()

  testthat::expect_true(abs(omxGetParameters(fit1)["lv_1"] -omxGetParameters(fit2)["lv_1"]) < 1e-4)
  testthat::expect_true(abs(logLik(fit1) - logLik(fit2)) < 1e-4)

  model1 <- '
  # latent variable definitions
     dem60 =~ y1 + y2 + y3 + y4
  # predict the latent variance with an intercept and a slope using x1
     dem60 ~~ {latent_var := exp(1 + lv_1 * data.x1)} * dem60
'

  fit1 <- mxsem(model = model1,
                data  = OpenMx::Bollen) |>
    mxTryHard()

  model2 <- '
  # latent variable definitions
     dem60 =~ y1 + y2 + y3 + y4
  # predict the latent variance with an intercept and a slope using x1
     dem60 ~~ {latent_var := exp(lv_0 + lv_1 * data.x1)} * dem60
'

  fit2 <- mxsem(model = model2,
                data  = OpenMx::Bollen) |>
    omxSetParameters(labels = "lv_0", free = FALSE, values = 1) |>
    mxTryHard()

  testthat::expect_true(abs(mxEval(latent_var, fit1) - exp(1 + omxGetParameters(fit1)["lv_1"]*OpenMx::Bollen[1,"x1"])) < 1e-4)
  testthat::expect_true(abs(omxGetParameters(fit1)["lv_1"] -omxGetParameters(fit2)["lv_1"]) < 1e-4)
  testthat::expect_true(abs(logLik(fit1) - logLik(fit2)) < 1e-4)


  model3 <- '
     !a
     a := 0
     l_0 := exp(a)
  # latent variable definitions
     dem60 =~ y1 + y2 + y3 + y4
  # predict the latent variance with an intercept and a slope using x1
     dem60 ~~ {latent_var := exp(l_0 + lv_1 * data.x1)} * dem60

'

  fit3 <- mxsem(model = model3,
                data  = OpenMx::Bollen) |>
    mxTryHard()

  testthat::expect_true(abs(mxEval(latent_var, fit3) - exp(1 + omxGetParameters(fit3)["lv_1"]*OpenMx::Bollen[1,"x1"])) < 1e-4)
  testthat::expect_true(abs(omxGetParameters(fit1)["lv_1"] -omxGetParameters(fit3)["lv_1"]) < 1e-4)
  testthat::expect_true(abs(logLik(fit1) - logLik(fit3)) < 1e-4)

  # test more complex transformation mapping [-infinity, infinity] in [0, 1]
  # https://math.stackexchange.com/questions/3200746/map-0-infinity-to-0-1
  model4 <- '
  # latent variable definitions
     dem60 =~ y1 + y2 + y3 + y4
  # predict the latent variance with an intercept and a slope using x1
     dem60 ~~ {latent_var := 1 - 1/(lv_par + 1)} * dem60
     !lv_0; !lv_1
     lv_par := lv_0 + lv_1 * data.x1
'

  fit4 <- mxsem(model = model4,
                data  = OpenMx::Bollen) |>
    mxTryHard()
  testthat::expect_true(abs(mxEval(latent_var, fit4) - (1 - 1/(omxGetParameters(fit4)["lv_0"] + omxGetParameters(fit4)["lv_1"]*OpenMx::Bollen[1,"x1"] + 1))) < 1e-4)

  model5 <- '
  # latent variable definitions
     dem60 =~ y1 + y2 + y3 + y4
  # predict the latent variance with an intercept and a slope using x1
     dem60 ~~ {latent_var := 1 - 1/(lv_0 + lv_1 * data.x1 + 1)} * dem60
'

  fit5 <- mxsem(model = model5,
                data  = OpenMx::Bollen) |>
    mxTryHard()
  testthat::expect_true(abs(mxEval(latent_var, fit5) - (1 - 1/(omxGetParameters(fit5)["lv_0"] + omxGetParameters(fit5)["lv_1"]*OpenMx::Bollen[1,"x1"] + 1))) < 1e-4)
  testthat::expect_true(abs(omxGetParameters(fit4)["lv_1"] -omxGetParameters(fit5)["lv_1"]) < 1e-4)
  testthat::expect_true(abs(logLik(fit4) - logLik(fit5)) < 1e-4)

  # check direct access to matrix elements
  model6 <- '
  # latent variable definitions
     dem60 =~ y1 + y2 + y3 + y4
  # predict the latent variance with an intercept and a slope using x1
     dem60 ~~ {latent_var := 1 - 1/(A[1,1] + lv_1 * data.x1 + 1)} * dem60
'

  fit6 <- mxsem(model = model6,
                data  = OpenMx::Bollen) |>
    mxTryHard()
  testthat::expect_true(abs(mxEval(latent_var, fit6) - (1 - 1/(fit6$A$values[1,1] + omxGetParameters(fit6)["lv_1"]*OpenMx::Bollen[1,"x1"] + 1))) < 1e-4)
  # check direct access to matrix elements
  model7 <- '
  # latent variable definitions
     dem60 =~ y1 + y2 + y3 + y4
  # predict the latent variance with an intercept and a slope using x1
     dem60 ~~ {latent_var := 1 - 1/(0 + lv_1 * data.x1 + 1)} * dem60
'

  fit7 <- mxsem(model = model7,
                data  = OpenMx::Bollen) |>
    mxTryHard()

  testthat::expect_true(abs(omxGetParameters(fit6)["lv_1"] -omxGetParameters(fit7)["lv_1"]) < 1e-4)
  testthat::expect_true(abs(logLik(fit6) - logLik(fit7)) < 1e-4)
})
