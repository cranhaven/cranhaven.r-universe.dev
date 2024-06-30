test_that("model name", {
  library(mxsem)

  model <- '
  # latent variable definitions
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + a1*y2 + b*y3 + c1*y4
     dem65 =~ y5 + a2*y6 + b*y7 + c2*y8

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

  fit <- mxsem(model = model,
               data  = OpenMx::Bollen) |>
    mxTryHard()

  model_named <- '
  ==== Bollen_Pol_Dem

  # latent variable definitions
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + a1*y2 + b*y3 + c1*y4
     dem65 =~ y5 + a2*y6 + b*y7 + c2*y8

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

  fit_named <- mxsem(model = model_named,
               data  = OpenMx::Bollen) |>
    mxTryHard()

  testthat::expect_true(fit_named$name == "Bollen_Pol_Dem")


  model_named <- '


  ==== Bollen_Pol_Dem ===========

  # latent variable definitions
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + a1*y2 + b*y3 + c1*y4
     dem65 =~ y5 + a2*y6 + b*y7 + c2*y8

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

  fit_named <- mxsem(model = model_named,
                     data  = OpenMx::Bollen) |>
    mxTryHard()

  testthat::expect_true(fit_named$name == "Bollen_Pol_Dem")
})
