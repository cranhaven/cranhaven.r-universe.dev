test_that("Political Democracy works", {
  set.seed(123)
  library(lavaan)
  library(mxsem)

  model <- '
  # latent variable definitions
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + a*y2 + b*y3 + c*y4
     dem65 =~ y5 + a*y6 + b*y7 + c*y8

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

  fit_lavaan <- sem(model, data = PoliticalDemocracy)

  fit_mxsem <- mxsem(model, data = PoliticalDemocracy) |>
    mxTryHard()

  testthat::expect_true(abs(-2*logLik(fit_lavaan) - fit_mxsem$fitfunction$result[[1]]) < 1e-2)

})

