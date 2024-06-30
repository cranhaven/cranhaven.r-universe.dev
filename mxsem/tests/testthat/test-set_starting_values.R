test_that("starting values", {
  set.seed(123)
  library(mxsem)
  library(lavaan)

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
    set_starting_values(values = c("a" = .3)) |>
    mxRun(useOptimizer = FALSE)

  testthat::expect_equal(parameters(fit_mxsem)["a"], c("a" = .3))
  testthat::expect_error(
    fit_mxsem <- mxsem(model, data = PoliticalDemocracy) |>
      set_starting_values(values = c("abc" = .3)) |>
      mxRun(useOptimizer = FALSE)
  )
})
