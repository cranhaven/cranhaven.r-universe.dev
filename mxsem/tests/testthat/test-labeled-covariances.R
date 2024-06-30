test_that("labeled covariances", {
  library(mxsem)

  model <- '
  # latent variable definitions
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + a1*y2 + b*y3 + c1*y4
     dem65 =~ y5 + a2*y6 + b*y7
    ind60 ~y8
    ind60 ~~ v1 * ind60
    dem60 ~~ v2 * ind60 + v3*dem60
    dem65 ~~ v4 * ind60 + v5*dem60 + v6*dem65
'

  fit <- mxsem(model = model,
               data  = OpenMx::Bollen) |>
    mxTryHard()

  for(i in paste0("v", 1:6)){
    testthat::expect_true(i %in% names(omxGetParameters(fit)))
  }


  model <- '
  # latent variable definitions
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + a1*y2 + b*y3 + c1*y4
     dem65 =~ y5 + a2*y6 + b*y7
    ind60 ~y8
    ind60 ~~ v1 * ind60
    dem60 ~~ v2 * ind60 + v3*dem60
    dem65 ~~ v4 * ind60 + v5*dem60 + v6*dem65

    x1 ~ int1*1; x2 ~ int1*1
'

  fit <- mxsem(model = model,
               data  = OpenMx::Bollen) |>
    mxTryHard()

  testthat::expect_true("int1" %in% names(omxGetParameters(fit)))

})
