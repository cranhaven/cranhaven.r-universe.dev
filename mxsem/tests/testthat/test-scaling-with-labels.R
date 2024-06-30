test_that("scaling with variables", {
  set.seed(123)
  library(lavaan)

  # automatic scaling
  l1s <- c("", "1*", "-1*", "-1.3*", "l1*")
  fits <- c()
  for(l1 in l1s){

    model <- paste0('
  # latent variable definitions
     ind60 =~ ', l1, 'x1 + l2*x2 + l3*x3
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
')

    if(l1 == "l1*"){
      testthat::expect_warning(
        fit_mxsem <- mxsem(model,
                           data = PoliticalDemocracy) |>
          mxTryHard()
      )
    }else if(l1 != ""){
      testthat::expect_message(
        fit_mxsem <- mxsem(model,
                           data = PoliticalDemocracy) |>
          mxTryHard()
      )
    }else{
      testthat::expect_no_warning(
        fit_mxsem <- mxsem(model,
                           data = PoliticalDemocracy) |>
          mxTryHard()
      )
    }
    fits <- c(fits, fit_mxsem$fitfunction$result[[1]])
    rm(fit_mxsem)
  }

  # all fits should be identical (the last model is not identified which basically
  # here results in the same fit)
  testthat::expect_true(all(abs(fits - fits[1]) < 1e-4))

  # automatic scaling using variance
  # automatic scaling
  v1s <- c("", "1*", ".5*", "v1*")
  fits <- c()
  for(v1 in v1s){

    model <- paste0('
  # latent variable definitions
     ind60 =~ x1 + l2*x2 + l3*x3
     dem60 =~ y1 + a*y2 + b*y3 + c*y4
     dem65 =~ y5 + a*y6 + b*y7 + c*y8

  # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60

    ind60 ~~ ',v1, 'ind60
  # residual correlations
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8
')

    if(v1 == "v1*"){
      testthat::expect_warning(
        fit_mxsem <- mxsem(model,
                           data = PoliticalDemocracy,
                           scale_loadings = FALSE,
                           scale_latent_variances = TRUE) |>
          mxTryHard()
      )
    }else if(v1 != ""){
      testthat::expect_message(
        fit_mxsem <- mxsem(model,
                           data = PoliticalDemocracy,
                           scale_loadings = FALSE,
                           scale_latent_variances = TRUE) |>
          mxTryHard()
      )
    }else{
      testthat::expect_no_warning(
        fit_mxsem <- mxsem(model,
                           data = PoliticalDemocracy,
                           scale_loadings = FALSE,
                           scale_latent_variances = TRUE) |>
          mxTryHard()
      )
    }
    fits <- c(fits, fit_mxsem$fitfunction$result[[1]])
    rm(fit_mxsem)
  }

  # all fits should be identical (the last model is not identified which basically
  # here results in the same fit)
  testthat::expect_true(all(abs(fits - fits[1]) < 1e-4))

})
