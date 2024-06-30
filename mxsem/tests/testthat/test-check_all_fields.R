test_that("check all fields", {
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

  fit_mxsem <- mxsem(model, data = PoliticalDemocracy, return_parameter_table = TRUE)

  testthat::expect_no_error(mxsem:::check_all_fields(fit_mxsem$parameter_table))
  fit_mxsem$parameter_table$parameter_table$lhs[1] <- "abc_kjshdfgk\\uslkg"
  testthat::expect_error(mxsem:::check_all_fields(fit_mxsem$parameter_table))
  fit_mxsem$parameter_table$parameter_table$lhs[1] <- "abc_kjshd*uslkg"
  testthat::expect_error(mxsem:::check_all_fields(fit_mxsem$parameter_table))
  fit_mxsem$parameter_table$parameter_table$lhs[1] <- "abc_kjs.lkg"
  testthat::expect_error(mxsem:::check_all_fields(fit_mxsem$parameter_table))
  fit_mxsem$parameter_table$parameter_table$lhs[1] <- "_abc_"
  testthat::expect_no_error(mxsem:::check_all_fields(fit_mxsem$parameter_table))

  fit_mxsem$parameter_table$parameter_table$rhs[1] <- "abc_kjshdfgk\\uslkg"
  testthat::expect_error(mxsem:::check_all_fields(fit_mxsem$parameter_table))
  fit_mxsem$parameter_table$parameter_table$rhs[1] <- "abc_kjshd*uslkg"
  testthat::expect_error(mxsem:::check_all_fields(fit_mxsem$parameter_table))
  fit_mxsem$parameter_table$parameter_table$rhs[1] <- "abc_kjs.lkg"
  testthat::expect_error(mxsem:::check_all_fields(fit_mxsem$parameter_table))
  fit_mxsem$parameter_table$parameter_table$rhs[1] <- "_abc_"
  testthat::expect_no_error(mxsem:::check_all_fields(fit_mxsem$parameter_table))

  fit_mxsem$parameter_table$parameter_table$modifier[1] <- "{abc_kjshd*uslkg\\kjdf*lkdf}"
  testthat::expect_no_error(mxsem:::check_all_fields(fit_mxsem$parameter_table))

  fit_mxsem$parameter_table$user_defined <- "{abc_kjshd*uslkg\\kjdf*lkdf}"
  testthat::expect_no_error(mxsem:::check_all_fields(fit_mxsem$parameter_table))
  fit_mxsem$parameter_table$user_defined <- "abc_kjshd*uslkg\\kjdf*lkdf}"
  testthat::expect_error(mxsem:::check_all_fields(fit_mxsem$parameter_table))
  fit_mxsem$parameter_table$user_defined <- "{abc_kjshd*uslkg\\kjdf*lkdf"
  testthat::expect_error(mxsem:::check_all_fields(fit_mxsem$parameter_table))
})
