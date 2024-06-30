test_that("testing individual algebras", {
  set.seed(123)
  dataset <- simulate_moderated_nonlinear_factor_analysis(N = 50)

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

  ind_alg <- get_individual_algebra_results(mxModel = mod)

  testthat::expect_true(length(ind_alg) == 1)
  testthat::expect_true(names(ind_alg) == c("a"))
  testthat::expect_true(all(colnames(ind_alg$a) == c("person", "k", "algebra_result")))

  testthat::expect_error(get_individual_algebra_results(mxModel = mod,
                                                        algebra_names = "b"))

  model <- "
  xi  =~ x1 + x2 + x3
  eta =~ y1 + y2 + y3
  eta ~  {a := a0 + data.k*a1}*xi
  "

  mod2 <- mxsem(model = model,
                data = dataset) |>
    mxTryHard()

  ind_alg <- get_individual_algebra_results(mxModel = mod2)

  testthat::expect_true(length(ind_alg) == 1)
  testthat::expect_true(names(ind_alg) == c("a"))
  testthat::expect_true(all(colnames(ind_alg$a) == c("person", "k", "algebra_result")))

  testthat::expect_error(get_individual_algebra_results(mxModel = mod2,
                                                        algebra_names = "b"))

  model <- "
  xi  =~ x1 + x2 + x3
  eta =~ y1 + y2 + y3
  eta ~  xi
  "
  mod3 <- mxsem(model = model,
                data = dataset) |>
    mxTryHard()

  testthat::expect_error(get_individual_algebra_results(mxModel = mod3))
})
