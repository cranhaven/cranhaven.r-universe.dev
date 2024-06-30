test_that("MIMIC works", {
  library(lavaan)

  model <- "
  eta =~ y1 + y2 + y3 + y4
  eta ~  x1 + x2 + x3 + x4 + x5
  "

  suppressWarnings(data <- lavaan::simulateData(model))

  fit_lavaan <- cfa(model,
                    data = data,
                    missing = "ml",
                    fixed.x = FALSE)

  fit_mx  <- mxsem(model = model,
                   data = data,
                   add_exogenous_latent_covariances = FALSE) |>
    mxTryHard()

  testthat::expect_true(length(unique(names(coef(fit_lavaan)))) == length(omxGetParameters(fit_mx)))

  testthat::expect_true(abs(-2*logLik(fit_lavaan) -
                              fit_mx$fitfunction$result[[1]]) < 1e-4)
})
