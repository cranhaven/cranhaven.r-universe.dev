test_that("CFA works", {

  # The following model is copied from Schoemann, A. M. (2015). Structural equation
  # modeling with R using lavaan. https://osf.io/5mvqt
  #
  # lavaan model by Alexander M. Schoemann
  # at https://osf.io/ijwtm
  library(lavaan)
  library(mxsem)

  set.seed(123)

  modCat <- '
reason =~ reason_4 + reason_16 + reason_17 + reason_19
letter =~ letter_7 + letter_33 + letter_34 + letter_58
matrix =~ matrix_45 + matrix_46 + matrix_47 + matrix_55
rotate =~ rotate_3 + rotate_4 + rotate_6 + rotate_8
'

  data <- lavaan::simulateData(modCat)

  fitCat <- cfa(modCat,
                data = data,
                std.lv = TRUE,
                missing = "ml")

  fit_mx  <- mxsem(model = modCat,
                   data = data,
                   scale_loadings = FALSE,
                   scale_latent_variances = TRUE) |>
    mxTryHard()


  testthat::expect_true(abs(-2*logLik(fitCat) -
                              fit_mx$fitfunction$result[[1]]) < 1e-4)

})
