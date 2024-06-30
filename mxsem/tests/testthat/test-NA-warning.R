test_that("warning for NA works", {
  library(mxsem)
  model <- '
  # latent variable definitions
     ind60 =~ NA*x1 + x2 + x3
     dem60 =~ y1 + a1*y2 + b*y3 + c1*y4
     dem65 =~ y5 + a2*y6 + b*y7 + c2*y8
'

  testthat::expect_warning(
    mxsem(model = model,
          data  = OpenMx::Bollen)
  )
})
