test_that("too-many-modifiers works", {
  library(mxsem)
  model <- '
  # latent variable definitions
     ind60 =~ x1 + a*1*x2 + x3
     dem60 =~ y1 + a1*y2 + b*y3 + c1*y4
     dem65 =~ y5 + a2*y6 + b*y7 + c2*y8
'

  testthat::expect_error(
    mxsem(model = model,
               data  = OpenMx::Bollen)
  )

})
