## ----load-lib, echo=FALSE-----------------------------------------------------
library(PKPDsim)

## ----export-example, eval = FALSE---------------------------------------------
#  p <- list(CL = 5, V = 50)
#  reg <- new_regimen (amt = 100, n = 4, interval = 12, type = "bolus",  cmt=1)
#  
#  new_ode_model(
#    code = "
#      dAdt[1] = -(CL/V) * A[1]
#    ",
#    dose = list(cmt = 1, bioav = 1),
#    obs = list(cmt = 1, scale = "V"),
#    parameters = p,
#    package = "pktest",
#    install = TRUE
#  )

## ----load-library, eval=F-----------------------------------------------------
#  library(pktest)
#  mod <- pktest::model()

## ----simulate, eval=F---------------------------------------------------------
#  sim(
#    ode = mod,
#    parameters = p,
#    regimen = reg
#  )

