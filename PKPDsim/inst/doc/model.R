## ----cran, eval=FALSE---------------------------------------------------------
#  install.packages("PKPDsim")

## ----load-lib, echo=FALSE-----------------------------------------------------
library("PKPDsim")

## ----new-model----------------------------------------------------------------
pk1 <- new_ode_model(model = "pk_1cmt_oral")

## ----available-models, error=TRUE---------------------------------------------
new_ode_model()

## ----custom-model-------------------------------------------------------------
pk1 <- new_ode_model(code = "
  dAdt[1] = -KA * A[1]
  dAdt[2] = +KA * A[1] -(CL/V) * A[2]
")

## ----print-custom-model-------------------------------------------------------
pk1

## ----custom-model-variables---------------------------------------------------
pk1 <- new_ode_model(code = "
  KEL = CL/V
  dAdt[1] = -KA * A[1]
  dAdt[2] = +KA * A[1] -KEL * A[2]
", declare_variables = c("KEL"))

## ----custom-model-covariates--------------------------------------------------
pk1 <- new_ode_model(code = "
  CLi = WT/70
  KEL = CLi/V
  dAdt[1] = -KA * A[1]
  dAdt[2] = +KA * A[1] -(CL*(WT/70)/V) * A[2]
", declare_variables = c("KEL", "CLi"), covariates = c("WT"))

## ----custom-model-power-functions---------------------------------------------
pk1 <- new_ode_model(code = "
  CLi = CL * pow((WT/70), 0.75)
  dAdt[1] = -KA * A[1]
  dAdt[2] = +KA * A[1] -(CLi/V) * A[2]
", declare_variables = c("CLi"))

## ----bioav--------------------------------------------------------------------
pk1 <- new_ode_model(code = "
    dAdt[1] = -KA * A[1]
    dAdt[2] = +KA * A[1] -(CL/V) * A[2]
  ",
  dose = list(cmt = 1, bioav = "F1"),
  parameters = list(KA = 1, CL = 5, V = 50, F1 = 0.7)
)

## ----scale--------------------------------------------------------------------
mod <- new_ode_model(code = "
    dAdt[1] = -(CL/V)*A[1];
  ",
  dose = list(cmt = 1, bioav = "WT"),
  obs = list(cmt = 1, scale = "V"),
  covariates = list("WT" = new_covariate(value = 70))
)

## ----obs----------------------------------------------------------------------
pk1 <- new_ode_model(code = "
    dAdt[1] = -KA * A[1]
    dAdt[2] = +KA * A[1] -(CL/V) * A[2]
  ", 
  obs = list(cmt = 2, scale = "V")
)

## ----scale-2------------------------------------------------------------------
pk1 <- new_ode_model(code = "
    Vi = V * (WT/70)
    dAdt[1] = -KA * A[1]
    dAdt[2] = +KA * A[1] -(CL/Vi) * A[2]
  ", 
  obs = list(cmt = 2, scale = "V * (WT/70)")
)

## ----variable-----------------------------------------------------------------
pk1 <- new_ode_model(code = "
    dAdt[1] = -KA * A[1]
    dAdt[2] = +KA * A[1] -(CL/V) * A[2]
    CONC = A[2]
  ", 
  obs = list(variable = "CONC"),
  declare_variables = "CONC"
)

## ----model-from-file, eval=FALSE----------------------------------------------
#  pk1 <- new_ode_model(
#    file = "pk_1cmt_oral_nonlin_v1.txt",
#    declare_variables = c("KEL", "CLi")
#  )

