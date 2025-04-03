## ----load-lib, echo=FALSE-----------------------------------------------------
library("PKPDsim")

## ----sim, eval=FALSE----------------------------------------------------------
#  dat <- sim(
#    ode = model,              # created using new_ode_model()
#    parameters = parameters,  # a named list of parameter values
#    regimen = regimen         # created using new_regimen
#  )

## ----sim-real-example---------------------------------------------------------
model <- new_ode_model("pk_1cmt_iv")
parameters <- list(CL = 5, V = 50)
regimen <- new_regimen(
  amt = 100,
  n = 3,
  interval = 12,
  type = "infusion",
  t_inf = 2
)

dat1 <- sim(
  ode = model,
  parameters = parameters,
  regimen = regimen
)
head(dat1)

## ----obs-times----------------------------------------------------------------
dat2 <- sim(
  ode = model,
  parameters = parameters,
  regimen = regimen,
  t_obs = c(0.5, 2, 4, 8, 12, 16, 24)
)
head(dat2)

