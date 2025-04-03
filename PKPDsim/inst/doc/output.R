## ----load-lib, echo=FALSE-----------------------------------------------------
library("PKPDsim")

## ----output-------------------------------------------------------------------
mod <- new_ode_model(code = "
  dAdt[1] = -KA * A[1];
  dAdt[2] = -(CL/V) * A[2] + KA*A[1];
", obs = list(cmt = 2, scale = "V"))

## ----mult-obs-types-----------------------------------------------------------
mod <- new_ode_model(code = "
    dAdt[1] = -KA * A[1];
    dAdt[2] = -(CL/V) * A[2] + KA*A[1];
  ", 
  obs = list(
    cmt = c(2, 2),
    scale = c(1, "V"),
    label = c("abs", "conc")
  )
)
par <- list(CL = 5, V = 50, KA = .5)
reg <- new_regimen(amt = 100, n = 5, interval = 12)
res <- sim(
  ode = mod, 
  parameters = par, 
  regimen = reg, 
  only_obs = T
)

## ----parameters-vars-covs-----------------------------------------------------
mod_1cmt_iv <- new_ode_model("pk_1cmt_iv")
p <- list(CL = 5, V = 50)
reg <- new_regimen (amt = 100, n = 4, interval = 12, type = "bolus",  cmt=1)
cov_table <- data.frame(
  id  = c(1, 1, 2, 3),
  WT  = c(40, 45, 50, 60),
  SCR = c(50, 150, 90,110),
  t   = c(0, 5, 0, 0)
)

dat <- sim(
  ode = mod_1cmt_iv,
  parameters = p,
  regimen = reg,
  covariates_table = cov_table,
  covariates_implementation = list(SCR = "interpolate"),
  n_ind = 3,
  only_obs = T,
  output_include = list(parameters = TRUE, covariates=TRUE)
)
head(dat)

