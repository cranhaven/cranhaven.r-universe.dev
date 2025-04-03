## ----load-lib, echo=FALSE-----------------------------------------------------
library(PKPDsim)

## ----cov----------------------------------------------------------------------
covariates <- list(
  "WT" = new_covariate(value = 70),
  "SCR" = new_covariate(value = 120)
)

## ----timevarying-cov----------------------------------------------------------
covariates <- list(
  "WT" = new_covariate(value = 70),
  "CR" = new_covariate(
    value = c(0.8, 1, 1.2),
    times = c(0, 48, 72)
  )
)

## ----cov-table----------------------------------------------------------------
cov_table <- data.frame(
  id  = c(1, 1, 2, 3),
  WT  = c(40, 45, 50, 60),
  SCR = c(50, 150, 90, 110),
  t   = c(0, 5, 0, 0)
)

## ----full-ex------------------------------------------------------------------
parameters <- list(
  CL = 1,
  V = 10,
  KA = 0.5
)
n_ind <- 50
cov_table <- data.frame(
  'id' = 1:n_ind,
  'WT' = rnorm(n_ind, mean = 70, sd = 5)
)

model <- new_ode_model(
  code = '
     CLi = CL * pow((WT/70), 0.75)
     Vi  = V * (WT/70)
     dAdt[1] = -KA*A[1]
     dAdt[2] =  KA*A[1] -(CLi/Vi)*A[2]
   ',
   declare_variables = c('CLi', 'Vi'),
   covariates = c('WT'),
   dose = list(cmt = 1),
   obs = list(cmt = 2, scale = 'V * (WT/70)')
)

regimen <- new_regimen(
  amt  = 30,
  n = 4,
  type = 'bolus',
  interval = 12
)

dat <- sim(
  ode = model,
  parameters = parameters,
  t_obs = c(0.5, 2, 4, 8, 12, 16, 24),
  n_ind = n_ind,
  regimen = regimen,
  covariates_table = cov_table,
  output_include = list(covariates = TRUE)
)

