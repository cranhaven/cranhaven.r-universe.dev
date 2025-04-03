## ----absorption-lagtime-example-----------------------------------------------
library("PKPDsim")

pk <- new_ode_model(
  code = " dAdt[1] = -KA * A[1]; dAdt[2] = KA*A[1] -(CL/V) * A[2] ",
  obs = list(cmt = 2, scale="V"),
  dose = list(cmt = 1),
  lagtime = "ALAG"
)

r <- new_regimen(
  amt = c(100, 100, 100),
  times = c(0, 12, 24),
  cmt = 1
)
p <- list(CL = 5, V  = 10, KA = 0.25, ALAG = 2)

res <- sim(
  ode = pk,
  n_ind = 25,
  omega = cv_to_omega(
    par_cv = list("CL" = 0.1, "V" = 0.1, "KA" = .1, "ALAG" = 0.3),
    p
  ),
  parameters = p,
  regimen = r,
  verbose = FALSE,
  only_obs = TRUE
)

## ----transit-compartments-example---------------------------------------------
library(PKPDsim)

parameters <- list(CL = 15, V = 50, MTT = 2.5, N = 4, KA = 1)
reg1 <- new_regimen(amt = 100, n = 3, interval = 12) # needs dummy doses

mod <- new_ode_model(
  code = "
  tad = t - t_prv_dose
  KTR = (N+1)/MTT
  LNFAC= log(2.506628)+(N+0.5)*log(N)-N
  dAdt[1] = exp(log(prv_dose) + log(KTR) + N*log(KTR * tad) - KTR * tad - LNFAC) - KA*A[1]
  dAdt[2] = KA*A[1]-CL/V*A[2]
",
  declare_variables = c("LNFAC", "KTR", "tad"),
  parameters = parameters,
  dose = list(cmt = 1, bioav = 0),
  obs = list(cmt = 2, scale = "V")
)

res <- sim(
  ode = mod,
  regimen = reg1,
  parameters = parameters,
  n = 5,
  omega = cv_to_omega(
    list(CL = 0.1, V = 0.1, MTT = 0.2, N =0.1, KA=0.1),
    parameters
  ),
  t_obs = seq(0, 36, .5)
)

