## ----setup, echo = FALSE------------------------------------------------------
knitr::opts_knit$set(global.par = TRUE)
knitr::opts_chunk$set(fig.width = 7, fig.height = 4)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  y1 + y2 | sd1 + sd2 ~ x1 + x2 + x3 + ns(n) | z1 + z2 + z3 | treat + trial (+ groups)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  f <- "y1 + y2 | sd1 + sd2 ~ x1 + x2 + x3 + ns(n) | z1 + z2 + z3 | treat + trial + groups"
#  out <- bmeta_analyze(formula(f), data = df,
#          mcmc = list(ndiscard = 20000, nskip = 5, nkeep = 10000),
#          prior = list(model = "NoRecovery"))

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  Rho_init <- diag(3) # assume 3 by 3
#  Rho_init[upper.tri(Rho_init)] <-
#      Rho_init[lower.tri(Rho_init)] <- 0.2
#  out <- bmeta_analyze(formula(f), data = df,
#          prior = list(model = "EquiCorr"), # abbreviation allowed
#          control = list(sample_Rho = FALSE),
#          init = list(Rho = Rho_init))

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  f <- "y | sd ~ x1 + x2 + x3 + ns(n) | z1 + z2 + z3 | treat + trial"
#  out <- bmeta_analyze(formula(f), data = df,
#          mcmc = list(discard = 20000, nskip = 5, nkeep = 10000),
#          prior = list(df = 3)) # heavy-tailed random effects

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  Rho_init <- diag(nT) # nT = the number of treatments
#  Rho_init[upper.tri(Rho_init)] <-
#      Rho_init[lower.tri(Rho_init)] <- 0.2
#  out <- bmeta_analyze(formula(f), data = df,
#          control = list(sample_df = TRUE, sample_Rho = FALSE),
#          init = list(Rho = Rho_init))

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  f <- "y | sd ~ x1 + x2 + x3 + ns(n) | treat + trial"

