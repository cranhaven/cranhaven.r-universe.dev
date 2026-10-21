## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## ----setup--------------------------------------------------------------------
library(evbsreg)

## ----data---------------------------------------------------------------------
data(itajai)
str(itajai)
summary(itajai$wind)

## ----fit----------------------------------------------------------------------
X <- cbind(1, itajai$pressure)
fit <- evbsreg.fit(X, itajai$wind)

data.frame(
  Parameter = c("beta0", "beta1", "alpha", "gamma"),
  Estimate  = round(fit$coeff, 4),
  SE        = round(c(fit$stderrors, fit$stderroralpha, fit$stderrorgama), 4)
)

## ----pvalues------------------------------------------------------------------
round(fit$pvalues, 4)

## ----diag---------------------------------------------------------------------
diag <- cnc_diagnostics(fit)

## Top four normalized eigenvalues
round(head(diag$eigenvalues_norm, 4), 5)

## Observations flagged at q = 7
which(diag$Bj[7, ] > diag$bq[7])

## ----plot-cnc, fig.width = 10, fig.height = 4.5-------------------------------
plot_cnc(diag, q = 7)

## ----deletion-----------------------------------------------------------------
fit82 <- evbsreg.fit(X[-82, ], itajai$wind[-82])
rc <- 100 * (fit82$coeff - fit$coeff) / abs(fit$coeff)
names(rc) <- c("beta0", "beta1", "alpha", "gamma")
round(rc, 2)

## ----residuals----------------------------------------------------------------
r <- rqrandomized(X, itajai$wind)
shapiro.test(r)

## ----envelope, fig.width = 6, fig.height = 6----------------------------------
envelope_qq(X, itajai$wind, nrep = 100)

## ----densities, fig.width = 7, fig.height = 5---------------------------------
plot_evbs_alpha()

## ----scripts, eval = FALSE----------------------------------------------------
# # Density figures (Figures 1-2)
# source(system.file("scripts/script_01_density_figures.R", package = "evbsreg"))
# 
# # Itajai application (Tables 1-3, Figures 3-6)
# source(system.file("scripts/script_02_itajai_application.R", package = "evbsreg"))
# 
# # Monte Carlo (Tables 4-9); set m <- 500 inside for a quick check
# source(system.file("scripts/script_03_simulation_scenario1.R", package = "evbsreg"))
# source(system.file("scripts/script_04_simulation_scenario2.R", package = "evbsreg"))
# source(system.file("scripts/script_05_simulation_scenario3.R", package = "evbsreg"))

