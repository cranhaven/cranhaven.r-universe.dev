## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo = F----------------------------------------------------------------
suppressPackageStartupMessages(library(phaseR))

## ---- echo = F, fig.dim = c(7, 5)---------------------------------------------
example1_flowField    <- flowField(example1,
                                   xlim   = c(0, 10), 
                                   ylim   = c(-5, 5),
                                   system = "one.dim",
                                  add    = F)
example1_trajectories <- trajectory(example1,
                                    y0     = c(-4, -1, 1, 4),
                                    tlim   = c(0, 10),
                                    system = "one.dim",
                                    col    = rep("black", 4),
                                    add    = T)

## ---- echo = F, fig.dim = c(7, 5)---------------------------------------------
example1_phasePortrait  <- phasePortrait(example1,
                                         ylim   = c(-5, 5))

## ---- echo = F, fig.dim = c(7, 5)---------------------------------------------
lotkaVolterra_flowField    <- flowField(lotkaVolterra,
                                        xlim       = c(0, 5), 
                                        ylim       = c(0, 5),
                                        parameters = c(1, 1, 1, 1),
                                        add        = F)
lotkaVolterra_trajectories <- trajectory(lotkaVolterra,
                                         y0     = rbind(c(2, 2),
                                                        c(0.5, 0.5),
                                                        c(0.5, 1.5),
                                                        c(1.5, 0.5),
                                                        c(3, 3)),
                                         parameters = c(1, 1, 1, 1),
                                         col    = rep("black", 5),
                                         tlim   = c(0, 100))

## ---- echo = F, fig.dim = c(7, 5)---------------------------------------------
lotkaVolterra_flowField    <- flowField(lotkaVolterra,
                                        xlim       = c(0, 5), 
                                        ylim       = c(0, 5),
                                        parameters = c(1, 1, 1, 1),
                                        add        = F)
lotkaVolterra_trajectories <- nullclines(lotkaVolterra,
                                         xlim      = c(-0.5, 5),
                                         ylim      = c(-0.5, 5),
                                         parameters = c(1, 1, 1, 1),
                                         points = 251)

## ---- echo = F, fig.dim = c(7, 5)---------------------------------------------
lotkaVolterra_numericalSolution <- numericalSolution(lotkaVolterra,
                                                     y0 = c(3, 4),
                                                     tlim = c(0, 50),
                                                     parameters = c(1, 1, 1, 1))

## ---- eval = F----------------------------------------------------------------
#  drawManifolds(deriv, y0 = NULL, parameters = NULL, tstep = 0.1, tend = 1000,
#                col = c("green", "red"), add.legend = TRUE,
#                state.names = c("x", "y"), ...)

## ---- eval = F----------------------------------------------------------------
#  findEquilibrium(deriv, y0 = NULL, parameters = NULL, system = "two.dim",
#                  tol = 1e-16, max.iter = 50, h = 1e-6, plot.it = FALSE,
#                  summary = TRUE,
#                  state.names = if (system == "two.dim") c("x", "y") else "y")

## ---- eval = F----------------------------------------------------------------
#  flowField(deriv, xlim, ylim, parameters = NULL, system = "two.dim", points = 21,
#            col = "gray", arrow.type = "equal", arrow.head = 0.05, frac = 1,
#            add = TRUE,
#            state.names = if (system == "two.dim") c("x", "y") else "y",
#            xlab = if (system == "two.dim") state.names[1] else "t",
#            ylab = if (system == "two.dim") state.names[2] else state.names[1],
#            ...)

## ---- eval = F----------------------------------------------------------------
#  nullclines(deriv, xlim, ylim, parameters = NULL, system = "two.dim",
#             points = 101, col = c("blue", "cyan"), add = TRUE, add.legend = TRUE,
#             state.names = if(system == "two.dim") c("x", "y") else "y", ...)

## ---- eval = F----------------------------------------------------------------
#  numericalSolution(deriv, y0 = NULL, tlim, tstep = 0.01, parameters = NULL,
#                    type = "one", col = c("red", "blue"), add.grid = TRUE,
#                    add.legend = TRUE, state.names = c("x", "y"), xlab = "t",
#                    ylab = state.names)

## ---- eval = F----------------------------------------------------------------
#  phasePlaneAnalysis(deriv, xlim, ylim, tend = 100, parameters = NULL,
#                     system = "two.dim", add = FALSE,
#                     state.names = if (system == "two.dim") c("x", "y") else "y")

## ---- eval = F----------------------------------------------------------------
#  phasePortrait(deriv, ylim, ystep = 0.01, parameters = NULL, points = 10,
#                frac = 0.75, arrow.head = 0.075, col = "black", add.grid = TRUE,
#                state.names = "y", xlab = state.names,
#                ylab = paste0("d", state.names), ...)

## ---- eval = F----------------------------------------------------------------
#  stability(deriv, ystar = NULL, parameters = NULL, system = "two.dim", h = 1e-07,
#            summary = TRUE,
#            state.names = if (system == "two.dim") c("x", "y") else "y")

## ---- eval = F----------------------------------------------------------------
#  trajectory(deriv, y0 = NULL, n = NULL, tlim, tstep = 0.01, parameters = NULL,
#             system = "two.dim", col = "black", add = TRUE,
#             state.names = if (system == "two.dim") c("x", "y") else "y", ...)

## ---- eval = F----------------------------------------------------------------
#  derivative <- function(t, y, parameters) {
#    # Enter derivative computation here
#    list(dy)
#  }

## ---- eval = F----------------------------------------------------------------
#  derivative <- function(t, y, parameters) {
#    x  <- y[1]
#    y  <- y[2]
#    dy <- c(3*y, 2*x)
#    list(dy)
#  }

## ---- eval = F----------------------------------------------------------------
#  derivative <- function(t, y, parameters) {
#    alpha <- parameters[1]
#    beta  <- parameters[2]
#    x     <- y[1]
#    y     <- y[2]
#    dy    <- c(alpha*y, beta*x)
#    list(dy)
#  }

## ---- eval = F----------------------------------------------------------------
#  derivative <- function(t, y, parameters) {
#    a  <- parameters[1]
#    b  <- parameters[2]
#    dy <- a*(b – 3 - y)^2
#    list(dy)
#  }

## ---- echo = T, fig.dim = c(7, 5)---------------------------------------------
example2_flowField  <- flowField(example2,
                                 xlim    = c(0, 4),
                                 ylim    = c(-1, 3),
                                 system  = "one.dim",
                                 add     = FALSE)
grid()
example2_nullclines <- nullclines(example2,
                                  xlim   = c(0, 4),
                                  ylim   = c(-1, 3),
                                  system = "one.dim")
example2_trajectory <- trajectory(example2,
                                  y0     = c(-0.5, 0.5, 1.5, 2.5),
                                  tlim   = c(0, 4),
                                  system = "one.dim")

## ---- echo = T, fig.dim = c(7, 5)---------------------------------------------
example2_phasePortrait <- phasePortrait(example2,
                                        ylim = c(-0.5, 2.5))

## ---- echo = T----------------------------------------------------------------
example2_stability_1 <- stability(example2,
                                  ystar  = 0,
                                  system = "one.dim")
example2_stability_2 <- stability(example2,
                                  ystar  = 1,
                                  system = "one.dim")
example2_stability_3 <- stability(example2,
                                  ystar  = 2,
                                  system = "one.dim")

## ---- echo = T, fig.dim = c(7, 5)---------------------------------------------
logistic_flowField  <- flowField(logistic,
                                 xlim       = c(0, 5),
                                 ylim       = c(-1, 3),
                                 parameters = c(1, 2),
                                 system     = "one.dim",
                                 add        = FALSE)
grid()
logistic_nullclines <- nullclines(logistic,
                                  xlim       = c(0, 5),
                                  ylim       = c(-1, 3),
                                  parameters = c(1, 2),
                                  system     = "one.dim")
logistic_trajectory <- trajectory(logistic,
                                  y0         = c(-0.5, 0.5, 1.5, 2.5),
                                  tlim       = c(0, 5),
                                  parameters = c(1, 2),
                                  system     = "one.dim")

## ---- echo = T, fig.dim = c(7, 5)---------------------------------------------
logistic_phasePortrait <- phasePortrait(logistic,
                                        ylim       = c(-0.5, 2.5),
                                        parameters = c(1, 2))

## ---- echo = T----------------------------------------------------------------
logistic_stability_1 <- stability(logistic,
                                  ystar      = 0,
                                  parameters = c(1, 2),
                                  system     = "one.dim")
logistic_stability_2 <- stability(logistic,
                                  ystar      = 2,
                                  parameters = c(1, 2),
                                  system     = "one.dim")

## ---- echo = T, fig.dim = c(7, 5)---------------------------------------------
example4_flowField  <- flowField(example4,
                                 xlim = c(-3, 3),
                                 ylim = c(-5, 5),
                                 add  = FALSE)
grid()
example4_nullclines <- nullclines(example4,
                                  xlim = c(-3, 3),
                                  ylim = c(-5, 5))
y0                  <- matrix(c(1, 0, -1, 0, 2, 2, -2, 2, 3, -3, -3, -4),
                              6, 2, byrow = TRUE)
example4_trajectory <- trajectory(example4,
                                  y0   = y0,
                                  tlim = c(0, 10))

## ---- echo = T, fig.dim = c(7, 5)---------------------------------------------
example5_flowField  <- flowField(example5,
                                 xlim = c(-3, 3), 
                                 ylim = c(-3, 3),
                                 add  = FALSE)
grid()
example5_nullclines <- nullclines(example5,
                                  xlim = c(-3, 3), 
                                  ylim = c(-3, 3))
y0                  <- matrix(c(1, 0, -1, 0, 2, 2, -2, 2, 0, 3, 0, -3),
                              6, 2, byrow = TRUE)
example5_trajectory <- trajectory(example5,
                                  y0   = y0,
                                  tlim = c(0, 10))
example5_manifolds  <- drawManifolds(example5,
                                     y0 = c(0, 0))

## ---- echo = T----------------------------------------------------------------
example5_stability <- stability(example5,
                                ystar = c(0, 0))

## ---- echo = T, fig.dim = c(7, 5)---------------------------------------------
example8_flowField  <- flowField(example8,
                                 xlim = c(-3, 3),
                                 ylim = c(-3, 3),
                                 add  = FALSE)
grid()
example8_nullclines <- nullclines(example8,
                                  xlim = c(-3, 3), 
                                  ylim = c(-3, 3))
y0                  <- matrix(c(1, 0, 0, 0.5, 2, -2, -2, -2),  
                              4, 2, byrow = TRUE)
example8_trajectory <- trajectory(example8,
                                  y0   = y0,
                                  tlim = c(0, 10))

## ---- echo = T----------------------------------------------------------------
example8_stability <- stability(example8,
                                ystar = c(0, 0))

## ---- echo = T, fig.dim = c(7, 5)---------------------------------------------
example11_flowField  <- flowField(example11,
                                  xlim = c(-5, 5), 
                                  ylim = c(-5, 5),
                                  add  = FALSE)
grid()
example11_nullclines <- nullclines(example11,
                                   xlim   = c(-5, 5),
                                   ylim   = c(-5, 5),
                                   points = 500)
y0                   <- matrix(c(4, 4, -1, -1, -2, 1, 1, -1), 
                               4, 2, byrow = TRUE)
example11_trajectory <- trajectory(example11,
                                   y0   = y0,
                                   tlim = c(0, 10))

## ---- echo = T----------------------------------------------------------------
example11_stability_1 <- stability(example11,
                                   ystar = c(0, 0))
example11_stability_2 <- stability(example11,
                                   ystar = c(0, 2))
example11_stability_3 <- stability(example11,
                                   ystar = c(1, 1),
                                   h     = 1e-8)
example11_stability_4 <- stability(example11,
                                   ystar = c(3, 0))

## ---- echo = T, fig.dim = c(7, 5)---------------------------------------------
example12_flowField  <- flowField(example12,
                                  xlim = c(-4, 4),
                                  ylim = c(-4, 4),
                                  add  = FALSE)
grid()
example12_nullclines <- nullclines(example12,
                                   xlim   = c(-4, 4), 
                                   ylim   = c(-4, 4),
                                   points = 500)
y0                   <- matrix(c(2, 2, -3, 0, 0, 2, 0, -3), 
                               4, 2, byrow = TRUE)
example12_trajectory <- trajectory(example12,
                                   y0   = y0,
                                   tlim = c(0, 10))

## ---- echo = T----------------------------------------------------------------
example12_stability_1 <- stability(example12,
                                   ystar = c(1, 1))
example12_stability_2 <- stability(example12,
                                   ystar = c(-1, -1),
                                   h     = 1e-8)

## ---- echo = T, fig.dim = c(7, 5)---------------------------------------------
simplePendulum_flowField  <- flowField(simplePendulum,
                                       xlim       = c(-7, 7),
                                       ylim       = c(-7, 7),
                                       parameters = 5,
                                       add        = FALSE)
grid()
simplePendulum_nullclines <- nullclines(simplePendulum,
                                        xlim       = c(-7, 7),
                                        ylim       = c(-7, 7),
                                        parameters = 5,
                                        points     = 500)
y0                        <- matrix(c(0, 1, 0, 4, -6, 1, 5, 0.5, 0, -3),
                                    5, 2, byrow = TRUE)
simplePendulum_trajectory <- trajectory(simplePendulum,
                                        y0         = y0,
                                        tlim       = c(0, 10),
                                        parameters = 5)

## ---- echo = T, fig.dim = c(7, 5)---------------------------------------------
simplePendulum_stability_1 <- stability(simplePendulum,
                                        ystar      = c(0, 0),
                                        parameters = 5)
simplePendulum_stability_2 <- stability(simplePendulum,
                                        ystar      = c(pi, 0),
                                        parameters = 5)

## ---- echo = T----------------------------------------------------------------
vanDerPol_stability_1 <- stability(vanDerPol, ystar = c(0, 0),
                                   parameters = 3)
vanDerPol_stability_2 <- stability(vanDerPol, ystar = c(0, 0),
                                   parameters = 1)

## ---- echo = T, fig.dim = c(7, 5)---------------------------------------------
vanDerPol_flowField  <- flowField(vanDerPol,
                                  xlim       = c(-5, 5), 
                                  ylim       = c(-5, 5),
                                  parameters = 3,
                                  add        = FALSE)
grid()
vanDerPol_nullclines <- nullclines(vanDerPol,
                                   xlim       = c(-5, 5), 
                                   ylim       = c(-5, 5),
                                   parameters = 3,
                                   points     = 500)
y0                   <- matrix(c(2, 0, 0, 2, 0.5, 0.5),
                               3, 2, byrow = TRUE)
vanDerPol_trajectory <- trajectory(vanDerPol,
                                   y0         = y0,
                                   tlim       = c(0, 10),
                                   parameters = 3)

