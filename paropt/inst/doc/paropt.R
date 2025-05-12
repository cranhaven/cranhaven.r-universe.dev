## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----eval = FALSE, echo = TRUE------------------------------------------------
#  # Optimize (all parameters are constant)
#  ode <- function(t, y, ydot, parameter) {
#    a_db = at(parameter, 1)
#    b_db = at(parameter, 2)
#    c_db = at(parameter, 3)
#    d_db = at(parameter, 4)
#    predator_db = at(y,1)
#    prey_db = at(y, 2)
#    ydot[1] = predator_db*prey_db*c_db - predator_db*d_db
#    ydot[2] = prey_db*a_db - prey_db*predator_db*b_db
#  }
#  path <- system.file("examples", package = "paropt")
#  states <- read.table(paste(path,"/states_LV.txt", sep = ""), header = TRUE)
#  lb <- data.frame(time = 0, a = 0.8, b = 0.3, c = 0.09, d = 0.09)
#  ub <- data.frame(time = 0, a = 1.3, b = 0.7, c = 0.4, d = 0.7)
#  set.seed(1)
#  res <- paropt::optimize(ode,
#                          lb = lb, ub = ub,
#                          reltol = 1e-06, abstol = c(1e-08, 1e-08),
#                          error = 0.0001,
#                          npop = 40, ngen = 1000,
#                          states = states)
#  
#  insilico <- res[[3]]
#  par(mfrow = c(2, 1))
#  plot(states$time, states$n1, type = "l")
#  points(insilico$time, insilico$n1, type = "l", col = "darkred")
#  
#  plot(states$time, states$n2, type = "l")
#  points(insilico$time, insilico$n2, type = "l", col = "darkred")

## ----eval = FALSE-------------------------------------------------------------
#  # Optimize (parameter a,b and c are constant. d is variable!)
#  r <- function(a) {
#    c(a, rep(NA, 3))
#  }
#  
#  lb <- data.frame(time = c(0, 20, 60, 80),
#                   a = r(0.8), b = r(0.3), c = r(0.09), d = 0.1)
#  ub <- data.frame(time = c(0, 20, 60, 80),
#                   a = r(1.3), b = r(0.7), c = r(0.4), d = 0.6)
#  set.seed(1)
#  res <- paropt::optimize(ode,
#                          lb = lb, ub = ub,
#                          reltol = 1e-06, abstol = c(1e-08, 1e-08),
#                          error = 0.0001,
#                          npop = 40, ngen = 10000,
#                          states = states)
#  
#  insilico <- res[[3]]
#  par(mfrow = c(2, 1))
#  plot(states$time, states$n1, type = "l")
#  points(insilico$time, insilico$n1, type = "l", col = "darkred")
#  
#  plot(states$time, states$n2, type = "l")
#  points(insilico$time, insilico$n2, type = "l", col = "darkred")

