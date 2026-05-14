## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE, echo = TRUE, fig.height = 6, fig.width = 8, message = FALSE, warning = FALSE
)

## ----install0, eval = FALSE---------------------------------------------------
#  install.packages("shinySIR")
#  

## ----install, eval = FALSE----------------------------------------------------
#  install.packages("devtools")
#  library("devtools")
#  
#  install_github("SineadMorris/shinySIR")
#  

## ----SIR, eval = FALSE--------------------------------------------------------
#  library(shinySIR)
#  
#  run_shiny(model = "SIR")
#  

## ----echo = FALSE-------------------------------------------------------------
library(shinySIR)

## ----mymodel, eval = FALSE----------------------------------------------------
#  mySIRS <- function(t, y, parms) {
#  
#      with(as.list(c(y, parms)),{
#  
#          # Change in Susceptibles
#          dS <- - beta * S * I + delta * R
#  
#          # Change in Infecteds
#          dI <- beta * S * I - gamma * I
#  
#          # Change in Recovereds
#          dR <- gamma * I - delta * R
#  
#      return(list(c(dS, dI, dR)))
#      })
#  }
#  

## ----mymodel_run, eval = FALSE------------------------------------------------
#  
#  run_shiny(model = "SIRS (w/out demography)",
#            neweqns = mySIRS,
#            ics = c(S = 9999, I = 1, R = 0),
#            parm0 = c(beta = 5e-5, gamma = 1/7, delta = 0.1),
#            parm_names = c("Transmission rate", "Recovery rate", "Loss of immunity"),
#            parm_min = c(beta = 1e-5, gamma = 1/21, delta = 1/365),
#            parm_max = c(beta = 9e-5, gamma = 1 , delta = 1))
#  

