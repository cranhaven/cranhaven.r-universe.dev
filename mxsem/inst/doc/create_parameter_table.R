## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include = FALSE---------------------------------------------------
library(mxsem)

## -----------------------------------------------------------------------------
library(mxsem)

model <- '
  # latent variable definitions
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + a1*y2 + b*y3 + c1*y4
     dem65 =~ y5 + a2*y6 + b*y7 + c2*y8

  # regressions
    dem60 ~ g1*ind60
    dem65 ~ g2*ind60 + g3*dem60

  # residual correlations
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8
    
! delta_a
! g1g3
a2   := a1 + delta_a
g1g3 := g1*g3
'

model_list <- mxsem(model = model,
                    data  = OpenMx::Bollen,
                    return_parameter_table = TRUE)

print(model_list$parameter_table)

