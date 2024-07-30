## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----data---------------------------------------------------------------------
dat <- data.frame(
  x = c(
    -2, -2, -2, -2, -2, 
    -1, -1, -1, -1, -1, 
     0,  0,  0,  0,  0,
     1,  1,  1,  1,  1,
     2,  2,  2,  2,  2
  ),
  y = c(
    1, 0, 0, 0, 0,
    1, 1, 1, 0, 0,
    1, 1, 0, 0, 0,
    1, 1, 1, 1, 0,
    1, 1, 1, 1, 1
  )
)

## ----logisreg, message=FALSE, results='hide'----------------------------------
library(gfilogisreg)
set.seed(666L)
fidsamples <- gfilogisreg(y ~ x, data = dat, N = 500L)

## ----gfisummary---------------------------------------------------------------
gfiSummary(fidsamples)

## ----glm----------------------------------------------------------------------
glm(y ~ x, data = dat, family = binomial())

## ----mu-----------------------------------------------------------------------
gfiConfInt(~ -`(Intercept)`/x, fidsamples)

