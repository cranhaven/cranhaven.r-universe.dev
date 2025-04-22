## ----setup, include = FALSE---------------------------------------------------
# show grouped code output instead of single lines
# use '#>' for R outputs
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(data.table)
library(ggplot2)

## ----GroupedData, echo=FALSE, fig.cap="Grouped Linear Data"-------------------
n = 40
dtLinData = data.table(Material = rep(LETTERS[1:3], each = n)
                       , Time = rep(seq(0, 5, length.out = n), 3)
                       , Intercept = rep(10, 3*n)
                       , Slope = rep(c(-0.1, -0.3, -0.4), each = n)
                       )
dtLinData[, Measurement := Intercept + Time*Slope + rnorm(3*n, sd = 0.05)]
ggplot(data = dtLinData, mapping = aes(Time, Measurement, colour = Material)) + 
  geom_point()

## ----GroupedDataFun, echo=FALSE, fig.cap="Grouped Linear Data with Fitted Function"----
ggplot(data = dtLinData, mapping = aes(Time, Measurement, colour = Material)) + 
  geom_point() + 
  geom_abline(aes(colour = Material, slope = Slope, intercept = Intercept))

## ----fitPreFunRunData, echo=FALSE, fig.cap="Fit Preselectable Function: Run Data"----
knitr::include_graphics("imgs/fitFunction01.png")

## ----fitPreFunMenu, echo=FALSE, fig.cap="Fit Preselectable Function: Menu"----
knitr::include_graphics("imgs/fitFunction02.png")

## ----fitPreFunVarSel, echo=FALSE, fig.cap="Fit Preselectable Function: Variable Selection"----
knitr::include_graphics("imgs/fitFunction03.png")

## ----fitPreFunRScript, echo=FALSE, fig.cap="Fit Preselectable Function: R Script"----
knitr::include_graphics("imgs/fitFunction04.png")

## ----fitPreFunRScriptVarsMenu, echo=FALSE, fig.cap="Fit Preselectable Function: R Script Variables Menu"----
knitr::include_graphics("imgs/fitFunction05.png")

## ----fitPreFunRScriptVars, echo=FALSE, fig.cap="Fit Preselectable Function: R Script Variables"----
knitr::include_graphics("imgs/fitFunction06.png")

## ----fitPreFunSummaries, echo=FALSE, fig.cap="Fit Preselectable Function: Result Menu"----
knitr::include_graphics("imgs/fitFunction07.png")

## ----fitPreFunFitted, echo=FALSE, fig.cap="Fit Preselectable Function: Fitted Data"----
knitr::include_graphics("imgs/fitFunction08.png")

## ----fitPreFunCoeffAB, echo=FALSE, fig.cap="Fit Preselectable Function: Scatterplot of Coefficients 'a' and 'b'."----
knitr::include_graphics("imgs/fitFunction09.png")

## ----fitUserFunData, echo=FALSE, fig.cap="Fit User Defined Function: Dissolution Data"----
knitr::include_graphics("imgs/fitFunction10.png")

## ----fitUserFunVarSel, echo=FALSE, fig.cap="Fit User Defined Function: Variable Selection"----
knitr::include_graphics("imgs/fitFunction11.png")

## ----fitUserFunScriptVars, echo=FALSE, fig.cap="Fit User Defined Function: Script Variables"----
knitr::include_graphics("imgs/fitFunction12.png")

## ----fitUserFunCoeffDS, echo=FALSE, fig.cap="Fit User Defined Function: Coefficient Table"----
knitr::include_graphics("imgs/fitFunction13.png")

## ----fitUserFunMulitVari, echo=FALSE, fig.cap="Fit User Defined Function: Multi-Vari Chart Coefficient 'A'"----
knitr::include_graphics("imgs/fitFunction14.png")

## ----fitFunLimitsDataGraph, echo=FALSE, fig.cap="Fit Function: Flattened Sinusoidal Oscillation"----
knitr::include_graphics("imgs/fitFunction15.png")

## ----fitFunLimitsSinGraph, echo=FALSE, fig.cap="Fit Function: Fitted Sinusoidal Oscillation"----
knitr::include_graphics("imgs/fitFunction16.png")

## ----fitFunLimitsScriptVariables, echo=FALSE, fig.cap="Fit Function: Script Variables"----
knitr::include_graphics("imgs/fitFunction17.png")

## ----fitFunLimitsSinLimitGraph, echo=FALSE, fig.cap="Fit Function: Fitted Sinusoidal Oscillation with Limits"----
knitr::include_graphics("imgs/fitFunction18.png")

## ----fitFunWeightsExample, echo=FALSE, fig.cap="Fit Function: Fit Linear Function on Hyperbolic Data"----
knitr::include_graphics("imgs/fitFunction19.png")

## ----fitFunWeightsScriptVars, echo=FALSE, fig.cap="Fit Function: Script Variables"----
knitr::include_graphics("imgs/fitFunction20.png")

## ----fitFunWeightsWeight, echo=FALSE, fig.cap="Fit Function: Weighted Linear Fit on Hyperbolic Data"----
knitr::include_graphics("imgs/fitFunction21.png")

