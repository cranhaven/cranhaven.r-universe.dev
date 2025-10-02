## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ForecastTB)

## -----------------------------------------------------------------------------
a <- prediction_errors(data = nottem)  #`nottem` is a sample dataset in CRAN

a

## ----fig.height = 7, fig.width = 7, fig.align = "center"----------------------
b <- plot(a)

## ----fig.height = 7, fig.width = 7, fig.align = "center"----------------------
library(decomposedPSF)
test1 <- function(data, nval){
   return(lpsf(data = data, n.ahead = nval))
}

library(PSF)
test2 <- function(data, nval){
  a <- psf(data = data, cycle = 12)
  b <- predict(object = a, n.ahead = nval)
  return(b)
}

## ----fig.height = 7, fig.width = 7, fig.align = "center"----------------------
a1 <- prediction_errors(data = nottem, nval = 48, 
                        Method = c("test1(data, nval)", "test2(data, nval)"), 
                        MethodName = c("LPSF","PSF"), append_ = 1)
a1@output$Error_Parameters
b1 <- plot(a1)

## ----fig.height = 8, fig.width = 8, fig.align = "center"----------------------
library(forecast)
test3 <- function(data, nval){
  b <- as.numeric(forecast(ets(data), h = nval)$mean)
  return(b)
}

## ----fig.height = 7, fig.width = 7, fig.align = "center"----------------------
c1 <- append_(object = a1, Method = c("test3(data,nval)"), MethodName = c('ETS'))
c1@output$Error_Parameters
d1 <- plot(c1)

## ----fig.height = 7, fig.width = 7, fig.align = "center"----------------------
pcv <- function(obs, pred){
  d <- (var(obs) - var(pred)) * 100/ var(obs)
  d <- abs(as.numeric(d))
  return(d)
}

## ----fig.height = 7, fig.width = 7, fig.align = "center"----------------------
a1 <- prediction_errors(data = nottem, nval = 48, 
                        Method = c("test1(data, nval)", "test2(data, nval)"), 
                        MethodName = c("LPSF","PSF"), 
                        ePara = "pcv(obs, pred)", ePara_name = 'PCV',
                        append_ = 1)
a1@output$Error_Parameters
b1 <- plot(a1)

## ----fig.height = 6, fig.width = 8, fig.align = "left"------------------------
plot_circle(a1)

## ----fig.height = 7, fig.width = 7, fig.align = "center"----------------------
a1 <- prediction_errors(data = nottem, nval = 48, 
                        Method = c("test1(data, nval)"), 
                        MethodName = c("LPSF"), append_ = 1)
monte_carlo(object = a1, size = 180, iteration = 10)

## ----fig.height = 7, fig.width = 7, fig.align = "center"----------------------
monte_carlo(object = a1, size = 144, iteration = 2, fval = 1, figs = 1)

