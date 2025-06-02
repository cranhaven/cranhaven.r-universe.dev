## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
#  obj = create_TRMF(A)

## ----eval=FALSE---------------------------------------------------------------
#  obj = TRMF_columns(obj,reg_type = "nnls",lambda=1)

## ----eval=FALSE---------------------------------------------------------------
#  obj = TRMF_trend(obj,numTS = 2,order = 2,lambdaD=1)

## ----eval=FALSE---------------------------------------------------------------
#  obj = TRMF_trend(obj,numTS = 3,order = 0.5,lambdaD=10)

## ----eval=FALSE---------------------------------------------------------------
#  obj = TRMF_regression(obj, Xreg, type = "global")

## ----eval=FALSE---------------------------------------------------------------
#  out = train(obj)

## ----eval=FALSE---------------------------------------------------------------
#  summary(out)
#  plot(out)
#  resid(out)
#  fitted(out)
#  

## ----eval=FALSE---------------------------------------------------------------
#  impute_TRMF(out)
#  coef(out)
#  Fm = out$Factors$Fm
#  Xm =out$Factors$Xm
#  predict(out)
#  

