## ---- include=FALSE-----------------------------------------------------------

rm(list=ls())
gc()

hook_output <- knitr::knit_hooks$get("output")

knitr::knit_hooks$set(output = function(x, options) {
  if (!is.null(n <- options$out.lines)) {
    x <- xfun::split_lines(x)
    if (length(x) > n) {
      x <- c(head(x, n), "....\n")
    }
    x <- paste(x, collapse = "\n")
  }
  hook_output(x, options)
})


## ----startup, eval=FALSE, error=FALSE, warning=FALSE, message=FALSE-----------
#  
#  library(devtools)
#  devtools::install_github("stefan-1997/plasso")
#  
#  install.packages("plasso")
#  

## ----load---------------------------------------------------------------------

library(plasso)


## ----data---------------------------------------------------------------------

data(toeplitz)

y = as.matrix(toeplitz[,1])
X = toeplitz[,-1]


## ----fitplasso, out.lines=10--------------------------------------------------

p = plasso::plasso(X,y)


## ----plotplasso, error=FALSE, warning=FALSE, message=FALSE--------------------

plot(p, lasso=FALSE, xvar="lambda")

plot(p, lasso=TRUE, xvar="lambda")


## ----coefplasso, error=FALSE, warning=FALSE, message=FALSE--------------------

coef_p = coef(p, s=0.01)

as.vector(coef_p$plasso)
as.vector(coef_p$lasso)


## ----fitcvplasso--------------------------------------------------------------

p.cv = plasso::cv.plasso(X,y,kf=5)
summary(p.cv, default=FALSE)


## ----plotcvplasso, fig.width=7, fig.height=3----------------------------------

plot(p.cv, legend_pos="left", legend_size=0.5)


## ----index_min_plasso---------------------------------------------------------

p.cv$lambda_min_pl


## ----coef_min_plasso, out.lines=10--------------------------------------------

coef_pcv = coef(p.cv, S="optimal")
as.vector(coef_pcv$plasso)


