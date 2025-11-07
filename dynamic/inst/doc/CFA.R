## ---- include = FALSE, message=FALSE, warning=FALSE---------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(lavaan)
dat <- lavaan::HolzingerSwineford1939
lavmod <- "visual  =~ x1 + x2 + x3
           textual =~ x4 + x5 + x6
           speed   =~ x7 + x8 + x9"
fit <- lavaan::cfa(model=lavmod,data=dat)

## ----fig.height=3, fig.width=7, message=FALSE, warning=FALSE, eval=FALSE------
#  library(dynamic)
#  dynamic::cfaHB(fit, plot = TRUE)

## ----out.width="98%", message=FALSE, warning=FALSE, echo=FALSE----------------
knitr::include_graphics("One.png")

## ----out.width="98%", message=FALSE, warning=FALSE, echo=FALSE----------------
knitr::include_graphics("Two.png")

## ----out.width="98%", message=FALSE, warning=FALSE, echo=FALSE----------------
knitr::include_graphics("Three.png")

## ----fig.height=3, fig.width=7, message=FALSE, warning=FALSE, eval=FALSE------
#  manmod <- "visualtextual =~ .419*x1 + .212*x2 + .203*x3 + .852*x4 + .847*x5 + .840*x6"
#  n <- 301
#  dynamic::cfaOne(model=manmod,n=n,manual=TRUE)

## ----out.width="98%", message=FALSE, warning=FALSE, echo=FALSE----------------
knitr::include_graphics("Four.png")

