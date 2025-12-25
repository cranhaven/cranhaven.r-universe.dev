## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = F,
  message = F
)

## ---- warning=F, message=FALSE, eval=TRUE-------------------------------------
library(bestridge)
data("trim32", package = "bestridge")

## -----------------------------------------------------------------------------
y <- trim32[, 1]
x <- as.matrix(trim32[, -1])
lm.bsrr <- bsrr(x, y)

## ---- eval=F------------------------------------------------------------------
#  coef(lm.bsrr, sparse = TRUE)

## ---- eval=F------------------------------------------------------------------
#  predict.bsrr <- predict(lm.bsrr, newx = x)

## ---- warning=FALSE, message = FALSE------------------------------------------
data("duke")
y <- duke$y
x <- as.matrix(duke[, -1])

## -----------------------------------------------------------------------------
logi.bsrr <- bsrr(x, y, family = "binomial", method = "sequential")

## -----------------------------------------------------------------------------
plot(logi.bsrr)

## ---- warning=FALSE, message = FALSE------------------------------------------
data(patient.data)

x <- patient.data$x
y <- patient.data$time
status <- patient.data$status

## -----------------------------------------------------------------------------
cox.bsrr <- bsrr(x, cbind(y, status), family = "cox")

## -----------------------------------------------------------------------------
summary(cox.bsrr)

## ---- eval=F------------------------------------------------------------------
#  lm.bsrr.ebic <- bsrr(x, y, tune = "ebic")

## ---- eval=F------------------------------------------------------------------
#  lm.bsrr.cv <- bsrr(x, y, tune = "cv", nfolds = 5)

## ---- eval=F------------------------------------------------------------------
#  my.lambda.list <- exp(seq(log(10), log(0.01), length.out = 10))
#  my.s.list <- 1:10
#  
#  lm.bsrr.seq <- bsrr(x, y, method = "sequential", s.list = my.s.list,
#                     lambda.list = my.lambda.list)

## ---- eval=F------------------------------------------------------------------
#  my.s.min <- 1
#  my.s.max <- 10
#  my.lambda.min <- 0.01
#  my.lambda.max <- 10
#  
#  lm.bsrr.powell <- bsrr(x, y, method = "pgsection",
#                        s.min = my.s.min, s.max = my.s.max,
#                        lambda.min = my.lambda.min, lambda.max = my.lambda.max)

## ---- eval=F------------------------------------------------------------------
#  lm.bsrr.screening <- bsrr(x, y, screening.num = round(nrow(x)/log(nrow(x))))

