## ----setup, include=FALSE-----------------------------------------------------
library(knitr)
knitr::opts_chunk$set(
	fig.align = "center",
	fig.height = 5.5,
	fig.width = 6,
	warning = FALSE,
	collapse = TRUE,
	dev.args = list(pointsize = 10),
	out.width = "90%",
	par = TRUE
)
knit_hooks$set(par = function(before, options, envir)
  { if (before && options$fig.show != "none") 
       par(family = "sans", mar = c(4.1,4.1,1.1,1.1), mgp = c(3,1,0), tcl = -0.5)
})

## ---- message = FALSE, echo = FALSE-------------------------------------------
library(meteorits)

## -----------------------------------------------------------------------------
n <- 500 # Size of the sample
alphak <- matrix(c(0, 8), ncol = 1) # Parameters of the gating network
betak <- matrix(c(0, -2.5, 0, 2.5), ncol = 2) # Regression coefficients of the experts
lambdak <- c(3, 5) # Skewness parameters of the experts
sigmak <- c(1, 1) # Standard deviations of the experts
x <- seq.int(from = -1, to = 1, length.out = n) # Inputs (predictors)

# Generate sample of size n
sample <- sampleUnivSNMoE(alphak = alphak, betak = betak, sigmak = sigmak, 
                          lambdak = lambdak, x = x)
y <- sample$y

## -----------------------------------------------------------------------------
K <- 2 # Number of regressors/experts
p <- 1 # Order of the polynomial regression (regressors/experts)
q <- 1 # Order of the logistic regression (gating network)

## -----------------------------------------------------------------------------
n_tries <- 1
max_iter <- 1500
threshold <- 1e-6
verbose <- TRUE
verbose_IRLS <- FALSE

## -----------------------------------------------------------------------------
snmoe <- emSNMoE(X = x, Y = y, K, p, q, n_tries, max_iter, 
                 threshold, verbose, verbose_IRLS)

## -----------------------------------------------------------------------------
snmoe$summary()

## -----------------------------------------------------------------------------
snmoe$plot(what = "meancurve")

## -----------------------------------------------------------------------------
snmoe$plot(what = "confregions")

## -----------------------------------------------------------------------------
snmoe$plot(what = "clusters")

## -----------------------------------------------------------------------------
snmoe$plot(what = "loglikelihood")

## -----------------------------------------------------------------------------
data("tempanomalies")
x <- tempanomalies$Year
y <- tempanomalies$AnnualAnomaly

## -----------------------------------------------------------------------------
K <- 2 # Number of regressors/experts
p <- 1 # Order of the polynomial regression (regressors/experts)
q <- 1 # Order of the logistic regression (gating network)

## -----------------------------------------------------------------------------
n_tries <- 1
max_iter <- 1500
threshold <- 1e-6
verbose <- TRUE
verbose_IRLS <- FALSE

## -----------------------------------------------------------------------------
snmoe <- emSNMoE(X = x, Y = y, K, p, q, n_tries, max_iter, 
                 threshold, verbose, verbose_IRLS)

## -----------------------------------------------------------------------------
snmoe$summary()

## -----------------------------------------------------------------------------
snmoe$plot(what = "meancurve")

## -----------------------------------------------------------------------------
snmoe$plot(what = "confregions")

## -----------------------------------------------------------------------------
snmoe$plot(what = "clusters")

## -----------------------------------------------------------------------------
snmoe$plot(what = "loglikelihood")

