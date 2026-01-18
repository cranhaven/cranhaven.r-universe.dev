## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = TRUE,
  fig.align = "center", 
  fig.width = 6, 
  fig.height = 6)

## ---- message = FALSE, warning = FALSE, print = FALSE-------------------------
library(fPortfolio)
library(BLCOP)
library(mnormt)

## ---- print = FALSE-----------------------------------------------------------
pickMatrix <- matrix(c(1/2, -1, 1/2, rep(0, 3)), nrow = 1, ncol = 6 )
views <- BLViews(P = pickMatrix, q = 0.06,confidences =  100,
assetNames = colnames(monthlyReturns))
views

## ---- print = FALSE-----------------------------------------------------------
priorMeans <- rep(0, 6)
priorVarcov <- MASS::cov.mve(monthlyReturns)$cov

## ---- print = TRUE------------------------------------------------------------
marketPosterior <- posteriorEst(views = views, sigma = priorVarcov,
 mu = priorMeans, tau = 1/2)
marketPosterior

## ---- print = FALSE-----------------------------------------------------------
finViews <- matrix(ncol = 4, nrow = 1, dimnames = list(NULL, c("C","JPM","BAC","MS")))
finViews[,1:4] <- rep(1/4,4)
views <- addBLViews(finViews, 0.15, 90, views)
views

## ---- print = TRUE------------------------------------------------------------
marketPosterior <- BLPosterior(as.matrix(monthlyReturns), views, tau = 1/2,
	marketIndex = as.matrix(sp500Returns),riskFree = as.matrix(US13wTB))
marketPosterior

## ---- print = TRUE, fig = TRUE, fig.height = 3--------------------------------
optPorts <- optimalPortfolios.fPort(marketPosterior, optimizer = "tangencyPortfolio")
optPorts
weightsPie(optPorts$priorOptimPortfolio)
weightsPie(optPorts$posteriorOptimPortfolio)

## ---- echo=TRUE, print=FALSE, fig=FALSE---------------------------------------
optPorts2 <- optimalPortfolios.fPort(marketPosterior,
		constraints = "minW[1:6]=0.1", optimizer = "minriskPortfolio")
optPorts2

## ---- echo=TRUE, print=FALSE, fig=TRUE----------------------------------------
densityPlots(marketPosterior, assetsSel = "JPM")

## ---- echo=TRUE, print=FALSE--------------------------------------------------
dispersion <- c(.376,.253,.360,.333,.360,.600,.397,.396,.578,.775) / 1000
sigma <- BLCOP:::.symmetricMatrix(dispersion, dim = 4)
caps <- rep(1/4, 4)
mu <- 2.5 * sigma %*% caps
dim(mu) <- NULL
marketDistribution <- mvdistribution("mt", mean = mu, S = sigma, df = 5 )
class(marketDistribution)

## ---- echo=TRUE, print=FALSE--------------------------------------------------
pick <- matrix(0, ncol = 4, nrow = 1, dimnames = list(NULL, c("SP", "FTSE", "CAC", "DAX")))
pick[1,"DAX"] <- 1
viewDist <- list(distribution("unif", min = -0.02, max = 0))
views <- COPViews(pick, viewDist = viewDist, confidences = 0.2, assetNames = c("SP", "FTSE", "CAC", "DAX"))

## ---- echo=TRUE, print=FALSE--------------------------------------------------
newPick <- matrix(0, 1, 2)
dimnames(newPick) <- list(NULL, c("SP", "FTSE"))
newPick[1,] <- c(1, -1) # add a relative view
views <- addCOPViews(newPick, list(distribution("norm", mean = 0.05, sd = 0.02)), 0.5, views)

## ---- echo=TRUE, print=FALSE, fig=TRUE----------------------------------------
marketPosterior <- COPPosterior(marketDistribution, views, numSimulations = 50000)
densityPlots(marketPosterior, assetsSel = 4)

