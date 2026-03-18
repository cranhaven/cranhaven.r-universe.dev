## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, eval = FALSE------------------------------------------------------
#  install.packages("mc.heterogeneity")

## ---- echo=FALSE--------------------------------------------------------------
library("mc.heterogeneity")

## -----------------------------------------------------------------------------
selfconcept <- mc.heterogeneity:::selfconcept

## -----------------------------------------------------------------------------
# n1 and n2 are lists of samples sizes in two groups
n1 <- selfconcept$n1
n2 <- selfconcept$n2
# g is a list of effect sizes
g <- selfconcept$g

## -----------------------------------------------------------------------------
cm <- (1-3/(4*(n1+n2-2)-1)) #correct factor to compensate for small sample bias (Hedges, 1981)
d <- cm*g

## ---- eval=FALSE, results = 'hide'--------------------------------------------
#  mc.run <- mc.d(n1, n2, est = d, model = 'random', p_cut = 0.05)

## ---- eval=FALSE, results = 'hide'--------------------------------------------
#  mc.run2 <- mc.d(n1, n2, est = g, model = 'random', adjust = TRUE, p_cut = 0.05)

## ---- eval=FALSE--------------------------------------------------------------
#  mc.run
#  #>              stat  p_value Heterogeneity
#  #> Qtest   23.391659 0.136929           n.s
#  #> mc.ML    1.610239 0.051200           n.s
#  #> mc.REML  2.037578 0.053100           n.s

## ---- eval=FALSE--------------------------------------------------------------
#  mc.run2
#  #>              stat  p_value Heterogeneity
#  #> Qtest   23.391659 0.136929           n.s
#  #> mc.ML    1.610239 0.051200           n.s
#  #> mc.REML  2.037578 0.053100           n.s

## -----------------------------------------------------------------------------
hypo_moder <- mc.heterogeneity:::hypo_moder

## -----------------------------------------------------------------------------
head(hypo_moder)

## ---- eval=FALSE, results = 'hide'--------------------------------------------
#  mc.run3 <- mc.d(n1 = hypo_moder$n1,
#                  n2 = hypo_moder$n2,
#                  est = hypo_moder$d,
#                  model = 'mixed',
#                  mods = cbind(hypo_moder$cov.z1, hypo_moder$cov.z2, hypo_moder$cov.z3),
#                  p_cut = 0.05)

## ---- eval=FALSE--------------------------------------------------------------
#  mc.run3
#  #>              stat    p_value    Heterogeneity
#  #> Qtest   31.849952 0.0008061727 		       sig
#  #> mc.ML    5.187700 0.0004000000        	   sig
#  #> mc.REML  9.283428 0.0004000000        	   sig

## -----------------------------------------------------------------------------
sensation <- mc.heterogeneity:::sensation

## -----------------------------------------------------------------------------
# n is a list of samples sizes
n <- sensation$n
# Pearson's correlation
r <- sensation$r
# Fisher's Transformation
z <- 1/2*log((1+r)/(1-r))

## ---- eval=FALSE, results = 'hide'--------------------------------------------
#  mc.run <- mc.fcor(n, z, model = 'random', p_cut = 0.05)

## ---- eval=FALSE--------------------------------------------------------------
#  mc.run
#  #>              stat    p_value Heterogeneity
#  #> Qtest   29.060970 0.00385868           sig
#  #> mc.ML    5.204299 0.00420000           sig
#  #> mc.REML  6.133111 0.00400000           sig

## -----------------------------------------------------------------------------
library(HSAUR3)
data(smoking)

## -----------------------------------------------------------------------------
# Y1: receive treatment; Y2: stop smoking
n_00 <- smoking$tc - smoking$qc  # not receive treatement yet not stop smoking
n_01 <- smoking$qc # not receive treatement but stop smoking
n_10 <- smoking$tt - smoking$qt # receive treatement but not stop smoking
n_11 <- smoking$qt # receive treatement and stop smoking

## -----------------------------------------------------------------------------
lnOR <- log(n_11*n_00/n_01/n_10)
lnOR

## ---- eval=FALSE, results = 'hide'--------------------------------------------
#  mc.run <- mc.lnOR(n_00, n_01, n_10, n_11, model = 'random', p_cut = 0.05)

## ---- eval=FALSE--------------------------------------------------------------
#  mc.run
#  #>              stat    p_value Heterogeneity
#  #> Qtest   34.873957 0.09050857           n.s
#  #> mc.ML    2.557171 0.02160000           sig
#  #> mc.REML  3.071329 0.02240000           sig

