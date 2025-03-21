## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(marlod)

## -----------------------------------------------------------------------------
y <- c(0,0,0,3.06,4.41,7.23,8.29,9.52,19.94,20.25) 

## Limit of detection (LOD) = 3
lod <- 3

Fillin(y, lod, "BetaMean")
Fillin(y, lod, "BetaGM")

## -----------------------------------------------------------------------------
data(simdata15)
head(simdata15)

## -----------------------------------------------------------------------------
id=as.matrix(as.vector(t(simdata15$id)))
y=as.matrix(as.vector(t(simdata15$y)))
x1=as.matrix(as.vector(t(simdata15$x1)))
x2=as.matrix(as.vector(t(simdata15$x2)))
x=cbind(x1,x2)

## LOD = 2 is equivalent to detection proportion = 56.3% (censoring proportion = 43.7%).
lod=2

## Intercept is not included in the "x" and "typed".
## Modified.GEE(id, y, x, lod, substitue, corstr, typetd, maxiter)
Modified.GEE(id, y, x, lod, "QQplot", "AR-1", c(1,1), 1000)

## -----------------------------------------------------------------------------
## Gets initial estimates for the QIF approach through independence structure
initial=glm(y ~ x1 + x2, data=simdata15, family=gaussian)
beta_initial=as.matrix(initial$coefficients)

## Intercept is not included in the "x" and "typed".
## Modified.QIF(id, y, x, lod, substitue, corstr, beta, typetd, maxiter)
Modified.QIF(id, y, x, lod, "QQplot", "exchangeable", beta_initial, c(1,1), 1000)

## -----------------------------------------------------------------------------
## Gets initial estimates for the GMM approach through independence structure
initial=glm(y ~ x1 + x2, data=simdata15, family=gaussian)
beta_initial=as.matrix(initial$coefficients)

## Intercept is not included in the "x" and "typed".
## Modified.GMM(id, y, x, lod, substitue, beta, maxiter)
Modified.GMM(id, y, x, lod, "QQplot", beta_initial, 1000)

## -----------------------------------------------------------------------------
data(simdata58)
head(simdata58)

## -----------------------------------------------------------------------------
id=as.matrix(as.vector(t(simdata58$id)))
y=as.matrix(as.vector(t(simdata58$y)))
x1=as.matrix(as.vector(t(simdata58$x1)))

## LOD = 0.05 is equivalent to detection proportion = 50.7% (censoring proportion = 49.3%).
lod=0.05

## Intercept is not included in the "x".
## Selected.GEE(id, y, x, lod, substitue, corstr, maxiter)
Selected.GEE(id, y, x1, lod, "MIWithID", "AR-1", 1000)

## -----------------------------------------------------------------------------
id=as.matrix(as.vector(t(simdata58$id)))
y=as.matrix(as.vector(t(simdata58$y)))
x1=as.matrix(as.vector(t(simdata58$x1)))

Modified.GEE(id, y, x1, lod, "MIWithID", "AR-1", c(3), 1000)

## -----------------------------------------------------------------------------
## Gets initial estimates for the QIF approach through independence structure
initial=glm(y ~ x1, data=simdata58, family=gaussian)
beta_initial=as.matrix(initial$coefficients)

## Intercept is not included in the "x" and "typed".
## Selected.QIF(id, y, x, lod, substitue, corstr, beta, maxiter)
Selected.QIF(id, y, x1, lod, "MIWithID", "AR-1", beta_initial, 1000)

## -----------------------------------------------------------------------------
y=as.matrix(as.vector(t(simdata15$y)))
x1=as.matrix(as.vector(t(simdata15$x1)))
x2=as.matrix(as.vector(t(simdata15$x2)))
x=cbind(matrix(1,length(x1),1),x1,x2)

## LOD = 2 is equivalent to detection proportion = 56.3% (censoring proportion = 43.7%).
lod=2

## Median or 50th quantile is given.
tau=0.5

## Intercept is included in the "x" but not in the "typed".
## Quantile.FWZ(y, x, lod, substitue, tau, corstr, typetd, data)
Quantile.FWZ(y, x, lod, "LOD2", tau, "exchangeable", c(1,1), simdata15)

## -----------------------------------------------------------------------------
y=as.matrix(as.vector(t(simdata58$y)))
x1=as.matrix(as.vector(t(simdata58$x1)))
x=cbind(matrix(1,length(x1),1),x1)

## LOD = 0.05 is equivalent to detection proportion = 50.7% (censoring proportion = 49.3%).
lod=0.05

## 95th quantile is given.
tau=0.95

## Intercept is included in the "x".
## Quantile.select.FWZ(y, x, lod, substitue, tau, data)
Quantile.select.FWZ(y, x, lod, "LOD2", tau, simdata58)

## -----------------------------------------------------------------------------
y=as.matrix(as.vector(t(simdata58$y)))
x1=as.matrix(as.vector(t(simdata58$x1)))
x=cbind(matrix(1,length(x1),1),x1)

## LOD = 0.05 is equivalent to detection proportion = 50.7% (censoring proportion = 49.3%).
lod=0.05

## 95th quantile is given.
tau=0.95

## Intercept is included in the "x" but not in the "typed".
## Quantile.FWZ(y, x, lod, substitue, tau, corstr, typetd, data)
Quantile.FWZ(y, x, lod, "LOD2", tau, "AR-1", c(2), simdata58)

