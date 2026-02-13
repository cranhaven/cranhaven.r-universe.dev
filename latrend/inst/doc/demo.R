## ----setup, include = FALSE---------------------------------------------------
set.seed(1)
knitr::opts_chunk$set(
  cache = TRUE,
  collapse = TRUE,
  fig.width = 7,
  fig.align = "center",
  fig.topcaption = TRUE,
  comment = "#>",
  eval = all(vapply(c('ggplot2', 'kml', 'lme4', 'mclustcomp'), requireNamespace, FUN.VALUE = TRUE, quietly = TRUE)) # needed to prevent errors for _R_CHECK_DEPENDS_ONLY_=true despite VignetteDepends declaration
)

## ----results='hide',message=FALSE,warning=FALSE-------------------------------
library(latrend)
library(ggplot2)

## -----------------------------------------------------------------------------
data(latrendData)
head(latrendData)

## -----------------------------------------------------------------------------
options(latrend.id = "Id", latrend.time = "Time")

## ----fig.asp = .6, fig.cap='Visualizing the trajectories of the `latrend` dataset.'----
plotTrajectories(latrendData, response = "Y")

## -----------------------------------------------------------------------------
kmlMethod <- lcMethodKML(response = "Y", nClusters = 2, nbRedrawing = 1)

kmlMethod

## -----------------------------------------------------------------------------
kmlModel <- latrend(kmlMethod, data = latrendData)

## -----------------------------------------------------------------------------
kmlModel

## -----------------------------------------------------------------------------
kmlMethods <- lcMethods(kmlMethod, nClusters = 1:7)

as.data.frame(kmlMethods)

## -----------------------------------------------------------------------------
kmlModels <- latrendBatch(kmlMethods, data = latrendData, verbose = FALSE)

kmlModels

## ----warning=FALSE, fig.cap = 'Elbow plots of three relevant cluster metrics across the fitted models.'----
plotMetric(kmlModels, c("logLik", "BIC", "WMAE"))

## -----------------------------------------------------------------------------
kmlModel4 <- subset(kmlModels, nClusters == 4, drop = TRUE)

kmlModel4

## ----fig.cap = 'Cluster trajectories for KML model with 4 clusters.'----------
plotClusterTrajectories(kmlModel4)

## ----fig.asp = .8, fig.cap = 'Cluster trajectories for KML model with 4 clusters, along with the assigned trajectories.'----
plot(kmlModel4)

## -----------------------------------------------------------------------------
trajectoryAssignments(kmlModel4)

## -----------------------------------------------------------------------------
# make sure to change the Id column name for your respective id column name
subjectClusters = data.frame(Id = ids(kmlModel4), Cluster = trajectoryAssignments(kmlModel4))
head(subjectClusters)

posthocAnalysisData = merge(latrendData, subjectClusters, by = 'Id')
head(posthocAnalysisData)
aggregate(Y ~ Cluster, posthocAnalysisData, mean)

## -----------------------------------------------------------------------------
getInternalMetricNames()

## -----------------------------------------------------------------------------
metric(kmlModel, c("APPA.mean", "WRSS", "WMAE"))

## ----fig.width = 5, fig.cap = 'QQ-plot of the selected KML model.'------------
qqPlot(kmlModel4)

## ----fig.asp = .8, fig.cap = 'Cluster-specific detrended QQ-plot for the selected KML model.'----
qqPlot(kmlModel4, byCluster = TRUE, detrend = TRUE)

## -----------------------------------------------------------------------------
lmkmMethod <- lcMethodLMKM(formula = Y ~ Time)

lmkmMethod

## ----echo=TRUE, results='hide'------------------------------------------------
lmkmMethods <- lcMethods(lmkmMethod, nClusters = 1:5)

lmkmModels <- latrendBatch(lmkmMethods, data = latrendData, verbose = FALSE)

## ----warning=FALSE, fig.cap = 'Three cluster metrics for each of the GBTMs.'----
plotMetric(lmkmModels, c("logLik", "BIC", "WMAE"))

## -----------------------------------------------------------------------------
bestLmkmModel <- subset(lmkmModels, nClusters == 3, drop=TRUE)
plot(bestLmkmModel)

## ----fig.width = 5, fig.cap = 'Non-parametric estimates of the cluster trajectories based on the reference assignments.'----
plotClusterTrajectories(latrendData, response = "Y", cluster = "Class")

## -----------------------------------------------------------------------------
refTrajAssigns <- aggregate(Class ~ Id, data = latrendData, FUN = data.table::first)
refModel <- lcModelPartition(data = latrendData, response = "Y", trajectoryAssignments = refTrajAssigns$Class)
refModel

## ----fig.cap = 'Cluster trajectories of the reference model.'-----------------
plot(refModel)

## -----------------------------------------------------------------------------
getExternalMetricNames()

## -----------------------------------------------------------------------------
externalMetric(bestLmkmModel, refModel, "adjustedRand")

