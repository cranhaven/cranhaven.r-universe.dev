## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = TRUE,
  eval = all(vapply(c('ggplot2', 'kml', 'lcmm'), requireNamespace, FUN.VALUE = TRUE, quietly = TRUE)) # needed to prevent errors for _R_CHECK_DEPENDS_ONLY_=true despite VignetteDepends declaration
)

## ----setup, include = FALSE---------------------------------------------------
library(ggplot2)
library(data.table)
library(magrittr)
library(latrend)
knitr::opts_chunk$set(
  cache = TRUE,
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(latrend)
library(data.table)
options(
  latrend.id = "Traj", 
  latrend.time = "Time",
  latrend.verbose = TRUE
)

## -----------------------------------------------------------------------------
set.seed(1)
casedata <- generateLongData(
  sizes = c(40, 60), 
  data = data.frame(Time = 0:10),
  fixed = Y ~ 1,
  fixedCoefs = 1,
  cluster = ~ Time, 
  clusterCoefs = cbind(c(2, -.1), c(0, .05)),
  random = ~ Time,
  randomScales = cbind(c(.2, .02), c(.2, .02)),
  noiseScales = .05
) %>% 
  as.data.table()

## -----------------------------------------------------------------------------
plotTrajectories(casedata, response = "Y")

## -----------------------------------------------------------------------------
plotClusterTrajectories(casedata, response = "Y", cluster = "Class")

## -----------------------------------------------------------------------------
ggplot(casedata[Time == 0], aes(x = Mu)) +
  geom_density(fill = "gray", adjust = .3)

## ----message=TRUE-------------------------------------------------------------
method <- lcMethodStratify(response = "Y", Y[1] > 1.6)
model <- latrend(method, casedata)

## -----------------------------------------------------------------------------
clusterProportions(model)

## ----message=TRUE-------------------------------------------------------------
stratfun <- function(data) {
  int <- coef(lm(Y ~ Time, data))[1]
  factor(int > 1.7, levels = c(FALSE, TRUE), labels = c("Low", "High"))
}
m2 <- lcMethodStratify(response = "Y", stratify = stratfun, center = mean)
model2 <- latrend(m2, casedata)

clusterProportions(model2)

## ----message=TRUE-------------------------------------------------------------
casedata[, Intercept := coef(lm(Y ~ Time, .SD))[1], by = Traj]

m3 <- lcMethodStratify(
  response = "Y", 
  stratify = Intercept[1] > 1.7, 
  clusterNames = c("Low", "High")
)
model3 <- latrend(m3, casedata)

## -----------------------------------------------------------------------------
repStep <- function(method, data, verbose) {
  dt <- as.data.table(data)
  coefdata <- dt[, lm(Y ~ Time, .SD) %>% coef() %>% as.list(), keyby = Traj]
  coefmat <- subset(coefdata, select = -1) %>% as.matrix()
  rownames(coefmat) <- coefdata$Traj
  coefmat
}

## -----------------------------------------------------------------------------
clusStep <- function(method, data, repMat, envir, verbose) {
  km <- kmeans(repMat, centers = 3)

  lcModelPartition(
    response = method$response, 
    data = data, 
    trajectoryAssignments = km$cluster,
    center = method$center,
    method = method,
    model = km
  )
}

## -----------------------------------------------------------------------------
m.twostep <- lcMethodFeature(
  response = "Y", 
  representationStep = repStep, 
  clusterStep = clusStep
)

## ----message=TRUE-------------------------------------------------------------
model.twostep <- latrend(m.twostep, data = casedata)
summary(model.twostep)

## -----------------------------------------------------------------------------
repStep.gen <- function(method, data, verbose) {
  dt <- as.data.table(data)
  coefdata <- dt[, lm(method$formula, .SD) %>% coef() %>% as.list(), keyby = c(method$id)]
  # exclude the id column
  coefmat <- subset(coefdata, select = -1) %>% as.matrix()
  rownames(coefmat) <- coefdata[[method$id]]
  coefmat
}

clusStep.gen <- function(method, data, repMat, envir, verbose) {
  km <- kmeans(repMat, centers = method$nClusters)

  lcModelPartition(
    response = method$response,
    data = data, 
    trajectoryAssignments = km$cluster,
    center = method$center,
    method = method,
    model = km
  )
}

## -----------------------------------------------------------------------------
m.twostepgen <- lcMethodFeature(
  response = "Y",
  representationStep = repStep.gen, 
  clusterStep = clusStep.gen
)

## ----message=TRUE-------------------------------------------------------------
model.twostepgen <- latrend(m.twostepgen, formula = Y ~ Time, nClusters = 2, casedata)
summary(model.twostepgen)

