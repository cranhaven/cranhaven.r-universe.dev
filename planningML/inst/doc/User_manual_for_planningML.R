## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
## load dataset
pilot.data = readRDS(system.file("extdata", "pilotdata.rds", package = "planningML"))
dim(pilot.data)

## -----------------------------------------------------------------------------
x = pilot.data[,-ncol(pilot.data)]
y = pilot.data$DEPRESSION

## -----------------------------------------------------------------------------
head(x)

## -----------------------------------------------------------------------------
y

## -----------------------------------------------------------------------------
library(planningML)

## ----eval=FALSE, include=TRUE-------------------------------------------------
#  features = featureselection(x = x, y = y)

## ----include=FALSE------------------------------------------------------------
features = readRDS(system.file("extdata", "features.rds", package = "planningML"))

## -----------------------------------------------------------------------------
summary(features)

## ----eval=FALSE, include=TRUE-------------------------------------------------
#  output = samplesize(features=features,
#                      method="HCT", m=c(5,10,length(features$features)), effectsize=NULL,
#                      class.prob = NULL, totalnum_features = NULL, threshold=0.1, metric="MCC")

## ----include=FALSE------------------------------------------------------------
output = readRDS(system.file("extdata", "output.rds", package = "planningML"))

## -----------------------------------------------------------------------------
head(output$outtable)

## -----------------------------------------------------------------------------
summary(output)

## -----------------------------------------------------------------------------
plot(output)

## -----------------------------------------------------------------------------
effect_size = readRDS(system.file("extdata", "effectsize.rds", package = "planningML"))
effect_size

## ----warning=FALSE------------------------------------------------------------
output2 = samplesize(features = NULL,
                      method="HCT", m=200, effectsize=effect_size, class.prob = 0.5, 
                     totalnum_features = 5000, threshold=0.1, metric="MCC")

## -----------------------------------------------------------------------------
summary(output2)

## -----------------------------------------------------------------------------
plot(output2)

## -----------------------------------------------------------------------------
pilotSet = readRDS(system.file("extdata", "pilotSet.rds", package = "planningML"))
pilotY = readRDS(system.file("extdata", "pilotY.rds", package = "planningML"))

## -----------------------------------------------------------------------------
dim(pilotSet)

## -----------------------------------------------------------------------------
table(pilotY)

## ----eval=FALSE, include=TRUE-------------------------------------------------
#  nhamcs_rf_auc <- learningcurve_data(pilotSet, pilotY, method="rf", batchsize = 100, nfold=5, nrepeat=10, class.prob = 0.105, metric="AUC")

## ----include=FALSE------------------------------------------------------------
nhamcs_rf_auc = readRDS(system.file("extdata", "nhamcs_rf_auc.rds", package = "planningML"))

## -----------------------------------------------------------------------------
nhamcs_rf_auc

## ----warning=FALSE------------------------------------------------------------
lc_fit <- fit_learningcurve(nhamcs_rf_auc, testX=seq(10, 1500, 5), target=0.8)

## -----------------------------------------------------------------------------
plot(lc_fit)

