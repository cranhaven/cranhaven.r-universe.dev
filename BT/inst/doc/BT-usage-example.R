## ---- echo=FALSE--------------------------------------------------------------
# write("TMPDIR = 'C:\\Users\\Gireg Willame\\Desktop\\TMP'", file=file.path(Sys.getenv('R_USER'), '.Renviron'))
# knitr::opts_chunk$set(
#    fig.path = "c:/Users/Gireg Willame/Desktop/TMP/Figures"
# )

## -----------------------------------------------------------------------------
library(BT)

## ---- tidy=TRUE---------------------------------------------------------------
db <- BT::BT_Simulated_Data

## ---- tidy=TRUE---------------------------------------------------------------
str(db)
head(db)

## ---- tidy=TRUE---------------------------------------------------------------
summary(db)

## ---- tidy=TRUE---------------------------------------------------------------
sum(db$Y)/sum(db$ExpoR)

## ---- tidy=TRUE---------------------------------------------------------------
set.seed(404)
trainObs <- sample(seq(1, nrow(db)), 0.8*nrow(db))
trainSet <- db[trainObs,]
testSet <- db[setdiff(seq(1, nrow(db)), trainObs),]

sum(trainSet$Y)/sum(trainSet$ExpoR)
sum(testSet$Y)/sum(testSet$ExpoR)

## ---- tidy=TRUE---------------------------------------------------------------
formFreq <- Y_normalized ~ Gender + Age + Split + Sport

## ---- tidy=TRUE---------------------------------------------------------------
bt0 <- BT(formula = formFreq,
          data = trainSet,
          tweedie.power = 1,
          ABT = FALSE,
          n.iter = 50,
          train.fraction = 0.8,
          interaction.depth = 3,
          shrinkage = 0.01,
          bag.fraction = 0.5,
          colsample.bytree = NULL,
          keep.data = TRUE,
          is.verbose = FALSE,
          cv.folds = 1,
          folds.id = NULL,
          n.cores = 1,
          weights = ExpoR,
          seed = 4)

## ---- tidy=TRUE---------------------------------------------------------------
bt0$call
bt0$distribution
bt0$BTParams
bt0$keep.data
bt0$is.verbose
bt0$seed
#bt0$w / bt0$response / bt0$var.name

## ---- tidy=TRUE---------------------------------------------------------------
print(bt0)

## ---- tidy=TRUE---------------------------------------------------------------
str(bt0$BTInit)

## ---- tidy=TRUE---------------------------------------------------------------
str(bt0$BTData)

## ---- tidy=TRUE---------------------------------------------------------------
head(bt0$fitted.values, 5)
str(bt0$BTErrors)

## ---- tidy=TRUE---------------------------------------------------------------
length(bt0$BTIndivFits)
# First tree in the expansion.
bt0$BTIndivFits[[1]]
bt0$BTIndivFits[[1]]$frame

## ---- tidy=TRUE, fig.align='center'-------------------------------------------
perfbt0_OOB <- BT_perf(bt0, method="OOB", oobag.curve = TRUE)
perfbt0_OOB

## ---- tidy=TRUE, fig.align='center'-------------------------------------------
perfbt0_val <- BT_perf(bt0, method="validation")
perfbt0_val

## ---- tidy=TRUE---------------------------------------------------------------
perfbt0_BG <- BT_perf(bt0, plot.it = FALSE)
perfbt0_BG

## ---- tidy=TRUE---------------------------------------------------------------
bt1 <- BT_more(bt0, new.n.iter = 150, seed = 4)
# See parameters and different inputs.
bt1$BTParams$n.iter

## ---- tidy=TRUE---------------------------------------------------------------
perfbt1_OOB <- BT_perf(bt1, method = 'OOB', plot.it = FALSE)
perfbt1_val <- BT_perf(bt1, method = 'validation', plot.it = FALSE)
perfbt1_OOB
perfbt1_val

## ---- tidy=TRUE---------------------------------------------------------------
bt2 <- BT(formula = formFreq, 
          data = trainSet,
          tweedie.power = 1,
          ABT = FALSE,
          n.iter = 200,
          train.fraction = 1,
          interaction.depth = 3,
          shrinkage = 0.01,
          bag.fraction = 0.5,
          colsample.bytree = NULL,
          keep.data = TRUE,
          is.verbose = FALSE,
          cv.folds = 3,
          folds.id = NULL,
          n.cores = 1,
          weights = ExpoR,
          seed = 4)

## ---- tidy=TRUE---------------------------------------------------------------
bt2$cv.folds
str(bt2$folds)
str(bt2$cv.fitted)
str(bt2$BTErrors)

## ---- tidy=TRUE, fig.align='center'-------------------------------------------
perfbt2_cv <- BT_perf(bt2, method = 'cv')

## ---- tidy=TRUE---------------------------------------------------------------
bt3 <- BT(formula = formFreq, 
          data = trainSet,
          tweedie.power = 1,
          ABT = FALSE,
          n.iter = 225,
          train.fraction = 1,
          interaction.depth = 2,
          shrinkage = 0.01,
          bag.fraction = 0.5,
          colsample.bytree = NULL,
          keep.data = TRUE,
          is.verbose = FALSE,
          cv.folds = 3,
          folds.id = NULL,
          n.cores = 1,
          weights = ExpoR,
          seed = 4)

## ---- tidy=TRUE---------------------------------------------------------------
indexMin <- which.min(c(min(bt2$BTErrors$cv.error), min(bt3$BTErrors$cv.error)))
btOpt <- if(indexMin==1) bt2 else bt3
perfbtOpt_cv <- BT_perf(btOpt, method='cv', plot.it=FALSE)

btOpt
perfbtOpt_cv

## ---- tidy=TRUE---------------------------------------------------------------
summary(btOpt, n.iter = perfbtOpt_cv)

## ---- tidy=TRUE---------------------------------------------------------------
head(predict(btOpt, n.iter = c(BT_perf(btOpt, method='OOB', plot.it=FALSE), perfbtOpt_cv), type = 'link'), 10) 
head(predict(btOpt, n.iter = c(BT_perf(btOpt, method='OOB', plot.it=FALSE), perfbtOpt_cv), type = 'response'), 10)

## ---- tidy=TRUE---------------------------------------------------------------
head(predict(btOpt, n.iter = 40, type = 'response', single.iter = TRUE), 10)

## ---- tidy=TRUE---------------------------------------------------------------
nIterVec <- 225
interactionDepthVec <- c(2, 3)
shrinkageVec <- 0.01
bagFractionVec <- 0.5

gridSearch <- expand.grid(n.iter = nIterVec,
                          interaction.depth = interactionDepthVec, 
                          shrinkage = shrinkageVec, 
                          bag.fraction = bagFractionVec)
gridSearch

## ---- tidy=TRUE---------------------------------------------------------------
abtRes_cv <- list()
for (iGrid in seq(1, nrow(gridSearch)))
{
  currABT <- BT(formula = formFreq, 
              data = trainSet,
              tweedie.power = 1,
              ABT = TRUE,
              n.iter = gridSearch[iGrid, "n.iter"],
              train.fraction = 1,
              interaction.depth = gridSearch[iGrid, "interaction.depth"],
              shrinkage = gridSearch[iGrid, "shrinkage"],
              bag.fraction = gridSearch[iGrid, "bag.fraction"],
              colsample.bytree = NULL,
              keep.data = FALSE,
              is.verbose = FALSE,
              cv.folds = 3,
              folds.id = NULL,
              n.cores = 1,
              weights = ExpoR,
              seed = 4)
  
  abtRes_cv[[iGrid]] <- currABT
}

## ---- tidy=TRUE, fig.align='center'-------------------------------------------
perfabt1_cv <- BT_perf(abtRes_cv[[1]], method='cv', plot.it=TRUE)
perfabt2_cv <- BT_perf(abtRes_cv[[2]], method='cv', plot.it=TRUE)

## ---- tidy=TRUE---------------------------------------------------------------
indexMin <- which.min(c(min(abtRes_cv[[1]]$BTErrors$cv.error), min(abtRes_cv[[2]]$BTErrors$cv.error)))
abtOpt <- if (indexMin==1) abtRes_cv[[1]] else abtRes_cv[[2]]
perfabtOpt_cv <- if (indexMin==1) perfabt1_cv else perfabt2_cv

abtOpt
abtOpt$BTParams$interaction.depth
perfabtOpt_cv

## ---- tidy=TRUE---------------------------------------------------------------
table(sapply(seq(1, perfbtOpt_cv), function(xx){nrow(btOpt$BTIndivFits[[xx]]$frame[btOpt$BTIndivFits[[xx]]$frame$var != "<leaf>",])}))
table(sapply(seq(1, perfabtOpt_cv), function(xx){nrow(abtOpt$BTIndivFits[[xx]]$frame[abtOpt$BTIndivFits[[xx]]$frame$var != "<leaf>",])}))

## ---- tidy=TRUE---------------------------------------------------------------
btPredTest <- predict(btOpt, newdata = testSet, n.iter = perfbtOpt_cv, type = "response") * testSet$ExpoR
abtPredTest <- predict(abtOpt, newdata = testSet, n.iter = perfabtOpt_cv, type = "response") * testSet$ExpoR

## ---- tidy=TRUE---------------------------------------------------------------
devPoisson <- function(obs, pred) {
    2 * (sum(dpois(x = obs, lambda = obs, log = TRUE)) - sum(dpois(x = obs, lambda = pred, log = TRUE)))
}

## ---- tidy=TRUE---------------------------------------------------------------
devPoisson(testSet$Y, btPredTest)
devPoisson(testSet$Y, abtPredTest)

