## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval = FALSE, 'Install package'------------------------------------------
#  install.packages("FSinR")

## ----message=FALSE, 'Load libraries'------------------------------------------
library(caret)
library(FSinR)

data(iris)

## ---- 'Generate Evaluator (S+W)'----------------------------------------------
evaluator <- wrapperEvaluator("knn")

## ---- 'Generate searcher (S+W)'-----------------------------------------------
searcher <- searchAlgorithm('sequentialForwardSelection')

## ---- 'Feature Selection (S+W)'-----------------------------------------------
results <- featureSelection(iris, 'Species', searcher, evaluator)

## ---- 'Results (S+W)'---------------------------------------------------------
results$bestFeatures
results$bestValue

## ----eval=FALSE, 'Generate wrapper (S+W) 2'-----------------------------------
#  resamplingParams <- list(method = "cv", number = 10)
#  fittingParams <- list(preProc = c("center", "scale"), metric="Accuracy", tuneGrid = expand.grid(k = c(1:20)))
#  
#  evaluator <- wrapperEvaluator("knn", resamplingParams, fittingParams)

## ----eval=FALSE, 'Search generator (W+S) 2'-----------------------------------
#  searcher <- searchAlgorithm('tabu', list(tamTabuList = 4, iter = 5, intensification=2, iterIntensification=5, diversification=1, iterDiversification=5, verbose=FALSE) )

## ----eval=FALSE, 'Feature Selection (S+W) 2'----------------------------------
#  results <- featureSelection(iris, 'Species', searcher, evaluator)

## ---- 'Generate Evaluator (S+F)'----------------------------------------------
evaluator <- filterEvaluator('MDLC')

## ---- 'Generate searcher (S+F)'-----------------------------------------------
searcher <- searchAlgorithm('sequentialForwardSelection')

## ---- 'Feature Selection (S+F)'-----------------------------------------------
results <- featureSelection(iris, 'Species', searcher, evaluator)

## ---- 'Results (S+F)'---------------------------------------------------------
results$bestFeatures
results$bestValue

## ---- 'Generate Evaluator (F/W)'----------------------------------------------
filter_evaluator <- filterEvaluator("IEConsistency")

wrapper_evaluator <- wrapperEvaluator("lvq")

## ---- 'Results (F/W)'---------------------------------------------------------
resultFilter <- filter_evaluator(iris, 'Species', c("Sepal.Length", "Sepal.Width",  "Petal.Length", "Petal.Width"))
resultFilter

resultWrapper <- wrapper_evaluator(iris, 'Species', c("Petal.Length", "Petal.Width"))
resultWrapper

## ----'DFS'--------------------------------------------------------------------
library(caret)
library(FSinR)

data(mtcars)


evaluator <- filterEvaluator('determinationCoefficient')

directSearcher <- directSearchAlgorithm('selectKBest', list(k=3))

results <- directFeatureSelection(mtcars, 'mpg', directSearcher, evaluator)
results$bestFeatures
results$featuresSelected
results$valuePerFeature

## ----'HFS'--------------------------------------------------------------------
library(caret)
library(FSinR)

data(mtcars)


evaluator_1 <- filterEvaluator('determinationCoefficient')
evaluator_2 <- filterEvaluator('ReliefFeatureSetMeasure')

hybridSearcher <- hybridSearchAlgorithm('LCC')

results <- hybridFeatureSelection(mtcars, 'mpg', hybridSearcher, evaluator_1, evaluator_2)
results$bestFeatures
results$bestValue

