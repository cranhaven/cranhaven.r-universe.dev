## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- message=FALSE, warning=FALSE, eval=FALSE--------------------------------
#  library(SmCCNet)
#  set.seed(123)
#  data("ExampleData")
#  Y_binary <- ifelse(Y > quantile(Y, 0.5), 1, 0)
#  # single-omics PLS
#  result <- fastAutoSmCCNet(X = list(X1), Y = as.factor(Y_binary),
#                            Kfold = 3,
#                            subSampNum = 100, DataType = c('Gene'),
#                            saving_dir = getwd(), EvalMethod = 'auc',
#                            summarization = 'NetSHy',
#                            CutHeight = 1 - 0.1^10, ncomp_pls = 5)
#  # single-omics CCA
#  result <- fastAutoSmCCNet(X = list(X1), Y = Y, Kfold = 3,
#                            preprocess = FALSE,
#                            subSampNum = 50, DataType = c('Gene'),
#                            saving_dir = getwd(), summarization = 'NetSHy',
#                            CutHeight = 1 - 0.1^10)
#  # multi-omics PLS
#  result <- fastAutoSmCCNet(X = list(X1,X2), Y = as.factor(Y_binary),
#                            Kfold = 3, subSampNum = 50,
#                            DataType = c('Gene', 'miRNA'),
#                            CutHeight = 1 - 0.1^10,
#                            saving_dir = getwd(),
#                            EvalMethod = 'auc',
#                            summarization = 'NetSHy',
#                            BetweenShrinkage = 5,
#                            ncomp_pls = 3)
#  # multi-omics CCA
#  result <- fastAutoSmCCNet(X = list(X1,X2), Y = Y,
#                            K = 3, subSampNum = 50,
#                            DataType = c('Gene', 'miRNA'),
#                            CutHeight = 1 - 0.1^10,
#                            saving_dir = getwd(),
#                            summarization = 'NetSHy',
#                            BetweenShrinkage = 5)

