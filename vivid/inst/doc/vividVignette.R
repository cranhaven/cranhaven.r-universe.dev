## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "vig/"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----setup, echo = FALSE, warning=FALSE, message=FALSE------------------------
library(vivid)

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(vivid) 

## ---- messages = FALSE--------------------------------------------------------
set.seed(101)
genFriedman <- function(noFeatures = 10, noSamples = 100, sigma = 1) {
  
  # Create dataframe with named columns
  df <- setNames(as.data.frame(matrix(runif(noSamples * noFeatures, 0, 1), nrow = noSamples), 
                               stringsAsFactors = FALSE), 
                 paste0("x", 1:noFeatures))
  
  # Equation: y = 10sin(πx1x2) + 20(x3−0.5)^2 + 10x4 + 5x5 + ε
  
  df$y <- 10 * sin(pi * df$x1 * df$x2) + 
    20 * (df$x3 - 0.5)^2 + 
    10 * df$x4 + 
    5 * df$x5 + 
    rnorm(noSamples, sd = sigma) # error
  
  return(df)
}

myData <- genFriedman(noFeatures = 9, noSamples = 350, sigma = 1) 

## ---- eval = FALSE------------------------------------------------------------
#  library(randomForest) # for model fit
#  set.seed(1701)
#  rf <- randomForest(y ~ ., data = myData)

## ---- eval = FALSE------------------------------------------------------------
#  set.seed(1701)
#  viviRf  <- vivi(fit = rf,
#                  data = myData,
#                  response = "y",
#                  gridSize = 50,
#                  importanceType = "agnostic",
#                  nmax = 500,
#                  reorder = TRUE,
#                  predictFun = NULL,
#                  numPerm = 4,
#                  showVimpError = FALSE)

## ---- eval = FALSE------------------------------------------------------------
#  viviHeatmap(mat = viviRf)

## ---- echo = F,  out.width = '100%'-------------------------------------------
knitr::include_graphics("https://raw.githubusercontent.com/AlanInglis/vivid/master/vividVigplots/rfheatmap.png")

## ---- eval = FALSE------------------------------------------------------------
#  viviNetwork(mat = viviRf)

## ---- echo = F,  out.width = '100%'-------------------------------------------
knitr::include_graphics("https://raw.githubusercontent.com/AlanInglis/vivid/master/vividVigplots/rfnetwork.png")

## ---- eval = FALSE------------------------------------------------------------
#  viviNetwork(mat = viviRf, intThreshold = 0.12, removeNode = FALSE)
#  viviNetwork(mat = viviRf, intThreshold = 0.12, removeNode = TRUE)

## ---- echo = F,  out.width = '100%'-------------------------------------------
knitr::include_graphics("https://raw.githubusercontent.com/AlanInglis/vivid/master/vividVigplots/rfnet_filter_comb1.png")

## ---- eval = FALSE------------------------------------------------------------
#  viviNetwork(mat = viviRf,
#              layout = cbind(c(1,1,1,1,2,2,2,2,2), c(1,2,4,5,1,2,3,4,5)))

## ---- echo = F,  out.width = '70%', fig.align='center'------------------------
knitr::include_graphics("https://raw.githubusercontent.com/AlanInglis/vivid/master/vividVigplots/rfnet_custom_layout.png")

## ---- eval = FALSE------------------------------------------------------------
#  set.seed(1701)
#  # clustered and filtered network for rf
#  intVals <- viviRf
#  diag(intVals) <- NA
#  
#  
#  # select VIVI values in top 20%
#  impTresh <- quantile(diag(viviRf),.8)
#  intThresh <- quantile(intVals,.8,na.rm=TRUE)
#  sv <- which(diag(viviRf) > impTresh |
#                apply(intVals, 1, max, na.rm=TRUE) > intThresh)
#  
#  h <- hclust(-as.dist(viviRf[sv,sv]), method="single")
#  
#  viviNetwork(viviRf[sv,sv],
#              cluster = cutree(h, k = 3), # specify number of groups
#              layout = igraph::layout_as_star)

## ---- echo = F,  out.width = '80%', fig.align='center'------------------------
knitr::include_graphics("https://raw.githubusercontent.com/AlanInglis/vivid/master/vividVigplots/rfnet_cluster.png")

## ---- eval = FALSE------------------------------------------------------------
#  top5 <- colnames(viviRf)[1:5]
#  pdpVars(data = myData,
#          fit = rf,
#          response = 'y',
#          vars = top5,
#          nIce = 100)

## ---- echo = F,  out.width = '100%', fig.align='center'-----------------------
knitr::include_graphics("https://raw.githubusercontent.com/AlanInglis/vivid/master/vividVigplots/rfpdp.png")

## ---- eval = FALSE------------------------------------------------------------
#  set.seed(1701)
#  pdpPairs(data = myData,
#           fit =  rf,
#           response = "y",
#           nmax = 500,
#           gridSize = 10,
#           vars = c("x1", "x2", "x3", "x4", "x5"),
#           nIce = 100)

## ---- echo = F,  out.width = '80%', fig.align='center'------------------------
knitr::include_graphics("https://raw.githubusercontent.com/AlanInglis/vivid/master/vividVigplots/rfGPGP_subset.png")

## ---- eval = FALSE------------------------------------------------------------
#  set.seed(1701)
#  pdpZen(data = myData, fit = rf, response = "y", nmax = 500, gridSize = 10)

## ---- echo = F,  out.width = '60%', fig.align='center'------------------------
knitr::include_graphics("https://raw.githubusercontent.com/AlanInglis/vivid/master/vividVigplots/rfzen_all.png")

## ---- eval = FALSE------------------------------------------------------------
#  set.seed(1701)
#  pdpZen(data = myData,
#         fit = rf,
#         response = "y",
#         nmax = 500,
#         gridSize = 10,
#         zpath = c("x1", "x2", "x3", "x4", "x5"))

## ---- echo = F,  out.width = '70%', fig.align='center'------------------------
knitr::include_graphics("https://raw.githubusercontent.com/AlanInglis/vivid/master/vividVigplots/rfzen_subset.png")

## ---- eval = FALSE------------------------------------------------------------
#  # set zpaths with different parameters
#  intVals <- viviRf
#  diag(intVals) <- NA
#  intThresh <- quantile(intVals, .90, na.rm=TRUE)
#  zpSw <- zPath(viv = viviRf, cutoff = intThresh, connect = FALSE, method = 'strictly.weighted')
#  
#  
#  
#  set.seed(1701)
#  pdpZen(data = myData,
#         fit = rf,
#         response = "y",
#         nmax = 500,
#         gridSize = 10,
#         zpath = zpSw)

## ---- echo = F,  out.width = '70%', fig.align='center'------------------------
knitr::include_graphics("https://raw.githubusercontent.com/AlanInglis/vivid/master/vividVigplots/rfzen_SW.png")

## ---- eval = FALSE------------------------------------------------------------
#  library("xgboost")
#  gbst <- xgboost(data = as.matrix(myData[,1:9]),
#                  label =  as.matrix(myData[,10]),
#                  nrounds = 100,
#                  verbose = 0)

## ---- eval = FALSE------------------------------------------------------------
#  # predict function for GBM
#  pFun <- function(fit, data, ...) predict(fit, as.matrix(data[,1:9]))
#  
#  set.seed(1701)
#  viviGBst <- vivi(fit = gbst,
#                   data = myData,
#                   response = "y",
#                   reorder = FALSE,
#                   normalized = FALSE,
#                   predictFun = pFun,
#                   gridSize = 50,
#                   nmax = 500)

## ---- eval = FALSE------------------------------------------------------------
#  viviHeatmap(mat = viviGBst)

## ---- echo = F,  out.width = '100%', fig.align='center'-----------------------
knitr::include_graphics("https://raw.githubusercontent.com/AlanInglis/vivid/master/vividVigplots/gbmheat.png")

## ---- eval = FALSE------------------------------------------------------------
#  set.seed(1701)
#  rfEmbedded <- randomForest(y ~ ., data = myData, importance = TRUE)

## ---- eval = FALSE------------------------------------------------------------
#  viviRfEmbedded <- vivi(fit = rfEmbedded,
#                         data = myData,
#                         response = "y",
#                         importanceType = "%IncMSE")

## ---- eval = FALSE------------------------------------------------------------
#  library("ranger")
#  rang <- ranger(y~., data = myData, importance = 'impurity')

## ---- eval = FALSE------------------------------------------------------------
#  viviRangEmbedded <- vivi(fit = rang,
#                           data = myData,
#                           response = "y",
#                           importanceType = "impurity")

## ---- eval = FALSE------------------------------------------------------------
#  set.seed(1701)
#  rfClassif <- ranger(Species~ ., data = iris, probability = T,
#                      importance = "impurity")
#  
#  set.seed(101)
#  viviClassif  <- vivi(fit = rfClassif,
#                       data = iris,
#                       response = "Species",
#                       gridSize = 10,
#                       nmax = 50,
#                       reorder = TRUE,
#                       class = "setosa")

## ---- eval = FALSE------------------------------------------------------------
#  viviHeatmap(mat = viviClassif)
#  viviNetwork(mat = viviClassif)

## ---- echo = F,  out.width = '100%', fig.align='center'-----------------------
knitr::include_graphics("https://raw.githubusercontent.com/AlanInglis/vivid/master/vividVigplots/classifVIVI.png")

## ---- eval = FALSE------------------------------------------------------------
#  set.seed(1701)
#  pdpPairs(data = iris,
#           fit = rfClassif,
#           response = "Species",
#           class = "setosa",
#           convexHull = T,
#           gridSize = 10,
#           nmax = 50)
#  

## ---- echo = F,  out.width = '80%', fig.align='center'------------------------
knitr::include_graphics("https://raw.githubusercontent.com/AlanInglis/vivid/master/vividVigplots/classifGPDP.png")

## ---- eval = FALSE------------------------------------------------------------
#  
#  # Load libraries
#  library("vivid")
#  library("randomForest")
#  
#  # Create data based on the Friedman equation
#  set.seed(101)
#  genFriedman <- function(noFeatures = 10, noSamples = 100, sigma = 1) {
#  
#    # Create dataframe with named columns
#    df <- setNames(as.data.frame(matrix(runif(noSamples * noFeatures, 0, 1), nrow = noSamples),
#                                 stringsAsFactors = FALSE),
#                   paste0("x", 1:noFeatures))
#  
#    # Equation: y = 10sin(πx1x2) + 20(x3−0.5)^2 + 10x4 + 5x5 + ε
#  
#    df$y <- 10 * sin(pi * df$x1 * df$x2) +
#      20 * (df$x3 - 0.5)^2 +
#      10 * df$x4 +
#      5 * df$x5 +
#      rnorm(noSamples, sd = sigma) # error
#  
#    return(df)
#  }
#  
#  myData <- genFriedman(noFeatures = 9, noSamples = 350, sigma = 1)
#  
#  -------------------------------------------------------------------
#  
#  # Fit random forest using randomForest package
#  set.seed(1701)
#  rf <- randomForest(y ~ ., data = myData)
#  
#  -------------------------------------------------------------------
#  
#  # Run vivid
#  set.seed(1701)
#  viviRf  <- vivi(fit = rf,
#                  data = myData,
#                  response = "y",
#                  gridSize = 50,
#                  importanceType = "agnostic",
#                  nmax = 500,
#                  reorder = TRUE,
#                  predictFun = NULL,
#                  numPerm = 4,
#                  showVimpError = FALSE)
#  
#  -------------------------------------------------------------------
#  
#  # Visualisations:
#  
#  # 1.0: Heatmap
#  viviHeatmap(mat = viviRf)
#  
#  # 1.1: Heatmap with custom colour palettes and a small border around the diagonal importance values
#  viviHeatmap(mat = viviRf,
#              intPal = colorspace::sequential_hcl(palette = "Oslo", n = 100),
#              impPal = rev(colorspace::sequential_hcl(palette = "Reds 3", n = 100)),
#              border = T)
#  
#  # 2.0: Network
#  viviNetwork(mat = viviRf)
#  
#  # 2.1: Network with interactions below 0.12 are filtered.
#  # By default, unconnected nodes are displayed, however, they can be removed by setting `removeNode = T`
#  viviNetwork(mat = viviRf, intThreshold = 0.12, removeNode = FALSE)
#  viviNetwork(mat = viviRf, intThreshold = 0.12, removeNode = TRUE)
#  
#  # 2.3: Network clustered and with interactions thresholded
#  set.seed(1701)
#  # clustered and filtered network for rf
#  intVals <- viviRf
#  diag(intVals) <- NA
#  
#  
#  # select VIVI values in top 20%
#  impTresh <- quantile(diag(viviRf),.8)
#  intThresh <- quantile(intVals,.8,na.rm=TRUE)
#  sv <- which(diag(viviRf) > impTresh |
#                apply(intVals, 1, max, na.rm=TRUE) > intThresh)
#  
#  h <- hclust(-as.dist(viviRf[sv,sv]), method="single")
#  
#  # plot
#  viviNetwork(viviRf[sv,sv],
#              cluster = cutree(h, k = 3), # specify number of groups
#              layout = igraph::layout_as_star)
#  
#  # 3.0: PDP of the top 5 variables extracted from the vivi matrix and number of ICe curves set to 100
#  top5 <- colnames(viviRf)[1:5]
#  pdpVars(data = myData,
#          fit = rf,
#          response = 'y',
#          vars = top5,
#          nIce = 100)
#  
#  # 4.0: GPDP of the variables x1 to x5, with 100 ICE curves shown.
#  set.seed(1701)
#  pdpPairs(data = myData,
#           fit =  rf,
#           response = "y",
#           nmax = 500,
#           gridSize = 10,
#           vars = c("x1", "x2", "x3", "x4", "x5"),
#           nIce = 100)
#  
#  # 5.0: ZPDP of all variables
#  set.seed(1701)
#  pdpZen(data = myData, fit = rf, response = "y", nmax = 500, gridSize = 10)
#  
#  # 5.1: ZPDP where the `zpath` argument specifies the variables to be plotted. In this case, x1 to x5.
#  set.seed(1701)
#  pdpZen(data = myData,
#         fit = rf,
#         response = "y",
#         nmax = 500,
#         gridSize = 10,
#         zpath = c("x1", "x2", "x3", "x4", "x5"))
#  
#  # 5.2: ZPDP with the zpaths set with different parameters using the `zPath` function.
#  intVals <- viviRf
#  diag(intVals) <- NA
#  intThresh <- quantile(intVals, .90, na.rm=TRUE)
#  zpSw <- zPath(viv = viviRf, cutoff = intThresh, connect = FALSE, method = 'strictly.weighted')
#  
#  set.seed(1701)
#  pdpZen(data = myData,
#         fit = rf,
#         response = "y",
#         nmax = 500,
#         gridSize = 10,
#         zpath = zpSw)

## ---- eval = FALSE------------------------------------------------------------
#  library("vivid")
#  library("xgboost")
#  gbst <- xgboost(data = as.matrix(myData[,1:9]),
#                  label =  as.matrix(myData[,10]),
#                  nrounds = 100,
#                  verbose = 0)
#  
#  # predict function for GBM
#  pFun <- function(fit, data, ...) predict(fit, as.matrix(data[,1:9]))
#  
#  # run vivid
#  set.seed(1701)
#  viviGBst <- vivi(fit = gbst,
#                   data = myData,
#                   response = "y",
#                   reorder = FALSE,
#                   normalized = FALSE,
#                   predictFun = pFun,
#                   gridSize = 50,
#                   nmax = 500)
#  
#  # plot heatmap
#  viviHeatmap(mat = viviGBst)
#  

## ---- eval = FALSE------------------------------------------------------------
#  library("vivid")
#  library("randomForest")
#  library("ranger")
#  
#  # randomForest
#  # Note importance must be set to TRUE to use embedded importance scores.
#  set.seed(1701)
#  rfEmbedded <- randomForest(y ~ ., data = myData, importance = TRUE)
#  
#  # Using % increase in MSE as the importance metric in vivid
#  viviRfEmbedded <- vivi(fit = rfEmbedded,
#                         data = myData,
#                         response = "y",
#                         importanceType = "%IncMSE")
#  
#  # Plot Heatmap
#  viviHeatmap(mat = viviRfEmbedded)
#  
#  # ranger
#  # Note the importance metric is selected directly in ranger
#  rang <- ranger(y~., data = myData, importance = 'impurity')
#  
#  # Run vivid
#  viviRangEmbedded <- vivi(fit = rang,
#                           data = myData,
#                           response = "y",
#                           importanceType = "impurity")
#  
#  # Plot Heatmap
#  viviHeatmap(mat = viviRangEmbedded)

## ---- eval = FALSE------------------------------------------------------------
#  library("vivid")
#  library("randomForest")
#  
#  set.seed(1701)
#  rfClassif <- ranger(Species~ ., data = iris, probability = T,
#                      importance = "impurity")
#  
#  set.seed(101)
#  viviClassif  <- vivi(fit = rfClassif,
#                       data = iris,
#                       response = "Species",
#                       gridSize = 10,
#                       nmax = 50,
#                       reorder = TRUE,
#                       class = "setosa")
#  
#  viviHeatmap(mat = viviClassif)
#  
#  set.seed(1701)
#  pdpPairs(data = iris,
#           fit = rfClassif,
#           response = "Species",
#           class = "setosa",
#           convexHull = T,
#           gridSize = 10,
#           nmax = 50)

