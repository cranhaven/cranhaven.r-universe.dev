## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----flowchart, fig.cap = "SmCCNet workflow overview for Quantitative Phenotype. X1, X2, and X3 are three omics data types for the same set of n subjects. Y indicates a quantitative phenotype measure for those n subjects. Note that the flowchart demonstrate workflow for three omics data, it is also compatible with more than three omics data or two omics data.", echo = FALSE,out.width='100%'----
knitr::include_graphics("figures/SmCCNet-Quant.jpg")

## ---- echo = FALSE, results = "hide", warning = FALSE, eval = TRUE------------
suppressPackageStartupMessages({
    library(pbapply)
    library(Matrix)
    library(igraph)
    library(furrr)
    library(future)
})

## ---- eval = TRUE-------------------------------------------------------------
library(pbapply)
library(Matrix)
library(igraph)
library(SmCCNet)
library(furrr)
library(future)

## ----example data-------------------------------------------------------------
data(ExampleData)
head(X1[ , 1:6])
head(X2[ , 1:6])
head(Y)

## ----p1p2, eval = TRUE--------------------------------------------------------
p1 <- ncol(X1)
p2 <- ncol(X2)
n <- nrow(X1)
AbarLabel <- c(colnames(cbind(X1, X2)))

## ---- eval = FALSE------------------------------------------------------------
#  # define data list
#  X <- list(X1, X2)
#  # preprocess data
#  processed_data <- lapply(X, function(Xmatrix){
#    as.matrix(dataPreprocess(X = as.data.frame(Xmatrix), covariates = NULL,
#                    is_cv = TRUE, cv_quantile = 0.2, center = TRUE,
#                   scale = TRUE))})
#  # re-standardize -omics data if regress-out approach is used (covariates != NULL)
#  processed_data <- lapply(processed_data, scale)
#  # if preprocess feature is used, X need to be overrided with the following code
#  X <- processed_data

## ----CVflow, fig.cap = "SmCCNet K-fold CV. The best penalty pairs are chosen based on the smallest total prediction error.", echo = FALSE, out.width='100%'----
knitr::include_graphics("figures/SmCCNetCV.png")

## ----CVpara, warning = FALSE, eval = FALSE------------------------------------
#  # number of folds in K-fold CV.
#  K <- 3
#  N <- nrow(X1)
#  # create a list of omics data **
#  X <- list(X1, X2)
#  # number of omics dataset **
#  num_omics <- 2
#  # tuning parameter candidate length for each omics data
#  tuneLength <- 5
#  # tuning parameter candadate range for each omics data
#  minTune <- 0.1
#  maxTune <- 0.5
#  # create empty matrix to store all possible penalty parameters
#  penSelect <- matrix(0, nrow = tuneLength, ncol = num_omics)
#  # create sparsity penalty options.
#  for (Idx in 1:ncol(penSelect))
#  {
#    penSelect[,Idx] <- seq(from = minTune,
#                           to = maxTune,
#                           length.out = tuneLength)
#  }
#  # expand grid
#  # convert matrix to list of columns
#  list_cols <- as.list(as.data.frame(penSelect))
#  # generate all possible combinations
#  PenExpand <- do.call(expand.grid, list_cols)
#  
#  # set a CV directory.
#  CVDir <- "Example3foldCV/"
#  dir.create(CVDir)

## ----make K-fold quantitative, eval = FALSE-----------------------------------
#  set.seed(12345) # set random seed.
#  
#  # split data into folds
#  foldIdx <- suppressWarnings(split(1:nrow(X[[1]]), sample(1:nrow(X[[1]]), K)))
#  folddata <- purrr::map(1:length(foldIdx), function(x){
#      Y <- as.matrix(Y)
#      X_train <- list()
#      X_test <- list()
#      Y_train <- list()
#      Y_test <- list()
#      for (i in 1:length(X))
#      {
#        X_train[[i]] <- scale(X[[i]][-foldIdx[[x]],])
#        X_test[[i]] <- scale(X[[i]][foldIdx[[x]],])
#      }
#      Y_train <- scale(Y[-foldIdx[[x]],])
#      Y_test <- scale(Y[foldIdx[[x]],])
#      return(list(X_train = X_train, X_test = X_test,Y_train = Y_train,
#                  Y_test = Y_test))
#  })
#  # name each fold of data
#  names(folddata) <- paste0('Fold_', 1:K)
#  # saving all preliminary data into local directory for reproducibility purpose
#  save(folddata, PenExpand,
#         file = paste0(CVDir, "CVData.RData"))

## ----set scaling factors, eval = FALSE----------------------------------------
#  # default
#  scalingFactor <- rep(1,ncol(combn(num_omics + 1,2)))
#  # interactive **
#  scalingFactor <- scalingFactorInput(DataType = c('mRNA', 'miRNA', 'phenotype'))

## ----run CV, eval = FALSE-----------------------------------------------------
#  # load cross-validation data
#  load(paste0(CVDir, "CVData.RData"))
#  # create an empty list for storing CV result for each fold
#  CVResult <- list()
#  for (CVidx in 1:K)
#  {
#      # set scaling factor
#      CCcoef <- scalingFactor
#      # create empty vector for storing cross-validation result
#      RhoTrain <- RhoTest <- DeltaCor <- rep(0, nrow(PenExpand))
#      for(idx in 1:nrow(PenExpand))
#      {
#        # consider one pair of sparsity penalties at a time.
#        l <- PenExpand[idx, ]
#        # run SmCCA on the subsamples (Figure 1, Step II)
#        Ws <- getCanWeightsMulti(folddata[[CVidx]][["X_train"]],
#                             Trait = as.matrix(folddata[[CVidx]][["Y_train"]]),
#                             Lambda = as.numeric(l), NoTrait = FALSE,
#                             CCcoef = CCcoef)
#        # evaluate the canonical correlation for training and testing data
#        rho.train <-  getCanCorMulti(X = folddata[[CVidx]][["X_train"]],
#                                  Y = as.matrix(folddata[[CVidx]][["Y_train"]]),
#                                  CCWeight = Ws,
#                                  CCcoef = CCcoef)
#        rho.test <-  getCanCorMulti(X = folddata[[CVidx]][["X_test"]],
#                                  Y = as.matrix(folddata[[CVidx]][["Y_test"]]),
#                                  CCWeight = Ws,
#                                  CCcoef = CCcoef)
#  
#        # store cv result
#        RhoTrain[idx] <- round(rho.train, digits = 5)
#        RhoTest[idx] <- round(rho.test, digits = 5)
#        DeltaCor[idx] <- abs(rho.train - rho.test)
#  
#      }
#  
#      # record prediction errors for given CV fold and all sparsity penalty
#      # options.
#      CVResult[[CVidx]] <- cbind(RhoTrain, RhoTest, DeltaCor)
#  }

## ----aggregate error, eval = FALSE--------------------------------------------
#  # aggregate CV result and select the best penalty term
#  AggregatedCVResult <- Reduce("+", CVResult) / length(CVResult)
#  # calculate the evaluation metric of interest
#  EvalMetric <- apply(AggregatedCVResult, 1, function(x) {x[3]/abs(x[2])})
#  # determine the best CV result
#  optIdx <- which.min(EvalMetric)

## ----contour, eval = FALSE----------------------------------------------------
#  library(plotly)
#  library(reshape2)
#  f1 <- list(
#    family = "Arial, sans-serif",
#    size = 20,
#    color = "black"
#  )
#  f2 <- list(
#    family = "Old Standard TT, serif",
#    size = 20,
#    color = "black"
#  )
#  a <- list(
#    title = "l1",
#    titlefont = f1,
#    showticklabels = TRUE,
#    tickfont = f2
#  )
#  b <- list(
#    title = "l2",
#    titlefont = f1,
#    showticklabels = TRUE,
#    tickfont = f2
#  )
#  # create melt data
#  PenExpandMelt <- cbind(PenExpand[,c(1,2)], EvalMetric)
#  colnames(PenExpandMelt)[c(1,2)] <- c('l1', 'l2')
#  hmelt <- melt(PenExpandMelt, id.vars = c("l1", "l2"))
#  contourPlot <- plot_ly(hmelt, x = ~l1, y = ~l2, z = ~value,
#                         type = "contour") %>%
#    layout(xaxis = a, yaxis = b, showlegend = TRUE, legend = f1)
#  # orca preinstalltion is required for next step:
#  # https://github.com/plotly/orca#installation
#  contourPlot

## ----contourPlot, fig.cap = "Total scaled prediction error contour plot for evaluation metric (defined as prediction error/test cc). The x- and y-axes indicate LASSO penalties considered for mRNA and miRNA, respectively. Blue to yellow scale indicates increasing total scaled prediction error.", echo = FALSE, out.width = '100%'----
knitr::include_graphics("figures/TotalPredictionError.jpeg")

## ----best pen, eval = FALSE---------------------------------------------------
#  # combine CV evaluation result with penalty candidates
#  overallCVInfo <- cbind(PenExpand, AggregatedCVResult, scaledPredError = EvalMetric)
#  # set column names for penalty terms
#  colnames(overallCVInfo)[1:num_omics] <- paste0('l',1:num_omics)
#  # save overall CV result
#  write.csv(overallCVInfo, file = paste0(CVDir, 'overallCVInfo.csv'),
#            row.names = FALSE)
#  # print out the best CV penalty pair and associated result
#  print(overallCVInfo[optIdx,])

## ----errorTable, echo = FALSE, eval = TRUE------------------------------------
knitr::kable(read.csv("../vignettes/results/overallCVInfo.csv"), caption = "Total Prediction Error from a 3-fold CV for the synthetic dataset")

## ----get abar, eval = FALSE---------------------------------------------------
#  # feature sampling proportions, 0.9 for miRNA since it has less features. **
#  s <- c(0.7,0.9)
#  # number of subsamples.
#  SubsamplingNum <- 50
#  # run SmCCA on the subsamples (Figure 1, Step II)
#  Ws <- getRobustWeightsMulti(X,
#                          Trait = as.matrix(Y),
#                          NoTrait = FALSE,CCcoef = scalingFactor,
#                          Lambda = as.numeric(overallCVInfo[optIdx,1:num_omics]),
#                          s = s,
#                          SubsamplingNum = SubsamplingNum)

## ----get modules, eval = FALSE------------------------------------------------
#  # construct adjacency matrix
#  Abar <- getAbar(Ws, FeatureLabel = AbarLabel)
#  # perform clustering based on the adjacency matrix Abar
#  OmicsModule <- getOmicsModules(Abar, PlotTree = FALSE)
#  save(Ws, Abar, OmicsModule, file = paste0(CVDir, "SmCCNetWeights.RData"))

## ----plotNet, eval = FALSE----------------------------------------------------
#  # make sure there are no duplicated labels
#  AbarLabel <- make.unique(AbarLabel)
#  
#  # create concatenate omics data for network pruning
#  X_big <- cbind(X1,X2)
#  
#  # calculate feature correlation matrix
#  bigCor2 <- cor(X_big)
#  
#  # data type
#  types <- c(rep('gene', ncol(X1)), rep('mirna', ncol(X2)))
#  
#  # filter out network modules with insufficient number of nodes
#  module_length <- unlist(lapply(OmicsModule, length))
#  network_modules <- OmicsModule[module_length > 10]
#  # extract pruned network modules
#  for(i in 1:length(network_modules))
#  {
#    cat(paste0('For network module: ', i, '\n'))
#    # define subnetwork
#    abar_sub <- Abar[network_modules[[i]],network_modules[[i]]]
#    cor_sub <- bigCor2[network_modules[[i]],network_modules[[i]]]
#    # prune network module
#    networkPruning(Abar = abar_sub,CorrMatrix = cor_sub,
#                            type = types[network_modules[[i]]],
#                   data = X_big[,network_modules[[i]]],
#  			  Pheno = Y, ModuleIdx = i, min_mod_size = 10,
#                            max_mod_size = 100, method = 'NetSHy',
#                            saving_dir = CVDir)
#  }

## ---- eval = TRUE,echo = FALSE, warning = FALSE, message = FALSE--------------
load('../vignettes/results/multi_omics_quant_size_18_net_1.Rdata')
row.names(omics_correlation_data) <- NULL
colnames(omics_correlation_data) <- c('Molecular Feature', 'Correlation to Phenotype', 'P-value')
knitr::kable(omics_correlation_data, caption = 'Individual molecular features correlation table with respect to phenotype (correlation and p-value).')

## ----loadings1, echo = FALSE, warning = FALSE, message = FALSE, out.width="100%", fig.cap = "PC1 loading for each subnetwork feature."----
library(grid)
library(tidyverse)
library(shadowtext)
library(reshape2)
BLUE <- "#076fa2"
data <- data.frame(name = row.names(pc_loading), loading = abs(pc_loading[,1]))
plt <- ggplot(data) +
  geom_col(aes(loading, name), fill = BLUE, width = 0.6) 

plt

## ----corheatmap, echo = FALSE, warning = FALSE, message = FALSE, out.width="100%", fig.cap = "Correlation heatmap for subnetwork features."----
###### Correlation heatmap
melted_cormat <- melt(correlation_sub)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
   labs(title = "Correlation Heatmap of Network Features") + 
  theme(plot.title.position = "plot")

## ----adjheatmap, echo = FALSE, warning = FALSE, message = FALSE, out.width="100%", fig.cap = "Adjacency matrix heatmap for subnetwork features."----
###### Correlation heatmap
melted_cormat <- melt(as.matrix(M))
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
   labs(title = "Adjacency Matrix Heatmap of Network Features") + 
  theme(plot.title.position = "plot")


## ---- eval = FALSE------------------------------------------------------------
#  library(RCy3)
#  library(igraph)
#  # load subnetwork data (example, user need to provide the directory)
#  load('ResultDirectory/size_a_net_b.Rdata')
#  M <- as.matrix(M)
#  correlation_filter <- 0.05
#  # correlation matrix filtering for the subnetwork edge-cut
#  filter_index <- which(abs(correlation_sub) < correlation_filter)
#  M_ind <- ifelse(correlation_sub > 0, 1, -1)
#  M_adj <- M * M_ind
#  M_adj[filter_index] <- 0
#  diag(M_adj) <- 0
#  
#  # network visualization through cytoscape
#  graph <- igraph::graph_from_adjacency_matrix(M_adj, mode = 'undirected',
#           weighted = TRUE, diag = TRUE, add.colnames = NULL, add.rownames = NA)
#  
#  # define network node type and connectivity and use them in cytoscape
#  V(graph)$type <- sub_type
#  V(graph)$type
#  V(graph)$connectivity <- rowSums(abs(M))
#  V(graph)$connectivity
#  # export subnetwork to Cytoscape
#  createNetworkFromIgraph(graph,"multi_omics_network")

## ----netPlot, out.width="100%", fig.cap = "Pruned module 1. The strength of the node connections is indicated by the thickness of edges. Red edges and blue edges are for negative and positive connections respectively. Red node represents genes, and blue node represent miRNAs.", echo = FALSE----
knitr::include_graphics("../vignettes/figures/MultiOmicsNet.png")

## ----CVpara tune, warning = FALSE, eval = FALSE-------------------------------
#  # number of folds in K-fold CV.
#  K <- 3
#  N <- nrow(X1)
#  # create a list of omics data
#  X <- list(X1, X2)
#  # number of omics dataset
#  num_omics <- 2
#  # tuning parameter candidate length for each omics data
#  tuneLength <- 5
#  # tuning parameter candadate range for each omics data
#  minTune <- 0.1
#  maxTune <- 0.5
#  # create empty matrix to store all possible penalty parameters
#  penSelect <- matrix(0, nrow = tuneLength, ncol = num_omics)
#  # create sparsity penalty options.
#  for (Idx in 1:ncol(penSelect))
#  {
#    penSelect[,Idx] <- seq(from = minTune,
#                           to = maxTune,
#                           length.out = tuneLength)
#  }
#  # expand grid
#  # convert matrix to list of columns
#  list_cols <- as.list(as.data.frame(penSelect))
#  # generate all possible combinations
#  PenExpand <- do.call(expand.grid, list_cols)
#  
#  # set a CV directory.
#  CVDir <- "Example3foldCVTune/"
#  dir.create(CVDir)

## ----make K-fold tune, eval = FALSE-------------------------------------------
#  set.seed(12345) # set random seed.
#  
#  # split data into folds
#  X <- lapply(X, scale)
#  foldIdx <- suppressWarnings(split(1:nrow(X[[1]]), sample(1:nrow(X[[1]]), K)))
#  folddata <- purrr::map(1:length(foldIdx), function(x){
#      Y <- as.matrix(Y)
#      X_train <- list()
#      X_test <- list()
#      Y_train <- list()
#      Y_test <- list()
#      for (i in 1:length(X))
#      {
#        X_train[[i]] <- X[[i]][-foldIdx[[x]],]
#        X_test[[i]] <- X[[i]][foldIdx[[x]],]
#      }
#      Y_train <- Y[-foldIdx[[x]],]
#      Y_test <- Y[foldIdx[[x]],]
#      return(list(X_train = X_train, X_test = X_test,Y_train = Y_train,
#                  Y_test = Y_test))
#  })
#  # name each fold of data
#  names(folddata) <- paste0('Fold_', 1:K)
#  # saving all preliminary data into local directory for reproducibility purpose
#  save(folddata, PenExpand,
#         file = paste0(CVDir, "CVData.RData"))

## ---- eval = FALSE------------------------------------------------------------
#  # create a function that set up the scaling factor candidate grids
#  gridCCcoef <- function(DataType, tuneLength = 5, tuneRangePheno = c(1,10))
#  {
#    # store the length of the data
#    datalength <- length(DataType)
#    phenotunelength <- datalength - 1
#    # create empty matrix for storing the candidate scaling factors
#    candCoef <- matrix(1, nrow = tuneLength^phenotunelength,
#                         ncol = ncol(utils::combn(datalength, 2)))
#    # create storage empty grid
#    phenoGrids <- matrix(0, nrow = tuneLength,
#                           ncol = phenotunelength)
#    # create grids
#    for (phenoIdx in 1:phenotunelength)
#    {
#      phenoGrids[,phenoIdx] <- seq(from = tuneRangePheno[1],
#                                   to = tuneRangePheno[2],
#                                   length.out = tuneLength)
#    }
#    # expand grid
#    # convert matrix to list of columns
#    list_cols <- as.list(as.data.frame(phenoGrids))
#    # generate combinations
#    phenoGridsExpand <- do.call(expand.grid, list_cols)
#    candCoef[,which(utils::combn(datalength,2)[2,] == datalength)] <- as.matrix(
#      phenoGridsExpand)
#    # provide column names
#    colnames(candCoef) <- apply(utils::combn(datalength,2),2, function(x){
#        paste0(DataType[x[1]],'_', DataType[x[2]])
#      })
#    # scale scaling factors so that each set of scaling factors sum up to 1
#    candCoef <- t(apply(candCoef, 1, function(x) {x/sum(x)}))
#    return(candCoef)
#  
#  }
#  
#  # scaling factor grids
#  CCcoefMatrix <- gridCCcoef(c('mRNA','miRNA', 'pheno'),
#                             tuneLength = 3, tuneRangePheno = c(5,10))
#  
#  
#  # create data matrix to store the cross-validation result
#  CVEval <- matrix(0, nrow = nrow(CCcoefMatrix), ncol = num_omics +
#                     ncol(utils::combn(num_omics + 1, 2)) + 3)
#  CVEval[,1:ncol(utils::combn(num_omics + 1, 2))] <- CCcoefMatrix
#  colnames(CVEval)<- c( paste0('CCcoef:',  colnames(CCcoefMatrix)),
#                        paste0('l',1:num_omics), "TrainingCC", "TestCC",
#                                  "CCPredError")

## ---- eval = FALSE------------------------------------------------------------
#  for (CCIdx in 1:nrow(CCcoefMatrix))
#  {
#    coef <- CCcoefMatrix[CCIdx,]
#    cat(paste0('Now running SmCCA for the scaling factor candidate ',
#               CCIdx, '\n'))
#  
#    future::plan(future::multisession, workers = K)
#    CVResult <- furrr::future_map(1:K, function(CVidx) {
#      # set scaling factor
#      CCcoef <- coef
#      # create empty vector for storing cross-validation result
#      RhoTrain <- RhoTest <- DeltaCor <- rep(0, nrow(PenExpand))
#      for(idx in 1:nrow(PenExpand))
#      {
#        # consider one pair of sparsity penalties at a time.
#        l <- PenExpand[idx, ]
#        # run SmCCA on the subsamples (Figure 1, Step II)
#        Ws <- getCanWeightsMulti(folddata[[CVidx]][["X_train"]],
#                             Trait = as.matrix(folddata[[CVidx]][["Y_train"]]),
#                             Lambda = as.numeric(l), NoTrait = FALSE,
#                             CCcoef = CCcoef)
#        # evaluate the canonical correlation for training and testing data
#        rho.train <-  getCanCorMulti(X = folddata[[CVidx]][["X_train"]],
#                                  Y = as.matrix(folddata[[CVidx]][["Y_train"]]),
#                                  CCWeight = Ws,
#                                  CCcoef = CCcoef)
#        rho.test <-  getCanCorMulti(X = folddata[[CVidx]][["X_test"]],
#                                  Y = as.matrix(folddata[[CVidx]][["Y_test"]]),
#                                  CCWeight = Ws,
#                                  CCcoef = CCcoef)
#  
#        # store cv result
#        RhoTrain[idx] <- round(rho.train, digits = 5)
#        RhoTest[idx] <- round(rho.test, digits = 5)
#        DeltaCor[idx] <- abs(rho.train - rho.test)
#  
#      }
#  
#      # record prediction errors for given CV fold and all sparsity penalty
#      # options.
#      DeltaCor.all <- cbind(RhoTrain, RhoTest, DeltaCor)
#      return(DeltaCor.all)
#    },.progress = TRUE,.options = furrr::furrr_options(seed = TRUE))
#    cat('\n')
#    # aggregate CV result and select the best penalty term
#    AggregatedCVResult <- Reduce("+", CVResult) / length(CVResult)
#    EvalMetric <- apply(AggregatedCVResult, 1, function(x) {x[3]/abs(x[2])})
#    # determine the best CV result
#    optIdx <- which.min(EvalMetric)
#    # fill in the optimal penalty pair for current scaling scaling
#    # factor selection as well as the evaluation result
#    CVEval[CCIdx,(ncol(utils::combn(num_omics + 1, 2))+
#                 1):ncol(CVEval)] <- c(as.numeric(PenExpand[optIdx,]),
#                        as.numeric(AggregatedCVResult[optIdx,]))
#  
#    # write out the cross-validation result
#    write.csv(CVEval,
#              file = paste0(CVDir, "/PredictionError.csv"), row.names = FALSE)
#  }

## ---- eval = FALSE------------------------------------------------------------
#  # read in the overall cv result
#  evalResult <- read.csv(paste0(CVDir, "/PredictionError.csv"))
#  # find the optn
#  evalOptIdx <- which.min(evalResult$CCPredError/abs(evalResult$TestCC))
#  # print the optimal result
#  evalResult[evalOptIdx,]

## ----get abar tune, eval = FALSE----------------------------------------------
#  # feature sampling proportions, 0.9 for miRNA since it has less features.
#  s <- c(0.7,0.9)
#  # number of subsamples.
#  SubsamplingNum <- 50
#  # run SmCCA on the subsamples (Figure 1, Step II)
#  Ws <- getRobustWeightsMulti(X,
#                          Trait = as.matrix(Y),
#                          NoTrait = FALSE,
#                          CCcoef = as.numeric(evalResult[evalOptIdx,
#                                   1:ncol(utils::combn(num_omics + 1,2))]),
#                          Lambda = as.numeric(evalResult[evalOptIdx,
#                                  (ncol(utils::combn(num_omics + 1, 2))
#                                   +1):(ncol(utils::combn(num_omics +
#                                   1, 2))+num_omics)]), s = s,
#                          SubsamplingNum = SubsamplingNum)

## ----flowchart-binary, fig.cap = "SmCCNet workflow overview for Binary Phenotype. X1, X2, and X3 are three omics data types for the same set of n subjects. Y indicates a Binary phenotype measure for those n subjects. Note that the flowchart demonstrate workflow for three omics data, it is also compatible with more than three omics data or two omics data.", echo = FALSE,out.width='100%'----
knitr::include_graphics("figures/SmCCNet-Binary.jpg")

## ----example data binary------------------------------------------------------
data(ExampleData)
head(X1[ , 1:6])
head(X2[ , 1:6])
# binarize phenotype variable
Y <- ifelse(Y > median(Y), 1, 0)
head(Y)

## ----p1p2 binary, eval = TRUE-------------------------------------------------
p1 <- ncol(X1)
p2 <- ncol(X2)
n <- nrow(X1)
AbarLabel <- c(colnames(cbind(X1, X2)))

## ----CVpara binary, warning = FALSE, eval = FALSE-----------------------------
#  # number of folds in K-fold CV.
#  K <- 3
#  N <- nrow(X1)
#  # create a list of omics data
#  X <- list(X1, X2)
#  # number of component for PLS
#  ncomp <- 3
#  # number of omics dataset
#  num_omics <- 2
#  # tuning parameter candidate length for each omics data
#  tuneLength <- 5
#  # tuning parameter candadate range for each omics data
#  minTune <- 0.1
#  maxTune <- 0.5
#  # create empty matrix to store all possible penalty parameters
#  penSelect <- matrix(0, nrow = tuneLength, ncol = num_omics)
#  # set up the evaluation metric (choose between 'accuracy', 'auc', 'precision',
#  # 'recall', 'f1')
#  metric <- 'auc'
#  # create sparsity penalty options.
#  for (Idx in 1:ncol(penSelect))
#  {
#    penSelect[,Idx] <- seq(from = minTune,
#                           to = maxTune,
#                           length.out = tuneLength)
#  }
#  # combine with penalty term for classifier
#  penSelect <- cbind(penSelect, seq(from = 0.5,
#                           to = 0.9,
#                           length.out = tuneLength))
#  # expand grid
#  # convert matrix to list of columns
#  list_cols <- as.list(as.data.frame(penSelect))
#  # generate all possible combinations
#  PenExpand <- do.call(expand.grid, list_cols)
#  
#  # set a CV directory.
#  CVDir <- "Example3foldCVBinary/"
#  dir.create(CVDir)

## ----make K-fold binary, eval = FALSE-----------------------------------------
#  set.seed(12345) # set random seed.
#  
#  # split data into folds
#  X <- lapply(X, scale)
#  foldIdx <- suppressWarnings(split(1:nrow(X[[1]]), sample(1:nrow(X[[1]]), K)))
#  folddata <- purrr::map(1:length(foldIdx), function(x){
#      Y <- as.matrix(Y)
#      X_train <- list()
#      X_test <- list()
#      Y_train <- list()
#      Y_test <- list()
#      for (i in 1:length(X))
#      {
#        X_train[[i]] <- X[[i]][-foldIdx[[x]],]
#        X_test[[i]] <- X[[i]][foldIdx[[x]],]
#      }
#      Y_train <- Y[-foldIdx[[x]],]
#      Y_test <- Y[foldIdx[[x]],]
#      return(list(X_train = X_train, X_test = X_test,Y_train = Y_train,
#                  Y_test = Y_test))
#  })
#  # name each fold of data
#  names(folddata) <- paste0('Fold_', 1:K)
#  # saving all preliminary data into local directory for reproducibility purpose
#  save(folddata, PenExpand,
#         file = paste0(CVDir, "CVData.RData"))

## ---- eval = FALSE------------------------------------------------------------
#  scalingFactor <- 1

## ----set scaling factors binary, eval = FALSE---------------------------------
#  scalingFactor <- scalingFactorInput(c('mRNA', 'miRNA'))

## ----run CV binary, eval = FALSE----------------------------------------------
#  # create an empty list to store the cv result
#  CVResult <- list()
#  # load cross-validation data
#  load(paste0(CVDir, "CVData.RData"))
#  for (CVidx in 1:K)
#  {
#    CCcoef <- scalingFactor
#    TrainScore <- TestScore <- rep(0, nrow(PenExpand))
#    for(idx in 1:nrow(PenExpand)){
#      # consider one pair of sparsity penalties at a time.
#      l <- PenExpand[idx, ]
#      # run multi-block PLS
#      CCcoef <- scalingFactor
#      # run multi-block PLS
#      suppressMessages(projection <- getRobustWeightsMultiBinary(
#                                     folddata[[CVidx]][["X_train"]],
#                                     as.numeric(folddata[[CVidx]][["Y_train"]]),
#                                     SubsamplingPercent=c(1,1),
#                                     Between_Discriminate_Ratio = c(1,1),
#                                     LambdaBetween = l[1,1:num_omics],
#                                     LambdaPheno = l[1,(num_omics + 1)],
#                                     SubsamplingNum = 1,
#                                     CCcoef = CCcoef,
#                                     ncomp_pls = ncomp, EvalClassifier = TRUE,
#                                     testData = folddata[[CVidx]][["X_test"]]))
#  
#  
#  
#  
#      # create training and testing data, and fit logistic regression model
#      train_data <- data.frame(x = projection[[1]],
#                               y = as.factor(folddata[[CVidx]][["Y_train"]]))
#      test_data <- data.frame(x =  projection[[2]])
#  
#      # catching error when performing the logistic regression
#      has_error <- FALSE
#      suppressWarnings(
#      tryCatch({
#          # fit logistic regression model
#          logisticFit <- stats::glm(y ~ ., family = 'binomial', data = train_data)
#          # make prediction for train/test set
#          train_pred <- stats::predict(logisticFit, train_data, type = 'response')
#          test_pred <- stats::predict(logisticFit, test_data, type = 'response')
#          train_score <- classifierEval(obs = folddata[[CVidx]][["Y_train"]],
#                                        pred = train_pred,
#                                        EvalMethod = metric, print_score = FALSE)
#          test_score <- classifierEval(obs = folddata[[CVidx]][["Y_test"]],
#                                        pred = test_pred,
#                                        EvalMethod = metric, print_score = FALSE)
#            },
#          error = function(e) {
#            cat("Caught an error:", e$message, "\n")
#            has_error <- TRUE
#          })
#          )
#  
#          TrainScore[idx] <- round(train_score, digits = 5)
#          TestScore[idx] <- round(test_score, digits = 5)
#  
#  
#      }
#  
#      # record prediction errors for given CV fold and all sparsity penalty
#      # options.
#      CVResult[[CVidx]] <- cbind(TrainScore, TestScore)
#  }

## ----aggregate error binary, eval = FALSE-------------------------------------
#  # aggregate CV result and select the best penalty term
#  AggregatedCVResult <- Reduce("+", CVResult) / length(CVResult)
#  # determine the best CV result
#  optIdx <- which.max(AggregatedCVResult[,2])

## ----best pen binary, eval = FALSE--------------------------------------------
#  # combine CV evaluation result with penalty candidates
#  overallCVInfo <- cbind(PenExpand, AggregatedCVResult)
#  # set column names for penalty terms for omics
#  colnames(overallCVInfo)[1:num_omics] <- paste0('l',1:num_omics)
#  # set column names for penalty terms for classifier
#  colnames(overallCVInfo)[num_omics+1] <- paste0('lpheno')
#  # save overall CV result
#  write.csv(overallCVInfo, file = paste0(CVDir, 'overallCVInfo.csv'),
#            row.names = FALSE)
#  # print out the best CV penalty pair and associated result
#  print(overallCVInfo[optIdx,])

## ----get abar binary, eval = FALSE--------------------------------------------
#  # feature sampling proportions, 0.9 for miRNA since it has less features.
#  s <- c(0.7,0.9)
#  # number of subsamples.
#  SubsamplingNum <- 50
#  # run SPLSDA on the subsamples
#  Ws <- getRobustWeightsMultiBinary(X,
#              as.numeric(Y),
#              SubsamplingPercent=s,
#              Between_Discriminate_Ratio = c(1,1),
#              LambdaBetween = as.numeric(overallCVInfo[optIdx,1:num_omics]),
#              LambdaPheno = as.numeric(overallCVInfo[optIdx,num_omics + 1]),
#              SubsamplingNum = SubsamplingNum,
#              CCcoef = scalingFactor,
#              ncomp_pls = ncomp, EvalClassifier = FALSE)

## ----sessionInfo--------------------------------------------------------------
sessionInfo()
warnings()

