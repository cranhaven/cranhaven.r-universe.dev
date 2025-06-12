## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- echo = FALSE, results = "hide", warning = FALSE, eval = TRUE------------
suppressPackageStartupMessages({
    library(pbapply)
    library(Matrix)
    library(igraph)
})

## ---- eval = TRUE-------------------------------------------------------------
library(pbapply)
library(Matrix)
library(igraph)
library(SmCCNet)
library(parallel)

## ----flowPlot, out.width="100%", fig.cap = "Workflow for SmCCNet single-omics setting (binary and quantitative phenotype).", echo = FALSE----
knitr::include_graphics("figures/single-omics-smccnet.jpg")

## ----example data-------------------------------------------------------------
data(ExampleData)
head(X1[ , 1:6])
head(Y)

## ----p1p2, eval = TRUE--------------------------------------------------------
p1 <- ncol(X1)
N <- nrow(X1)
AbarLabel <- colnames(X1)

## ---- eval = FALSE------------------------------------------------------------
#  # preprocess data
#  processed_data <- dataPreprocess(X = as.data.frame(X1), covariates = NULL,
#                    is_cv = TRUE, cv_quantile = 0.2, center = TRUE, scale = TRUE)

## ----CVpara, eval = FALSE, warning = FALSE------------------------------------
#  K <- 3 # number of folds in K-fold CV.
#  s1 <- 0.7
#  subSamp <- 50 # number of subsamples (will be described in later section).
#  
#  # create sparsity penalty options.
#  pen1 <- seq(.05, .3, by = .05)
#  
#  # set a CV directory.
#  CVDir <- "Example3foldCV/"
#  pheno <- "Example3foldCV"
#  dir.create(CVDir)

## ----make K-fold, eval = FALSE------------------------------------------------
#  # set random seed
#  set.seed(12345)
#  
#  # save data and parameters into local directory
#  save(X1, Y, s1, subSamp, pen1,
#       file = paste0(CVDir, "Data.Rdata"))
#  
#  # split data into K folds
#  foldIdx <- split(1:N, sample(1:N, K))
#  for(i in 1:K){
#    iIdx <- foldIdx[[i]]
#    x1.train <- scale(X1[-iIdx, ])
#    yy.train <- Y[-iIdx, ]
#    x1.test <- scale(X1[iIdx, ])
#    yy.test <- Y[iIdx, ]
#  
#    if(is.na(min(min(x1.train), min(yy.train), min(x1.test), min(yy.test)))){
#      stop("Invalid scaled data.")
#    }
#  
#    subD <- paste0(CVDir, "CV_", i, "/")
#    dir.create(subD)
#  
#    # save data from each fold into local directory
#    save(x1.train, yy.train, x1.test, yy.test,
#         pen1, p1,
#         file = paste0(subD, "Data.Rdata"))
#  }

## ----run CV, eval = FALSE-----------------------------------------------------
#  # number of clusters in parSapply should be the same as number specified above
#  suppressWarnings(for (CVidx in 1:K)
#  {
#    # define the sub-directory for each fold
#    subD <- paste0(CVDir, "CV_", CVidx, "/")
#    # load fold data
#    load(paste0(subD, "Data.Rdata"))
#    dir.create(paste0(subD, "SmCCA/"))
#    # create empty vector to store cross-validation result
#    RhoTrain <- RhoTest <- DeltaCor <- rep(0, length(pen1))
#    # evaluate through all the possible penalty candidates
#    for(idx in 1:length(pen1)){
#      l1 <- pen1[idx]
#      print(paste0("Running SmCCA on CV_", CVidx, " pen=", l1))
#      # run single-omics SmCCNet
#      Ws <- getRobustWeightsSingle(x1.train, as.matrix(yy.train), l1, 1,
#                                      SubsamplingNum = 1)
#      # average
#      meanW <- rowMeans(Ws)
#      v <- meanW[1:p1]
#  
#      rho.train <-  cor(x1.train %*% v, yy.train)
#  
#  
#      rho.test <- cor(x1.test %*% v, yy.test)
#  
#  
#      RhoTrain[idx] <- round(rho.train, digits = 5)
#      RhoTest[idx] <- round(rho.test, digits = 5)
#      DeltaCor[idx] <- abs(rho.train - rho.test)
#  
#  
#  
#    }
#  
#    DeltaCor.all <- cbind(pen1, RhoTrain, RhoTest, DeltaCor)
#    colnames(DeltaCor.all) <- c("l1", "Training CC", "Test CC", "CC Pred. Error")
#    write.csv(DeltaCor.all,
#              file = paste0(subD, "SmCCA/SCCA_", subSamp,"_allDeltaCor.csv"))
#  
#  
#  })

## ----aggregate error, eval = FALSE--------------------------------------------
#  # combine prediction errors from all K folds and compute the total prediction
#  # error for each sparsity penalty pair.
#  aggregateCVSingle(CVDir, "SmCCA", NumSubsamp = subSamp, K = K)

## ----get abar, eval = FALSE---------------------------------------------------
#  # set up directory to store all the results
#  plotD <- paste0(CVDir, "Figures/")
#  saveD <- paste0(CVDir, "Results/")
#  dataF <- paste0(CVDir, "Data.Rdata")
#  dir.create(plotD)
#  dir.create(saveD)
#  dir.create(dataF)
#  
#  # type of CCA result, only "SmCCA" supported
#  Method = "SmCCA"
#  
#  
#  # after SmCCA CV, select the best penalty term,
#  # and use it for running SmCCA on the complete dataset
#  for(Method in "SmCCA"){
#    # select optimal penalty term from CV result
#    T12 <- read.csv(paste0(CVDir, "Results/", Method, "CVmeanDeltaCors.csv"))
#    # calculate evaluation metric **
#    pen <- T12[which.min(T12[ ,4]/abs(T12[ ,3])) ,2]
#  
#  
#    l1 <- pen;
#    system.time({
#      Ws <- getRobustWeightsSingle(X1 = X1, Trait = as.matrix(Y),
#                                         Lambda1 = l1,
#                                         s1, SubsamplingNum = subSamp)
#  
#  
#      Abar <- getAbar(Ws, FeatureLabel = AbarLabel[1:p1])
#      save(l1, X1, Y, s1, Ws, Abar,
#           file = paste0(saveD, Method, K, "foldSamp", subSamp, "_", pen,
#                         ".Rdata"))
#    })
#  
#  
#  }

## ----get modules, eval = FALSE------------------------------------------------
#  # perform clustering based on the adjacency matrix Abar
#  OmicsModule <- getOmicsModules(Abar, PlotTree = FALSE)
#  
#  # make sure there are no duplicated labels
#  AbarLabel <- make.unique(AbarLabel)
#  
#  # calculate feature correlation matrix
#  bigCor2 <- cor(X1)
#  
#  # data type
#  types <- rep('gene', nrow(bigCor2))

## ---- eval = FALSE------------------------------------------------------------
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
#                   data = X1[,network_modules[[i]]],
#  			  Pheno = Y, ModuleIdx = i, min_mod_size = 10,
#                            max_mod_size = 100, method = 'PCA',
#                            saving_dir = getwd())
#    cat("\n")
#  }

## ---- eval = TRUE,echo = FALSE, warning = FALSE, message = FALSE--------------
load('../vignettes/cont_size_16_net_1.Rdata')
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
#  
#  # correlation matrix for the subnetwork
#  filter_index <- which(abs(correlation_sub) < 0.05)
#  M_ind <- ifelse(correlation_sub > 0, 1, -1)
#  M_adj <- M * M_ind
#  M_adj[filter_index] <- 0
#  diag(M_adj) <- 0
#  
#  # network visualization through cytoscape
#  graph <- igraph::graph_from_adjacency_matrix(M_adj, mode = 'undirected',
#           weighted = TRUE, diag = TRUE, add.colnames = NULL, add.rownames = NA)
#  
#  # define network node type and connectivity
#  V(graph)$type <- sub_type
#  V(graph)$type
#  V(graph)$connectivity <- rowSums(abs(M))
#  V(graph)$connectivity
#  
#  createNetworkFromIgraph(graph,"single_omics_network")

## ----netPlot, out.width="100%", fig.cap = "Trimmed module 1. The strength of the node connections is indicated by the thickness of edges. Red edges and blue edges are for negative and positive connections respectively.", echo = FALSE----
knitr::include_graphics("../vignettes/example_network_cont.png")

## ----example data binary------------------------------------------------------
data(ExampleData)
head(X1[ , 1:6])
head(Y)
# binarize quantitative outcome 
Y <- ifelse(Y > median(Y), 1, 0)

## ----p1p2 binary, eval = TRUE-------------------------------------------------
p1 <- ncol(X1)
N <- nrow(X1)
AbarLabel <- colnames(X1)

## ---- eval = FALSE------------------------------------------------------------
#  K <- 3 # number of folds in K-fold CV.
#  s1 <- 0.7 # feature subsampling percentage.
#  subSamp <- 50 # number of subsamples.
#  CCcoef <- NULL # unweighted version of SmCCNet.
#  metric <- 'auc' # evaluation metric to be used.
#  
#  # create sparsity penalty options.
#  pen1 <- seq(.1, .9, by = .1)
#  
#  # set a CV directory.
#  CVDir <- "Example3foldCV_Binary/"
#  pheno <- "Example3foldCV_Binary"
#  dir.create(CVDir)
#  
#  Y <- ifelse(Y > median(Y), 1, 0)

## ----make K-fold 2, eval = FALSE----------------------------------------------
#  set.seed(12345) # set random seed.
#  # save original data into local directory
#  save(X1, Y, s1, subSamp, pen1,
#       file = paste0(CVDir, "Data.Rdata"))
#  
#  
#  # define the number of components to be extracted
#  ncomp <- 3
#  # split data into k folds
#  foldIdx <- split(1:N, sample(1:N, K))
#  for(i in 1:K){
#    iIdx <- foldIdx[[i]]
#    x1.train <- scale(X1[-iIdx, ])
#    yy.train <- Y[-iIdx, ]
#    x1.test <- scale(X1[iIdx, ])
#    yy.test <- Y[iIdx, ]
#  
#    if(is.na(min(min(x1.train), min(yy.train), min(x1.test), min(yy.test)))){
#      stop("Invalid scaled data.")
#    }
#  
#    subD <- paste0(CVDir, "CV_", i, "/")
#    dir.create(subD)
#    save(x1.train, yy.train, x1.test, yy.test,
#        pen1, p1,
#         file = paste0(subD, "Data.Rdata"))
#  }
#  
#  ############################################## Running Cross-Validation
#  
#  # run SmCCA with K-fold cross-validation
#  suppressWarnings(for (CVidx in 1:K){
#    # define evaluation method
#    EvalMethod <- 'precision'
#    # define and create saving directory
#    subD <- paste0(CVDir, "CV_", CVidx, "/")
#    load(paste0(subD, "Data.Rdata"))
#    dir.create(paste0(subD, "SmCCA/"))
#  
#    # create empty vector to store cross-validation accuracy result
#    TrainScore <- TestScore <- rep(0, length(pen1))
#    for(idx in 1:length(pen1)){
#      # define value of penalty
#      l1 <- pen1[idx]
#  
#      # run SPLS-DA to extract canonical weight
#      Ws <- spls::splsda(x = x1.train, y = yy.train, K = ncomp, eta = l1,
#                         kappa=0.5,
#                         classifier= 'logistic', scale.x=FALSE)
#  
#      # create emtpy matrix to save canonical weights for each subsampling
#      weight <- matrix(0,nrow = ncol(x1.train), ncol = ncomp)
#      weight[Ws[["A"]],] <- Ws[["W"]]
#  
#      # train the latent factor model with logistic regression
#      train_data <- data.frame(x = (x1.train %*% weight)[,1:ncomp],
#                               y = as.factor(yy.train))
#      test_data <- data.frame(x = (x1.test %*% weight)[,1:ncomp])
#  
#      logisticFit <- stats::glm(y~., family = 'binomial',data = train_data)
#      # make prediction for train/test set
#      train_pred <- stats::predict(logisticFit, train_data, type = 'response')
#      test_pred <- stats::predict(logisticFit, test_data, type = 'response')
#      # specifying which method to use as evaluation metric
#      TrainScore[idx] <- classifierEval(obs = yy.train,
#                                        pred = train_pred,
#                                        EvalMethod = metric, print_score = FALSE)
#      TestScore[idx] <- classifierEval(obs = yy.test,
#                                        pred = test_pred,
#                                        EvalMethod = metric, print_score = FALSE)
#  
#    }
#  
#    # combine cross-validation results
#    DeltaAcc.all <- as.data.frame(cbind(pen1, TrainScore, TestScore))
#    DeltaAcc.all$Delta <- abs(DeltaAcc.all$TrainScore - DeltaAcc.all$TestScore)
#    colnames(DeltaAcc.all) <- c("l1", "Training Score", "Testing Score", "Delta")
#  
#    # save cross-validation results to local directory
#    write.csv(DeltaAcc.all,
#              file = paste0(subD, "SmCCA/SCCA_", subSamp,"_allDeltaCor.csv"))
#  
#  }
#  )

## ---- eval = FALSE------------------------------------------------------------
#  # save cross-validation result
#  cv_result <- aggregateCVSingle(CVDir, "SmCCA", NumSubsamp = subSamp, K = 3)
#  
#  # create directory to save overall result with the best penalty term
#  plotD <- paste0(CVDir, "Figures/")
#  saveD <- paste0(CVDir, "Results/")
#  dataF <- paste0(CVDir, "Data.Rdata")
#  dir.create(plotD)
#  dir.create(saveD)
#  dir.create(dataF)
#  
#  # specify method (only SmCCA works)
#  Method = "SmCCA"
#  
#  for(Method in "SmCCA"){
#    # read cross validation result in
#    T12 <- read.csv(paste0(CVDir, "Results/", Method, "CVmeanDeltaCors.csv"))
#    # determine the optimal penalty term
#    pen <- T12[which.max(T12[ ,3]) ,2]
#    l1 <- pen;
#    system.time({
#      # run SPLSDA with optimal penalty term
#      Ws <- getRobustWeightsSingleBinary(X1 = X1, Trait = as.matrix(Y),
#                                         Lambda1 = l1,
#                                         s1, SubsamplingNum = subSamp)
#  
#      # get adjacency matrix
#      Abar <- getAbar(Ws, FeatureLabel = AbarLabel[1:p1])
#      # save result into local directory
#      save(l1, X1, Y, s1, Ws, Abar,
#           file = paste0(saveD, Method, K, "foldSamp", subSamp, "_", pen,
#                         ".Rdata"))
#    })
#  
#  
#  }

## ----sessionInfo--------------------------------------------------------------
sessionInfo()
warnings()

