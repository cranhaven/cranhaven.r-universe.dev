#'@title Peak-Gene Network via Lasso
#'
#'@description Construct the peak-gene network via Lasso.
#'@name PGN_Lasso
#'@import Matrix
#'@import glmnet
#'@importFrom stats coef
#'@importFrom parallel makeCluster stopCluster
#'@importFrom doParallel registerDoParallel
#'@importFrom foreach foreach %dopar%
#'@importFrom tictoc tic toc
#'@param X The scATAC-seq data, sparse matrix.
#'@param Y The scRNA-seq data, sparse matrix.
#'@param gene_data The information for genes, must have a col names "gene_name".
#'@param neibor_peak The peak IDs within a certain range of each gene, must have cols c("gene_name", "start_use", "end_use"). The id numbers in "start_use" and "end_use" are start from 0.
#'@param dirpath The folder path to read or write file.
#'@param count_device The number of cpus used to train the Lasso model.
#'@param rebuild_PGN_Lasso Logical. Whether to rebuild the peak-gene network via Lasso from scratch. If FALSE, the function will attempt to read from `PGN_Lasso.mtx` under \cr `dirpath/test` in single mode or `dirpath/state_name/test` in compare mode.
#'@param save_file Logical, whether to save the output to a file.
#'@return The PGN_Lasso network.
#'@examples
#'\donttest{
#' library(scPOEM)
#' dirpath <- "./example_data"
#' # Download single mode example data
#' data(example_data_single)
#' # Construct PGN net via Lasso.
#' net_Lasso <- PGN_Lasso(example_data_single$X,
#'                        example_data_single$Y,
#'                        example_data_single$gene_data,
#'                        example_data_single$neibor_peak,
#'                        file.path(dirpath, "single"),
#'                        save_file=FALSE)
#'}
#'
#'@export

PGN_Lasso <- function(X, Y, gene_data, neibor_peak, dirpath=tempdir(), count_device=1, rebuild_PGN_Lasso=TRUE, save_file=TRUE){
  if (!rebuild_PGN_Lasso){
    message("Load peak-gene network constructed by Lasso\n")
    net_lasso <- readMM(file.path(dirpath, "test/PGN_Lasso.mtx"))
    return(net_lasso)
  }
  message("Construct peak-gene network via Lasso from scratch...\n")
  myic.glmnet <- function (x, y, ...)
  {
    n = length(y)
    model = glmnet(x = x, y = y, alpha = 1, ...)
    coefs = coef(model)
    lambda = model$lambda
    df = model$df
    yhat = cbind(1, x) %*% coefs
    residuals = (y - yhat)
    mse = colMeans(residuals^2)
    sse = colSums(residuals^2)
    nvar = df + 1
    bic = n * log(mse) + nvar * log(n)
    selected = best.model = which(bic == min(bic))
    ic = c(bic = bic[selected])
    result = list(coefficients = coefs[-1, selected], ic = ic,
                  lambda = lambda[selected], nvar = nvar[selected], glmnet = model,
                  fitted.values = yhat[, selected], bic = bic, df = df, call = match.call())
    class(result) = "ic.glmnet"
    return(result)
  }

  GetPredictionwithCrspPeaksLASSO <- function(X, Y, f=myic.glmnet, neibor_peak, gene_data, count_device){

    func.Optimal.lambda.match <-  function(ii){
      gene <- gene_data$gene_name[ii]
      coefficient_extension <- rep(0, ncol(X))
      if (!(gene %in% neibor_peak$gene_name)){
        return(coefficient_extension)
      }
      gene.info <- neibor_peak[neibor_peak$gene_name == gene, ]
      peak.ind <- seq(from = gene.info$start_use+1, to = gene.info$end_use+1)

      if(peak.ind[1]==-1){
        coefficient_extension <- Matrix(coefficient_extension, nrow = ncol(X), sparse = TRUE)
        return(coefficient_extension)
      }
      if(length(peak.ind)<=5){
        length_peak = length(peak.ind)
        peak.ind <- seq(from = peak.ind[1], to = min(peak.ind[1]+5, ncol(X)))
      }
      if(sum(Y[,ii])==0){
        coefficient_extension <- Matrix(coefficient_extension, nrow = ncol(X), sparse = TRUE)
        return(coefficient_extension)
      }
      icmodel=f(x=as.matrix(X[, peak.ind, drop = FALSE]), y=Y[,ii])
      para.num = icmodel[["nvar"]]-1
      lambda = icmodel[["lambda"]]
      coefficient = icmodel[["coefficients"]]
      if (para.num<5) {
        icmodel=icmodel[["glmnet"]]
        if(max(icmodel[["df"]])<5){
          ind = min(which(icmodel[["df"]]==max(icmodel[["df"]])))
        }
        else{
          ind = min(which(icmodel[["df"]]>=5))
        }
        coefficient=coef(icmodel)[-1, ind]
      }

      coefficient_extension[peak.ind] <- coefficient
      coefficient_extension <- Matrix(coefficient_extension, nrow = ncol(X), sparse = TRUE)
      return(coefficient_extension)
    }


    cl.cores = count_device
    cl = makeCluster(cl.cores)
    registerDoParallel(cl)
    tic()
    ii <- NULL
    result.pre <- foreach(ii=1:ncol(Y),
                          .combine = "cbind",
                          .packages = c("Matrix","glmnet")
    ) %dopar% func.Optimal.lambda.match(ii)
    stopCluster(cl)
    toc()
    #colnames(result.pre)=colnames(Y)
    #row.names(result.pre)=colnames(X)
    return(result.pre)
  }

  #X <- Matrix::readMM(file.path(dirpath, "X.mtx"))
  #Y <- Matrix::readMM(file.path(dirpath, "Y.mtx"))

  #peak_data <- read.csv(file.path(dirpath, "peak_data.csv"))
  #colnames(peak_data) <- c('rank', 'peak_name')
  #colnames(X) <- peak_data$peak_name
  #gene_data <- read.csv(file.path(dirpath, "gene_data.csv"))
  #colnames(gene_data) <- c('rank', 'gene_name')
  #colnames(Y) <- gene_data$gene_name
  #cell_data <- read.csv(file.path(dirpath, "cell_data.csv"))
  #colnames(cell_data) <- c('rank', 'cell_name')
  #row.names(X) <- cell_data$cell_name
  #row.names(Y) <- cell_data$cell_name

  #neibor_peak <- read.csv(file.path(dirpath,"peakuse_100kbp.csv"))

  net_lasso <- GetPredictionwithCrspPeaksLASSO(X, Y, f=myic.glmnet, neibor_peak, gene_data, as.integer(count_device))
  if (save_file==TRUE){
    if (!dir.exists(file.path(dirpath, "test"))) {
      dir.create(file.path(dirpath, "test"), showWarnings = FALSE, recursive = TRUE)
    }
    writeMM(net_lasso, file = file.path(dirpath, "test/PGN_Lasso.mtx"))
    message("PGN_Lasso is saved in:", file.path(dirpath, "test/PGN_Lasso.mtx"), "\n")
  }

  return(net_lasso)
}


#args <- commandArgs(trailingOnly = TRUE)
#dirpath <- NULL
#count_device <- NULL

#for (i in seq(1, length(args))) {
#  if (args[i] == "--dirpath") {
#    dirpath <- args[i+1]
#  }
#  if (args[i] == "--count_device") {
#    count_device <- as.integer(args[i+1])
#  }
#}

#if (is.null(dirpath)){
#  dirpath <- "data_example/single"
#}
#if(is.null(count_device)){
#  count_device <- 1
#}

#PGN_Lasso(dirpath, count_device)
