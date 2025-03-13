#' @importFrom scDHA scDHA
#' @importFrom FNN knn
#' @importFrom purrr quietly
#' @importFrom stats dnorm
#' @title scCAN
#' @description This is the main function to perform sc-RNA seq data clustering clustering. scCAN is fully unsupervised scRNA-seq
#' clustering framework that uses deep neural network and network fusion-based clustering algorithm.
#' First, scCAN applies a non-negative autoencoder to filter scRNA-seq data.
#' Second, the filtered data is passed to stacked Bayesian autoencoder to get multiple low-dimensional representations of input data.
#' Subsequently, scCAN converts these compressed data into networks and unify those networks to a single graph.
#' Then, scCAN uses a spectral clustering algorithm to obtain final clusters assignment.
#' @param data Gene expression matrix, with rows represent samples and columns represent genes.
#' @param sparse Boolen variable indicating whether data is a sparse matrix. The input must be a non negative sparse matrix.
#' @param n.neighbors Number of neighboring cells that are used to caculate the edge's weight. The number of neighbors are set \code{n.neighbors = 30} by default.
#' @param alpha A hyper-parameter that is used to calculate the network kernel. The value is set to \code{alpha = 0.5} by default.
#' @param n.iters A hyper-parameter to set the number of network fusion iterations. It is set to \code{n.iters = 10} by default.
#' @param ncores Number of processor cores to use.
#' @param r.seed A parameter to set a seed for reproducibility. This values is set to \code{r.seed = 1} by default.
#' @param subsamp Enable subsampling process for big data. This values is set to \code{subsamp = T} by default.
#' @param alpha A hyper parameter that control the weight of graph. This values is set to \code{alpha = 0.5} by default.
#' @param k A vector to search for optimal number of cluster.
#' @param samp.size A parameter to control number of sub-sampled cells.
#' @return List with the following keys:
#' \itemize{
#' \item cluster - A numeric vector containing cluster assignment for each sample.
#' \item k  - The optimal number of cluster.
#' \item latent - The latent data generated from autoencoders.
#' }
#'
#' @references
#'
#' 1. Duc Tran, Hung Nguyen, Bang Tran, Carlo La Vecchia, Hung N. Luu, Tin Nguyen (2021). Fast and precise single-cell data analysis using a hierarchical autoencoder. Nature Communications, 12, 1029. doi: 10.1038/s41467-021-21312-2
#
#' @examples
#' \dontrun{
#' # Not run if scDHA has not installed yet.
#' # Load the package and the example data (SCE dataset)
#' library(scCAN)
#' #Load example data
#' data("SCE")
#'
#' #Get data matrix and label
#' data <- t(SCE$data); label <- as.character(SCE$cell_type1)
#'
#' #Generate clustering result, the input matrix has rows as samples and columns as genes
#' result <- scCAN(data, r.seed = 1)
#'
#' #Get the clustering result
#' cluster <- result$cluster
#'
#' #Calculate adjusted Rand Index
#' ari <- round(scCAN::adjustedRandIndex(cluster,label), 2)
#' message(paste0("ARI = ", ari))
#'
#' }
#' @export

scCAN <- function(data, sparse = FALSE, n.neighbors = 30, alpha = 0.5, n.iters = 10, ncores = 10, r.seed = 1, subsamp = TRUE, k = 2:15,samp.size = 5000){
  set.seed(r.seed)
  result <- purrr::quietly(scDHA)(data,sparse = sparse, ncores = ncores, seed = r.seed)$result
  latent <- result$latent
  if(subsamp ==TRUE){
    if(nrow(data)>5000){
      res <- cluster.big(result, n.neighbors = n.neighbors, alpha = alpha, n.iters = n.iters, r.seed = r.seed,samp.size =samp.size, k = k)
      res$latent <- latent
    }else{
      res <- cluster.small(result,n.neighbors = n.neighbors, alpha = alpha, n.iters = n.iters, r.seed = r.seed, k = k)
      res$latent <- latent
    }
  }else{
    res <- cluster.small(result,n.neighbors = n.neighbors, alpha = alpha, n.iters = n.iters, r.seed = r.seed, k = k)
    res$latent <- latent
  }
  res
}

cluster.big <- function(data, samp.size = 5000, n.neighbors = 30, alpha = 0.5, n.iters = 10, r.seed = 1,k = 2:15){
  set.seed(r.seed)

  n_samples <- nrow(data$all.latent[[1]])
  groups <- rep(0, n_samples)
  message(n_samples)
  ind <- sample.int(n_samples, size = samp.size)

  all.sim <- list()
  for(i in 1 : length(data$all.latent)){
    dat <- data$all.latent[[i]][ind,]
    tmp <- dist2(dat,dat)^(1/2)
    all.sim[[i]] <- tmp
  }

  all.aff <- list()
  for(i in 1 : length(all.sim)){
    dat <- all.sim[[i]]
    tmp <- affinityMatrix(dat, sigma = alpha)
    all.aff[[i]] <- tmp
  }

  W = SNF(all.aff, n.neighbors, n.iters)
  res = estimateNumberOfClustersGivenGraph(W, NUMC=k)
  k1 <- res$`Eigen-gap best`
  k2 =res$`Eigen-gap 2nd best`
  k <- min(k1,k2)
  message(paste0("The optimal number of cluster is: ", k))

  cluster1 = spectralClustering(W,k)
  train <- data$latent[ind,]
  test  <- data$latent[-ind,]
  cluster2 <- FNN::knn(train, test, cluster1, k = 10, prob = FALSE)

  groups[ind] <-cluster1
  groups[-ind]<-cluster2

  list(cluster = groups,
       k = k)
}

cluster.small <- function(data, n.neighbors = 30, alpha = 0.5, n.iters = 10, r.seed = 1,k = 2:15){
  set.seed(r.seed)

  all.sim <- list()
  for(i in 1 : length(data$all.latent)){
    dat <- data$all.latent[[i]]
    tmp <- dist2(dat,dat)^(1/2)
    all.sim[[i]] <- tmp
  }

  all.aff <- list()
  for(i in 1 : length(all.sim)){
    dat <- all.sim[[i]]
    tmp <- affinityMatrix(dat, sigma = alpha)
    all.aff[[i]] <- tmp
  }

  W = SNF(all.aff, n.neighbors, n.iters)
  res = estimateNumberOfClustersGivenGraph(W, NUMC=k)
  k1 <- res$`Eigen-gap best`
  k2 =res$`Eigen-gap 2nd best`
  k <- min(k1,k2)
  message(paste0("The optimal number of cluster is: ", k))
  groups = spectralClustering(W,k)

  list(cluster = groups,
       k = k)
}

#' @title SCE
#'
#' @description SCE dataset includes scRNA-seq data and cell type information.
"SCE"
