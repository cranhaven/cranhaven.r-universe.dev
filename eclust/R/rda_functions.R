#' Cluster data using environmental exposure
#'
#' @description This is one of the functions for real data analysis, which will
#'   cluster the data based on the environment, as well as ignoring the
#'   environment
#' @param cluster_distance character representing which matrix from the training
#'   set that you want to use to cluster the genes. Must be one of the following
#'   \itemize{ \item corr, corr0, corr1, tom, tom0, tom1, diffcorr, difftom,
#'   corScor, tomScor, fisherScore }
#' @param eclust_distance character representing which matrix from the training
#'   set that you want to use to cluster the genes based on the environment. See
#'   \code{cluster_distance} for avaialble options. Should be different from
#'   \code{cluster_distance}. For example, if \code{cluster_distance=corr} and
#'   \code{EclustDistance=fisherScore}. That is, one should be based on
#'   correlations ignoring the environment, and the other should be based on
#'   correlations accounting for the environment. This function will always
#'   return this add on
#' @param measure_distance  one of "euclidean","maximum","manhattan",
#'   "canberra", "binary","minkowski" to be passed to \code{\link[stats]{dist}}
#'   function for calculating the distance for the clusters based on the
#'   corr,corr1,corr0, tom, tom0, tom1 matrices
#' @param data n x p matrix of data. rows are samples, columns are genes or cpg
#'   sites. Should not contain the environment variable
#' @param response numeric vector of length n
#' @param exposure binary (0,1) numeric vector of length n for the exposure
#'   status of the n samples
#' @param train_index numeric vector indcating the indices of \code{response}
#'   and the rows of \code{data} that are in the training set
#' @param test_index numeric vector indcating the indices of \code{response} and
#'   the rows of \code{data} that are in the test set
#' @param minimum_cluster_size The minimum cluster size. Only applicable if
#'   \code{cutMethod='dynamic'}. This argument is passed to the
#'   \code{\link[dynamicTreeCut]{cutreeDynamic}} function through the
#'   \code{\link{u_cluster_similarity}} function. Default is 50.
#' @param ... arguments passed to the \code{\link{u_cluster_similarity}}
#'   function
#' @seealso \code{\link{u_cluster_similarity}}
#' @return a list of length 8: \describe{\item{clustersAddon}{clustering results
#'   based on the environment and not the environment. see
#'   \code{\link{u_cluster_similarity}} for
#'   details}\item{clustersAll}{clustering results ignoring the environment. See
#'   \code{\link{u_cluster_similarity}} for details}\item{etrain}{vector of the
#'   exposure variable for the training
#'   set}\item{cluster_distance_similarity}{the similarity matrix based on the
#'   argument specified in
#'   \code{cluster_distance}}\item{eclust_distance_similarity}{the similarity
#'   matrix based on the argument specified in
#'   \code{eclust_distance}}\item{clustersAddonMembership}{a data.frame and
#'   data.table of the clustering membership for clustering results based on the
#'   environment and not the environment. As a result, each gene will show up
#'   twice in this table}\item{clustersAllMembership}{a data.frame and
#'   data.table of the clustering membership for clustering results based on all
#'   subjects i.e. ignoring the environment. Each gene will only show up once in
#'   this table}\item{clustersEclustMembership}{a data.frame and data.table of
#'   the clustering membership for clustering results accounting for the
#'   environment. Each gene will only show up once in this table}}
#' @details This function clusters the data. The results of this function should
#'   then be passed to the \code{\link{r_prepare_data}} function which output
#'   the appropriate X and Y matrices in the right format for regression
#'   packages such as \code{mgcv}, \code{caret} and \code{glmnet}
#' @examples
#' data("tcgaov")
#' tcgaov[1:5,1:6, with = FALSE]
#' Y <- log(tcgaov[["OS"]])
#' E <- tcgaov[["E"]]
#' genes <- as.matrix(tcgaov[,-c("OS","rn","subtype","E","status"),with = FALSE])
#' trainIndex <- drop(caret::createDataPartition(Y, p = 0.5, list = FALSE, times = 1))
#' testIndex <- setdiff(seq_len(length(Y)),trainIndex)
#'
#' \dontrun{
#' cluster_res <- r_cluster_data(data = genes,
#'                               response = Y,
#'                               exposure = E,
#'                               train_index = trainIndex,
#'                               test_index = testIndex,
#'                               cluster_distance = "tom",
#'                               eclust_distance = "difftom",
#'                               measure_distance = "euclidean",
#'                               clustMethod = "hclust",
#'                               cutMethod = "dynamic",
#'                               method = "average",
#'                               nPC = 1,
#'                               minimum_cluster_size = 60)
#'
#' # the number of clusters determined by the similarity matrices specified
#' # in the cluster_distance and eclust_distance arguments. This will always be larger
#' # than cluster_res$clustersAll$nclusters which is based on the similarity matrix
#' # specified in the cluster_distance argument
#' cluster_res$clustersAddon$nclusters
#'
#' # the number of clusters determined by the similarity matrices specified
#' # in the cluster_distance argument only
#' cluster_res$clustersAll$nclusters
#' }
#' @export
r_cluster_data <- function(data,
                           response,
                           exposure,
                           train_index,
                           test_index,
                           cluster_distance = c("corr", "corr0", "corr1", "tom",
                                                "tom0", "tom1", "diffcorr",
                                                "difftom", "fisherScore"),
                           eclust_distance = c("fisherScore", "corScor", "diffcorr",
                                               "difftom"),
                           measure_distance = c("euclidean","maximum","manhattan", "canberra",
                                               "binary","minkowski"),
                           minimum_cluster_size = 50, ...) {

  # rm(list = ls())
  # data("tcgaov")
  # response <- log(tcgaov[["OS"]])
  # exposure <- tcgaov[["E"]]
  # data <- as.matrix(tcgaov[,-c("OS","rn","subtype","E","status"),with=F])
  # train_index <- drop(caret::createDataPartition(response, p = 0.5, list = FALSE, times = 1))
  # test_index <- setdiff(seq_len(length(response)),train_index)
  # cluster_distance = "tom"
  # eclust_distance = "difftom"
  # measure_distance = "maximum"
  # clustMethod = "hclust"
  # cutMethod = "dynamic"
  # method = "average"
  # nPC = 1
  # minimum_cluster_size = 50

  # ==================================================================

  cluster = NULL
  args <- list(...)
  cluster_distance <- match.arg(cluster_distance)
  eclust_distance <- match.arg(eclust_distance)
  measure_distance <- match.arg(measure_distance)

  xtrain <- data[train_index,]
  xtest <- data[test_index,]
  etrain <- exposure[train_index]

  ytrain <- response[train_index]
  ytest <- response[test_index]

  xtrainE0 <- xtrain[which(etrain == 0), ]
  xtrainE1 <- xtrain[which(etrain == 1), ]

  corrTrain <- WGCNA::cor(xtrain)

  #   message(sprintf("Calculating number of clusters based on %s using %s with %s
  #                   linkage and the %s method to determine the number of clusters",
  #                   cluster_distance, cluster_method, agglomeration_method, cut_method))

  #############################################################################
  #               CORRELATION/TOM CLUSTERS                                    #
  #############################################################################

  # this cannot be based on difftom, diffcorr, fisherScore
  if (cluster_distance %in% c("difftom", "diffcorr", "fisherScore"))
    stop(message("cluster_distance must be one of corr, corr0, corr1, tom, tom0, tom1"))

  # clusters based on cluster_distance argument
  similarity <- switch(cluster_distance,
                       corr = corrTrain,
                       corr0 = WGCNA::cor(xtrainE0),
                       corr1 = WGCNA::cor(xtrainE1),
                       tom0 = {
                         tomTrainE0 <- WGCNA::TOMsimilarityFromExpr(xtrainE0)
                         dimnames(tomTrainE0)[[1]] <- dimnames(corrTrain)[[1]]
                         dimnames(tomTrainE0)[[2]] <- dimnames(corrTrain)[[2]]
                         tomTrainE0
                       },
                       tom1 = {
                         tomTrainE1 <- WGCNA::TOMsimilarityFromExpr(xtrainE1)
                         dimnames(tomTrainE1)[[1]] <- dimnames(corrTrain)[[1]]
                         dimnames(tomTrainE1)[[2]] <- dimnames(corrTrain)[[2]]
                         tomTrainE1
                       },
                       tom = {
                         tomTrainAll <- WGCNA::TOMsimilarityFromExpr(xtrain)
                         dimnames(tomTrainAll)[[1]] <- dimnames(corrTrain)[[1]]
                         dimnames(tomTrainAll)[[2]] <- dimnames(corrTrain)[[2]]
                         tomTrainAll
                       })

  # results for clustering
  # note that the u_cluster_similarity returns the PCs but you dont need this info
  # because it is calculated in the clust_fun fitting function
  # we just need to provide the clust_fun function the group membership for all the data
  res <- u_cluster_similarity(x = similarity,
                              expr = xtrain,
                              exprTest = xtest,
                              minimum_cluster_size = minimum_cluster_size,
                              distanceMethod = measure_distance, ...)


  #############################################################################
  #               ECLUST CLUSTERS                                             #
  #############################################################################

  message(paste("Calculating number of environment clusters based on",eclust_distance))

  # clusters based on eclust_distance
  similarityEclust <- switch(eclust_distance,
                             corr = corrTrain,
                             corr0 = WGCNA::cor(xtrainE0),
                             corr1 = WGCNA::cor(xtrainE1),
                             diffcorr = {
                               corr0 <- WGCNA::cor(xtrainE0)
                               corr1 <- WGCNA::cor(xtrainE1)
                               abs(corr1 - corr0)
                             },
                             difftom = {
                               tomTrainE0 <- WGCNA::TOMsimilarityFromExpr(xtrainE0)
                               tomTrainE1 <- WGCNA::TOMsimilarityFromExpr(xtrainE1)
                               tomTrainDiff <- abs(tomTrainE1 - tomTrainE0)
                               dimnames(tomTrainDiff)[[1]] <- dimnames(corrTrain)[[1]]
                               dimnames(tomTrainDiff)[[2]] <- dimnames(corrTrain)[[2]]
                               tomTrainDiff
                             },
                             tom0 = {
                               tomTrainE0 <- WGCNA::TOMsimilarityFromExpr(xtrainE0)
                               dimnames(tomTrainE0)[[1]] <- dimnames(corrTrain)[[1]]
                               dimnames(tomTrainE0)[[2]] <- dimnames(corrTrain)[[2]]
                               tomTrainE0
                             },
                             tom1 = {
                               tomTrainE1 <- WGCNA::TOMsimilarityFromExpr(xtrainE1)
                               dimnames(tomTrainE1)[[1]] <- dimnames(corrTrain)[[1]]
                               dimnames(tomTrainE1)[[2]] <- dimnames(corrTrain)[[2]]
                               tomTrainE1
                             },
                             tom = {
                               tomTrainAll <- WGCNA::TOMsimilarityFromExpr(xtrain)
                               dimnames(tomTrainAll)[[1]] <- dimnames(corrTrain)[[1]]
                               dimnames(tomTrainAll)[[2]] <- dimnames(corrTrain)[[2]]
                               tomTrainAll
                             },
                             fisherScore = {
                               n0 <- nrow(xtrainE0)
                               n1 <- nrow(xtrainE1)
                               corr0 <- WGCNA::cor(xtrainE0)
                               corr1 <- WGCNA::cor(xtrainE1)
                               u_fisherZ(n0 = n0, cor0 = corr0,
                                       n1 = n1, cor1 = corr1)
                             })

  resEclust <- if (eclust_distance %in% c("diffcorr","difftom","fisherScore")) {
    u_cluster_similarity(x = similarityEclust,
                         expr = xtrain,
                         minimum_cluster_size = minimum_cluster_size,
                         exprTest = xtest, ...)
  } else {
    u_cluster_similarity(x = similarityEclust,
                         expr = xtrain,
                         exprTest = xtest,
                         minimum_cluster_size = minimum_cluster_size,
                         distanceMethod = measure_distance, ...)
  }

  # we need to combine the cluster information here
  # this is based on cluster_distance only
  clustersAll <- data.table::copy(res$clusters)
  n_clusters_All <- res$pcInfo$nclusters

  message(sprintf("There are %d clusters derived from the %s similarity matrix",
                  n_clusters_All, cluster_distance))

  # this is based on eclust_distance only
  n_clusters_Eclust <- resEclust$pcInfo$nclusters
  clustersEclust <- data.table::copy(resEclust$clusters)

  message(sprintf("There are %d clusters derived from the %s environment similarity matrix",
                  n_clusters_Eclust, eclust_distance))

  # this is based on both
  n_clusters_Addon <- n_clusters_All + n_clusters_Eclust

  message(sprintf("There are a total of %d clusters derived from the %s
                  similarity matrix and the %s environment similarity matrix",
                  n_clusters_Addon,cluster_distance,eclust_distance))

  # check if any of the cluster numbers in clustersEclust are 0
  # if there are, then add n_clusters+1 to each module number in
  # clustersEclust, else just add n_clusters. this is to control for the
  # situation where there are some clusters numbers of 0 which would cause
  # identical cluster numbers in the clusters and clustersEclust data
  if (clustersEclust[,any(cluster==0)]) {
    clustersEclust[,cluster := cluster + n_clusters_All + 1 ]
  } else {
    clustersEclust[,cluster := cluster + n_clusters_All ]
  }

  # this contains the clusters from the cluster_distance (e.g. TOM matrix)
  # and the clusters from the eclust_distance (e.g. diffTOM)
  clustersAddon <- data.table::rbindlist(list(clustersAll, clustersEclust))
  # clustersAddon[, table(cluster, module)]

  # these are only derived on the main effects genes.. E is only included in the model
  # this is the clusters based on tom and difftom
  PC_and_avg_Addon <- u_extract_summary(x_train = xtrain[,clustersAddon$gene],
                                colors = clustersAddon$cluster,
                                x_test = xtest[,clustersAddon$gene],
                                y_train = ytrain,
                                y_test = ytest, nPC = args$nPC)

  # this is the clusters based on tom only
  PC_and_avg_All <- u_extract_summary(x_train = xtrain[,clustersAll$gene],
                              colors = clustersAll$cluster,
                              x_test = xtest[,clustersAll$gene],
                              y_train = ytrain,
                              y_test = ytest, nPC = args$nPC)

  # n.clusters <- PC_and_avg_Addon$nclusters

  # this contains either the averages or PCs for each module in a data.frame
  #   clust_data_Addon <- switch(summary,
  #                        avg = PC_and_avg_Addon$averageExpr,
  #                        pc = PC_and_avg_Addon$PC)
  #
  #   clust_data_All <- switch(summary,
  #                            avg = PC_and_avg_All$averageExpr,
  #                            pc = PC_and_avg_All$PC)


  results_final <- list(clustersAddon = PC_and_avg_Addon,
                        clustersAll = PC_and_avg_All,
                        etrain = etrain,
                        cluster_distance_similarity = similarity,
                        eclust_distance_similarity = similarityEclust,
                        clustersAddonMembership = clustersAddon,
                        clustersAllMembership = clustersAll,
                        clustersEclustMembership = clustersEclust)

  class(results_final) <- "eclust"

  return(results_final)
}





#' Prepare data for regression routines
#'
#' @description This function will output the appropriate X and Y matrices in
#'   the right format for regression packages such as \code{mgcv}, \code{caret}
#'   and \code{glmnet}
#' @param data the data frame which contains the response, exposure, and genes
#'   or cpgs or covariates. the columns should be labelled.
#' @param response the column name of the response in the \code{data} argument
#' @param exposure the column name of the exposure in the \code{data} argument
#' @param probe_names the column names of the genes, or cpg sites or covariates
#' @return a list of length 5: \describe{\item{X}{the X matrix}\item{Y}{the
#'   response vector}\item{E}{the exposure vector}\item{main_effect_names}{the
#'   names of the main effects including the
#'   exposure}\item{interaction_names}{the names of the interaction effects}}
#' @examples
#' data("tcgaov")
#' tcgaov[1:5,1:6, with = FALSE]
#' Y <- log(tcgaov[["OS"]])
#' E <- tcgaov[["E"]]
#' genes <- as.matrix(tcgaov[,-c("OS","rn","subtype","E","status"),with = FALSE])
#' trainIndex <- drop(caret::createDataPartition(Y, p = 0.5, list = FALSE, times = 1))
#' testIndex <- setdiff(seq_len(length(Y)),trainIndex)
#'
#' \dontrun{
#' cluster_res <- r_cluster_data(data = genes,
#'                               response = Y,
#'                               exposure = E,
#'                               train_index = trainIndex,
#'                               test_index = testIndex,
#'                               cluster_distance = "tom",
#'                               eclust_distance = "difftom",
#'                               measure_distance = "euclidean",
#'                               clustMethod = "hclust",
#'                               cutMethod = "dynamic",
#'                               method = "average",
#'                               nPC = 1,
#'                               minimum_cluster_size = 50)
#'
#' pc_eclust_interaction <- r_prepare_data(data = cbind(cluster_res$clustersAddon$PC,
#'                                                      survival = Y[trainIndex],
#'                                                      subtype = E[trainIndex]),
#'                                         response = "survival", exposure = "subtype")
#' names(pc_eclust_interaction)
#' dim(pc_eclust_interaction$X)
#' pc_eclust_interaction$main_effect_names
#' pc_eclust_interaction$interaction_names
#' }
#' @export

r_prepare_data <- function(data, response = "Y", exposure = "E", probe_names) {

  # data = cbind(pcTrain, Y = Y[trainIndex], E = E[trainIndex])


  # ===========================================================

  # Check for sensible dataset
  ## Make sure you have response, exposure.
  if (!(response %in% colnames(data))) stop(sprintf("response argument specified as %s but this column not found in 'data' data.frame", response))
  if (!(exposure %in% colnames(data))) stop(sprintf("exposure argument specified as %s but this column not found in 'data' data.frame", exposure))
  if (!missing(probe_names)) {
    if (!(probe_names %in% colnames(data))) stop(sprintf("probe_names argument specified as %s but this column not found in 'data' data.frame", probe_names))
  }

  # if missing main_effect_names, assume everything except response and exposure
  # are main effects
  if (missing(probe_names)) {
    probe_names <- setdiff(colnames(data), c(response, exposure))
  }

  # rename response to be Y and exposure to be E
  colnames(data)[which(colnames(data) == response)] <- "Y"
  colnames(data)[which(colnames(data) == exposure)] <- "E"

  x_mat <- stats::model.matrix(stats::as.formula(paste0("~(", paste0(probe_names, collapse="+"), ")*E - 1")), data = data)


  # reformulate(paste0("~(", paste0(colnames(pcTrain)[1:5], collapse="+"), ")*E"))


  interaction_names <- grep(":", colnames(x_mat), value = T)
  main_effect_names <- setdiff(colnames(x_mat), interaction_names)

  return(list(X = x_mat, Y = data[["Y"]], E = data[["E"]],
              main_effect_names = main_effect_names,
              interaction_names = interaction_names))
  # x_mat


}
