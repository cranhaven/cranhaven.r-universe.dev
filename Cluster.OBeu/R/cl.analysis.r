#' @title
#' Cluster analusis 
#' @description 
#' Clustering Analysis for OBEU datasets.
#' 
#' @usage cl.analysis(cl.data, cl_feature = NULL, amount = NULL, cl.aggregate = "sum",
#'cl.meth = NULL, clust.numb = NULL, dist = "euclidean", tojson = FALSE)
#'                      
#' @param cl.data The input data 
#' @param cl_feature The feature to be clustered (nominal variables)
#' @param amount The numeric variables
#' @param cl.aggregate Select a different aggregation in case of filtering the input data
#' @param cl.meth The clustering method algorithm
#' @param clust.numb The number of clusters
#' @param dist The distance metric
#' @param tojson If TRUE the results are returned in json format, default returns a list
#' 
#' @details There are different clustering models to be selected through an evaluation process. 
#' The user should define the cl_feature, cl.aggregate and amount parameters to form the structure of cluster data. 
#' The clustering algorithm, the number of clusters and the distance metric of the clustering model are set
#' to the best selection using internal and stability measures. 
#' The end user can also interact with the cluster analysis and these parameters by specifying the cl.method, cl.num and cl.dist parameters respectively.
#'  
#' @return The final returns are the parameters needed for visualizing the cluster data depending on the selected algorithm and the specification parameters, 
#' as long as some comparison measure matrices.
#' 
#' \itemize{
#' \item cluster.method - Label of the clustering algorithm
#' \item raw.data - Input data
#' \item data.pca - The principal components to visualize the input data
#' \item modelparam - The results of this parameter depend of the selected clustering model
#' \item compare - Clustering measures}
#' 
#' @author Kleanthis Koupidis, Jaroslav Kuchar
#' @examples
#' cl.analysis(city_data, cl.meth = "pam", clust.numb = 3)
#' @seealso \code{\link{cl.features}}, \code{\link[clValid]{clValid}}, \code{\link[cluster]{diana}}, \code{\link[cluster]{agnes}},
#' \code{\link[cluster]{pam}}, \code{\link[cluster]{clara}}, \code{\link[cluster]{fanny}}, \code{\link[mclust]{Mclust}} 
#' @rdname cl.analysis
#' 
#' @export
#' 

cl.analysis <- function(cl.data, cl_feature = NULL, amount = NULL, cl.aggregate = "sum",
                        cl.meth = NULL, clust.numb = NULL, dist = "euclidean", tojson = FALSE) {
  
  # Select clustering feature
  data <- cl.data
  clusterr.data <- cl.features(cl.data, features = cl_feature, amounts = amount, aggregate = cl.aggregate)
  cl.data <- nums(clusterr.data)
  
  if (ncol(nums(cl.data)) < 2) {
    stop ("The dimension (number of columns) of dataset must be at least 2 numeric variables.")
  }
  
  ## If method and number of clusters is not provided
  if (is.null(cl.meth) & 
      is.null(clust.numb)) {
    
    method_clvalid <- clValid::clValid(as.matrix(cl.data), 2:5,
                                       clMethods = c("hierarchical", "kmeans", "pam", "clara", "fanny", "model"),
                                       validation = c("internal", "stability"),
                                       metric = "euclidean", maxitems = nrow(cl.data))
    
    clust.numb <- cl.summary(method_clvalid)$nb.clust
    cl.meth <- cl.summary(method_clvalid)$method.cluster
  }
  
  ## If method is not provided
  if (is.null(cl.meth)) {
    
    method_clvalid <- clValid::clValid(as.matrix(cl.data), clust.numb,
                                       clMethods = c("hierarchical", "kmeans", "pam", "clara", "fanny", "model"),
                                       validation = c("internal", "stability"),
                                       metric = "euclidean",
                                       maxitems = nrow(cl.data))
    
    cl.meth <- cl.summary(method_clvalid)$method.cluster
  }
  
  ## If number of clusters is not provided
  if (is.null(clust.numb)) {
    
    method_clvalid <- clValid::clValid(as.matrix(cl.data), 2:5,
                                       clMethods = cl.meth,
                                       validation = c("internal", "stability"),
                                       metric = "euclidean",
                                       maxitems = nrow(cl.data))
    
    clust.numb <- cl.summary(method_clvalid)$nb.clust
  }
  
  ## Hierarchical
  if (cl.meth %in% c("hierarchical", "diana", "agnes")) {
    
    # hierarchical
    if (cl.meth == "hierarchical") {
      tree <- stats::hclust(stats::dist(cl.data), method = "ward.D2")
      
      # Diana (DIvisive ANAlysis Clustering)
    } else if (cl.meth == "diana") {
      tree <- cluster::diana(cl.data)
      
      # Agnes (Agglomerative Nesting- Hierarchical Clustering)
    } else if (cl.meth == "agnes") {
      tree <- cluster::agnes(cl.data, method = "ward")
    }  
    
    #Convert to dendrogram
    dendr <- stats::as.dendrogram(tree)
    tree2 <- data.tree::as.Node(dendr)
    
    ## Create Clusters
    create.clust <- dendextend::cutree(dendr, k = clust.numb)
    tree <- data.tree::ToListExplicit(tree2, unname = TRUE,  childrenName = "children")
    
    # Model Parameters
    modelparam <- list(
      cluster.method = cl.meth,
      raw.data = data,
      tree = tree, 
      list(clusters = create.clust))
    
    ## K-Means
  } else if (cl.meth == "kmeans") {
    
    kmeans <- kmeans(cl.data, clust.numb)
    
    #comparative parameters
    comp.parameters <- list(
      total.sumOfsquares = kmeans$totss,
      within.sumofsquares = kmeans$withinss,
      total.within.sumofsquares = kmeans$tot.withinss,
      between.sumofsquares = kmeans$betweenss,
      cluster.size = kmeans$size)
    
    # PCA
    data.pca <- stats::prcomp(cl.data, scale. = TRUE, center = TRUE)
    
    # model parameters
    modelparam <- list(
      cluster.method = cl.meth,
      raw.data = cl.data,
      data.pca = data.pca$x[,1:2],
      clusters = kmeans$cluster,
      cluster.centers = kmeans$centers,
      compare = comp.parameters)
    
    # ellipses + convex hulls
    clusterellipses <- ellipses(modelparam, data.pca)
    clusterconvex.hulls <- convex.hulls(modelparam, data.pca)
    
    ## model parameters
    modelparam <- utils::modifyList(modelparam, 
                                    list(clusterellipses = clusterellipses, 
                                         clusterconvex.hulls = clusterconvex.hulls))
    
    ## Pam (Partitioning Around Medoids)
  } else if (cl.meth == "pam") {
    pam <- cluster::pam( cl.data, clust.numb, metric = "euclidean" )
    
    # comparative parameters
    comp.parameters <- list(
      cluster.size = pam$clusinfo[,"size"],
      cluster.max_diss = pam$clusinfo[,"max_diss"],
      cluster.av_diss = pam$clusinfo[,"av_diss"],
      cluster.diameter = pam$clusinfo[,"diameter"],
      cluster.separation = pam$clusinfo[,"separation"],
      silhouette.info = pam$silinfo)
    
    # PCA
    data.pca <- stats::prcomp(cl.data, scale. = TRUE, center = TRUE)
    
    #model parameters
    modelparam <- list(
      cluster.method = cl.meth,
      raw.data = cl.data,
      data.pca = data.pca$x[,1:2],
      medoids = pam$medoids,
      medoids.id = pam$id.med,
      clusters = pam$clustering,
      compare = comp.parameters)
    ### ellipses + convex hulls
    clusterellipses <- ellipses(modelparam, data.pca)
    clusterconvex.hulls <- convex.hulls(modelparam, data.pca)
    
    ## model parameters
    modelparam <- utils::modifyList(modelparam, list(clusterellipses = clusterellipses, clusterconvex.hulls = clusterconvex.hulls ) )
    
    ## Clara (Clustering Large Applications)
  } else if (cl.meth == "clara") {
    
    clara <- cluster::clara( cl.data, clust.numb, metric = "euclidean", samples=100)
    
    # comparative parameters
    comp.parameters <- list( 
      cluster.size = clara$clusinfo[,"size"],
      cluster.max_diss = clara$clusinfo[,"max_diss"],
      cluster.av_diss = clara$clusinfo[,"av_diss"],
      cluster.diameter = clara$clusinfo[,"isolation"],
      silhouette.info = clara$silinfo)
    
    # PCA
    data.pca <- stats::prcomp(cl.data, scale. = TRUE, center = TRUE)
    
    # model parameters
    modelparam <- list(
      cluster.method = cl.meth,
      raw.data = cl.data,
      data.pca = data.pca$x[,1:2],
      medoids = clara$medoids,
      medoids.id = clara$i.med,
      clusters = clara$clustering,
      compare = comp.parameters)
    
    ## ellipses + convex hulls
    clusterellipses <- ellipses(modelparam, data.pca)
    clusterconvex.hulls <- convex.hulls(modelparam, data.pca)
    
    ## model parameters
    modelparam <- utils::modifyList(
      modelparam, 
      list(clusterellipses = clusterellipses, clusterconvex.hulls = clusterconvex.hulls))
    
    ## Fanny (Fuzzy Analysis Clustering)
  } else if (cl.meth == "fanny") {
    
    fanny <- cluster::fanny(cl.data, clust.numb, metric = "euclidean")
    
    # comparative parameters
    comp.parameters <- list( 
      membership = fanny$membership,
      coeff = fanny$coeff,
      memb.exp = fanny$memb.exp,
      fanny$k.crisp,
      fanny$objective,
      fanny$convergence,
      fanny$silinfo)
    
    # PCA
    data.pca <- stats::prcomp(cl.data, scale. = TRUE, center = TRUE)
    
    # model parameters
    modelparam <- list( 
      cluster.method = cl.meth,
      raw.data = cl.data,
      data.pca = data.pca$x[ ,1:2],
      clusters = fanny$clustering,
      compare = comp.parameters)
    
    ## ellipses + convex hulls
    clusterellipses <- ellipses(modelparam, data.pca)
    clusterconvex.hulls <- convex.hulls(modelparam, data.pca)
    
    ## model parameters
    modelparam <- utils::modifyList(
      modelparam, 
      list(clusterellipses = clusterellipses, clusterconvex.hulls = clusterconvex.hulls))
    
    ## Model Based Clustering
  } else if (cl.meth == "model") {
    # mclust = c(cl.data, clust.numb)
    # data.list = utils::combn(cl.data, 2, simplify = FALSE)
    # data.list.colnames = lapply(data.list, colnames)
    mclust <- mclust::Mclust(cl.data, G = clust.numb)
    
    # comparative parameters
    comp.parameters <- list(
      model.name = mclust$modelName,
      observations = mclust$n,
      data.dimension = mclust$d,
      clust.numb = mclust$G,
      all.Bics = data.frame(matrix(mclust$BIC, dimnames = list(c(colnames(mclust$BIC)), "bic"))),
      optimal.bic = mclust$bic,
      optimal.loglik = mclust$loglik,
      numb.estimated.parameters = mclust$df,
      hypervolume.parameter = mclust$hypvol,
      mixing.proportion = mclust$parameters$pro,
      mean.component = mclust$parameters$mean,
      variance.components = mclust$parameters$variance,
      class.probs = mclust$z,
      uncertainty = mclust$uncertainty)
    
    # model parameters
    modelparam <- list( 
      cluster.method = cl.meth,
      raw.data = data,
      data.pca = mclust$data,
      #data.list = data.list,
      #data.list.colnames = data.list.colnames,
      clusters = mclust$classification,
      compare = comp.parameters)
  }
  
  if (tojson == TRUE) {
    modelparam <- jsonlite::toJSON(modelparam)
  }
  return(modelparam)
}
