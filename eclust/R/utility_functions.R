#' Cluster similarity matrix
#'
#' @description Return cluster membership of each predictor. This function is
#'   called internally by the \code{\link{s_generate_data}} and
#'   \code{\link{s_generate_data_mars}} functions. Is also used by the
#'   \code{r_clust} function for real data analysis.
#'
#' @param x similarity matrix. must have non-NULL dimnames i.e., the rows and
#'   columns should be labelled, e.g. "Gene1, Gene2, ..."
#' @param expr gene expression data (training set). rows are people, columns are
#'   genes
#' @param exprTest gene expression test set. If using real data, and you dont
#'   have enough samples for a test set then just supply the same data supplied
#'   to the \code{expr} argument
#' @param distanceMethod  one of "euclidean","maximum","manhattan", "canberra",
#'   "binary","minkowski" to be passed to \code{\link[stats]{dist}} function. If
#'   missing, then this function will take 1-x as the dissimilarity measure.
#'   This functionality is for diffCorr,diffTOM, fisherScore matrices which need
#'   to be converted to a distance type matrix.
#' @param clustMethod Cluster the data using hierarchical clustering or
#'   prototype clustering. Defaults \code{clustMethod="hclust"}. Other option is
#'   \code{\link[protoclust]{protoclust}}, however this package must be
#'   installed before proceeding with this option
#' @param cutMethod what method to use to cut the dendrogram. \code{'dynamic'}
#'   refers to \code{\link[dynamicTreeCut]{cutreeDynamicTree}} library.
#'   \code{'gap'} is Tibshirani's gap statistic \code{\link[cluster]{clusGap}}
#'   using the \code{'Tibs2001SEmax'} rule. \code{'fixed'} is a fixed number
#'   specified by the \code{nClusters} argument
#' @param nClusters number of clusters. Only used if \code{cutMethod = fixed}
#' @param K.max the maximum number of clusters to consider, must be at least
#'   two. Only used if \code{cutMethod='gap'}
#' @param B integer, number of Monte Carlo (“bootstrap”) samples. Only used if
#'   \code{cutMethod='gap'}
#' @param method the agglomeration method to be used. This should be (an
#'   unambiguous abbreviation of) one of "ward.D", "ward.D2", "single",
#'   "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC)
#'   or "centroid" (= UPGMC).
#' @param nPC number of principal components. Can be 1 or 2.
#' @param minimum_cluster_size The minimum cluster size. Only applicable if
#'   \code{cutMethod='dynamic'}. This argument is passed to the
#'   \code{\link[dynamicTreeCut]{cutreeDynamic}} function. Default is 50.
#' @return a list of length 2: \describe{\item{clusters}{a p x 3 data.frame or
#'   data.table which give the cluster membership of each gene, where p is the
#'   number of genes. The first column is the gene name, the second column is
#'   the cluster number (numeric) and the third column is the cluster membership
#'   as a character vector of color names (these will match up exactly with the
#'   cluster number)}\item{pcInfo}{a list of length
#'   9:\describe{\item{eigengenes}{a list of the eigengenes i.e. the 1st (and
#'   2nd if nPC=2) principal component of each module}\item{averageExpr}{a
#'   data.frame of the average expression for each module for the training
#'   set}\item{averageExprTest}{a data.frame of the average expression for each
#'   module for the test set}\item{varExplained}{percentage of variance
#'   explained by each 1st (and 2nd if nPC=2) principal component of each
#'   module}\item{validColors}{cluster membership of each gene}\item{PC}{a
#'   data.frame of the 1st (and 2nd if nPC=2) PC for each module for the
#'   training set}\item{PCTest}{a data.frame of the 1st (and 2nd if nPC=2) PC
#'   for each module for the test set}\item{prcompObj}{the \code{prcomp}
#'   object}\item{nclusters}{a numeric value for the total number of
#'   clusters}}}}
#'
#' @examples
#' data("simdata")
#' X = simdata[,c(-1,-2)]
#' train_index <- sample(1:nrow(simdata),100)
#'
#' cluster_results <- u_cluster_similarity(x = cor(X),
#'                                         expr = X[train_index,],
#'                                         exprTest = X[-train_index,],
#'                                         distanceMethod = "euclidean",
#'                                         clustMethod = "hclust",
#'                                         cutMethod = "dynamic",
#'                                         method = "average", nPC = 2,
#'                                         minimum_cluster_size = 75)
#'
#' cluster_results$clusters[, table(module)]
#' names(cluster_results$pcInfo)
#' cluster_results$pcInfo$nclusters
#' @export
u_cluster_similarity <- function(x,
                                 expr,
                                 exprTest,
                                 distanceMethod,
                                 clustMethod = c("hclust", "protoclust"),
                                 cutMethod = c("dynamic","gap", "fixed"),
                                 nClusters,
                                 method = c("complete", "average", "ward.D2",
                                            "single", "ward.D", "mcquitty",
                                            "median", "centroid"),
                                 K.max = 10, B = 50, nPC, minimum_cluster_size = 50) {

  # x = corrX ; expr = X
  # exprTest = X[sample(seq_len(nrow(X)),nrow(X), replace = TRUE ),]
  # dim(X) ; dim(expr) ; dim(exprTest)
  # clustMetho .0d = c("hclust")
  # cutMethod = c("dynamic")
  # nClusters = 6
  # method = c("complete")
  # summary = c("pca")
  # K.max = 10; B = 50
  # distance = as.dist(1 - x)
  module = cluster = NULL
  geneNames <- dimnames(x)[[1]]
  p <- nrow(x)
  method <- match.arg(method)
  cutMethod <- match.arg(cutMethod)
  clustMethod <- match.arg(clustMethod)


  if (clustMethod=="protoclust") {
    if (!requireNamespace("protoclust", quietly = TRUE)) {
      stop("protoclust package needed for this function to work. Please install it.",
           call. = FALSE)
    }
  }

  if (cutMethod=="gap") {
    if (!requireNamespace("cluster", quietly = TRUE)) {
      stop("cluster package needed for this function to work. Please install it.",
           call. = FALSE)
    }
  }


  distance <- if (missing(distanceMethod)) {
    stats::as.dist(1 - x)
  } else stats::dist(x = x, method = distanceMethod)

  hc <- switch(clustMethod,
               hclust = {
                 stats::hclust(distance, method = method)
               },
               protoclust = {
                 protoclust::protoclust(distance)
               }
  )

  #plot(hc)
  # create cluster function used if Gap statistic is requested
  # its as.dist(x) here because I am passing the
  # 1-x matrix to the cluster::clusGap function
  if (cutMethod == "gap") {

    FUNcluster <- if (missing(distanceMethod)) {
      switch(clustMethod,
             hclust = {
               function(xMat,k) list(cluster = {
                 as.numeric(
                   stats::cutree(
                     stats::hclust(stats::as.dist(xMat), method = method), k = k
                   )
                 )
               })
             },
             protoclust = {
               function(xMat,k) list(cluster = {
                 as.numeric(protoclust::protocut(protoclust::protoclust(stats::as.dist(xMat)),
                                                 k = k)$cl)})
             }
      )
    } else {
      switch(clustMethod,
             hclust = {
               function(xMat,k) list(cluster = {
                 as.numeric(stats::cutree(stats::hclust(stats::dist(xMat, method = distanceMethod),
                                          method = method), k = k))})
             },
             protoclust = {
               function(xMat,k) list(cluster = {
                 as.numeric(protoclust::protocut(
                   protoclust::protoclust(stats::dist(xMat, method = distanceMethod)),
                   k = k)$cl)})
             }
      )
    }
    #return(FUNcluster)
  }


  clustAssignment <- switch(cutMethod,
                            dynamic = {
                              if (clustMethod == "hclust") {
                                dynamicTreeCut::cutreeDynamic(
                                  hc,
                                  method = "hybrid",
                                  distM = as.matrix(distance),
                                  #cutHeight = 0.995,
                                  deepSplit = 1,
                                  pamRespectsDendro = T,
                                  minClusterSize = minimum_cluster_size)
                              } else {
                                hcMod <- hc
                                class(hcMod) <- "hclust"
                                dynamicTreeCut::cutreeDynamic(
                                  hcMod,
                                  distM = as.matrix(distance),
                                  #cutHeight = 0.995,
                                  deepSplit = 1,
                                  method = "hybrid",
                                  pamRespectsDendro = T,
                                  minClusterSize = minimum_cluster_size)
                              }
                            },
                            gap = {
                              if (clustMethod == "hclust") {
                                gapResult <- cluster::clusGap(1 - x,
                                                              FUNcluster = FUNcluster,
                                                              K.max = K.max,
                                                              B = B)
                                nClustGap <- cluster::maxSE(f = gapResult$Tab[, "gap"],
                                                            SE.f = gapResult$Tab[, "SE.sim"],
                                                            method = "Tibs2001SEmax",
                                                            SE.factor = 1)
                                stats::cutree(hc, nClustGap)

                              } else {
                                gapResult <- cluster::clusGap(1 - x,
                                                              FUNcluster = FUNcluster,
                                                              K.max = K.max,
                                                              B = B)
                                nClustGap <- cluster::maxSE(f = gapResult$Tab[, "gap"],
                                                            SE.f = gapResult$Tab[, "SE.sim"],
                                                            method = "Tibs2001SEmax",
                                                            SE.factor = 1)
                                protoclust::protocut(hc, k = nClustGap)[["cl"]]
                              }
                            },
                            fixed = {
                              if (clustMethod == "hclust") {
                                stats::cutree(hc, nClusters)
                              } else protoclust::protocut(hc, k = nClusters)[["cl"]]
                            }
  )

  # check if all cluster groups are 0 which means no cluster
  # assignment and everyone is in their own group
  # plot(clustAssignment)
  clusters <- data.table(gene = geneNames,
                         cluster = if (all(clustAssignment == 0))
                           1:p else clustAssignment)
  #setkey(clusters, "cluster")

  # convert cluster numbers to colors which define modules
  clusters[, module := WGCNA::labels2colors(cluster)]
  clusters[, table(cluster,module)]


  # note that the align argument acts as follows if equal to "along average"
  # which is the default: it take the correlation between the average expression
  # in a module and the 1st eigenvector in a module and checks if its less
  # than 0, if its less than 0, then the moduleEigengenes function multiplies
  # the 1st eigenvector by -1, else it returns the unmodified 1st eigenvector
  # note that moduleEigengenes function returns the 1st eigenvector which is
  # equivalent to the rotation returned by prcomp, and what is used in
  # predict.prcomp to calculate the actual PCs.
  # to calculate PC's the following are all equivalent:
  # all.equal((expr %*% prcomp.object$rotation)[,1],
  # predict(prcomp.object)[,1],prcomp.object$x[,1])
  #
  # these are equivalent
  # p <- WGCNA::moduleEigengenes(expr = expr[, clusters$gene],
  #                              colors = clusters$module,
  #                              align = "",
  #                              scale = FALSE)
  # l <- prcomp(t(expr[, which(clusters$module %in% "blue")]), scale. = FALSE,
  # center = FALSE)
  #
  # plot(l$rotation[,1,drop=F],p$eigengenes[,"MEblue"])

  # this plots the eigenvector against the average expression
  # to show the effect of the "along average" argument
  # cbind(pp$PC,pp$averageExpr) %>%
  #   mutate(id = 1:n) %>%
  #   gather(type, value, -id) %>%
  #   separate(type, c("type","module")) %>%
  #   spread(type,value) %>%
  #   magrittr::set_colnames(c("id","module","average", "PC")) %>%
  #   ggplot(.,aes(x = average, y = PC)) + geom_point() + facet_grid(~module) +
  #   theme_bw()

  pp <- u_extract_summary(x_train = expr[, clusters$gene],
                  x_test = exprTest[, clusters$gene],
                  colors = clusters$module,
                  scale = TRUE, nPC = nPC)

  # clusters
  # pp %>% names
  # pp$PCTest
  #
  # pp$varExplained
  # pp$averageExpr
  # pp$eigengenes
  # pp$PC


  return(list(clusters = clusters, pcInfo = pp))

}




#' Calculate Fisher's Z Transformation for Correlations
#'
#' @param n0 number of unexposed subjects
#' @param cor0 correlation matrix of unexposed covariate values. Should be
#'   dimension pxp
#' @param n1 number of exposed subjects
#' @param cor1 correlation matrix of exposed covariate values. Should be
#'   dimension pxp
#'
#' @description Calculate Fisher's Z transformation for correlations. This can
#'   be used as an alternative measure of similarity. Used in the
#'   \code{s_generate_data} function
#' @examples
#' data("simdata")
#'
#' X = simdata[,c(-1,-2)]
#' fisherScore <- u_fisherZ(n0 = 100, cor0 = cor(X[1:50,]),
#'                          n1 = 100, cor1 = cor(X[51:100,]))
#'
#' dim(fisherScore)
#'
#' fisherScore[1:5,1:5]
#' @return a pxp matrix of Fisher's Z transformation of correlations
#' @references \url{https://en.wikipedia.org/wiki/Fisher_transformation}
#' @export
u_fisherZ <- function(n0, cor0, n1, cor1) {

  # n0 = 50
  # n1 = 50
  # cor0 = corrX0
  # cor1 = corrX1

  # by default this doesnt include the diagonal
  # this collapses the correlation matrix by columns
  ccc0 <- as.vector(cor0[lower.tri(cor0)])
  ccc1 <- as.vector(cor1[lower.tri(cor1)])

  p <- nrow(cor1)

  # number of Z statistics to calculate (p choose 2)
  geneNames <- rownames(cor1)

  zstat <- fisherTransform(n0, ccc0, n1, ccc1)$diff

  # convert vector to symmetric matrix
  zMat <- diag(p)
  zMat[lower.tri(zMat)] <- zstat
  zMat <- zMat + t(zMat) - diag(diag(zMat))
  dimnames(zMat) <- list(geneNames,geneNames)
  class(zMat) <- c("similarity", class(zMat))
  return(zMat)
}


#' @note \code{fisherTransform} is called internally by \code{u_fisherZ} function
#' @param r1 correlation for unexposed
#' @param r2 correlation for exposed
#' @param n_1 number of unexposed subjects
#' @param n_2 number of exposed subjects
#' @inheritParams u_fisherZ
#' @rdname u_fisherZ
fisherTransform <- function (n_1, r1, n_2, r2) {
  num1a <- which(r1 >= 0.99)
  num2a <- which(r2 >= 0.99)
  r1[num1a] <- 0.99
  r2[num2a] <- 0.99
  num1b <- which(r1 <= -0.99)
  num2b <- which(r2 <= -0.99)
  r1[num1b] <- -0.99
  r2[num2b] <- -0.99
  # atanh (inverse hyperbolic tangent) simplifies to
  # 0.5 * log(1+r)/log(1-r) , for r < 1
  z1 <- atanh(r1)
  z2 <- atanh(r2)
  dz <- (z1 - z2)/sqrt(1/(n_1 - 3) + (1/(n_2 - 3)))
  pv <- 2 * (1 - stats::pnorm(abs(dz)))
  return(list(diff = dz, pval = pv))
}



"%ni%" <- Negate("%in%")




#' Calculates cluster summaries
#'
#' @description This is a modified version of
#'   \code{\link[WGCNA]{moduleEigengenes}}. It can extract (1st and 2nd
#'   principal component) of modules in a given single dataset. It can also
#'   return the average, the variance explained This function is more flexible
#'   and the nPC argument is used. currently only nPC = 1 and nPC = 2 are
#'   supported
#' @param x_train Training data for a single set in the form of a data frame
#'   where rows are samples and columns are genes (probes, cpgs, covariates).
#' @param colors A vector of the same length as the number of probes in expr,
#'   giving module color for all probes (genes). Color "grey" is reserved for
#'   unassigned genes.
#' @param x_test Test set in the form of a data frame where rows are samples and
#'   columns are genes (probes, cpgs, covariates).
#' @param y_train Training response numeric vector
#' @param y_test Test response numeric vector
#' @param impute If TRUE, expression data will be checked for the presence of NA
#'   entries and if the latter are present, numerical data will be imputed,
#'   using function impute.knn and probes from the same module as the missing
#'   datum. The function impute.knn uses a fixed random seed giving repeatable
#'   results.
#' @param nPC Number of principal components and variance explained entries to
#'   be calculated. Note that only 1 or 2 is possible.
#' @param excludeGrey Should the improper module consisting of 'grey' genes be
#'   excluded from the eigengenes?
#' @param grey Value of colors designating the improper module. Note that if
#'   colors is a factor of numbers, the default value will be incorrect.
#' @param subHubs Controls whether hub genes should be substituted for missing
#'   eigengenes. If TRUE, each missing eigengene (i.e., eigengene whose
#'   calculation failed and the error was trapped) will be replaced by a
#'   weighted average of the most connected hub genes in the corresponding
#'   module. If this calculation fails, or if subHubs==FALSE, the value of
#'   trapErrors will determine whether the offending module will be removed or
#'   whether the function will issue an error and stop.
#' @param trapErrors Controls handling of errors from that may arise when there
#'   are too many NA entries in expression data. If TRUE, errors from calling
#'   these functions will be trapped without abnormal exit. If FALSE, errors
#'   will cause the function to stop. Note, however, that subHubs takes
#'   precedence in the sense that if subHubs==TRUE and trapErrors==FALSE, an
#'   error will be issued only if both the principal component and the hubgene
#'   calculations have failed.
#' @param returnValidOnly logical; controls whether the returned data frame of
#'   module eigengenes contains columns corresponding only to modules whose
#'   eigengenes or hub genes could be calculated correctly (TRUE), or whether
#'   the data frame should have columns for each of the input color labels
#'   (FALSE).
#' @param softPower The power used in soft-thresholding the adjacency matrix.
#'   Only used when the hubgene approximation is necessary because the principal
#'   component calculation failed. It must be non-negative. The default value
#'   should only be changed if there is a clear indication that it leads to
#'   incorrect results.
#' @param scale logical; can be used to turn off scaling of the expression data
#'   before calculating the singular value decomposition. The scaling should
#'   only be turned off if the data has been scaled previously, in which case
#'   the function can run a bit faster. Note however that the function first
#'   imputes, then scales the expression data in each module. If the expression
#'   contain missing data, scaling outside of the function and letting the
#'   function impute missing data may lead to slightly different results than if
#'   the data is scaled within the function.
#' @param verbose Controls verbosity of printed progress messages. 0 means
#'   silent, up to (about) 5 the verbosity gradually increases.
#' @param indent A single non-negative integer controlling indentation of
#'   printed messages. 0 means no indentation, each unit above that adds two
#'   spaces.
#' @details This function is called internally by the
#'   \code{\link{u_cluster_similarity}} function
#'
#' @return A list with the following components:
#'   \describe{\item{eigengenes}{Module eigengenes in a dataframe, with each
#'   column corresponding to one eigengene}\item{averageExpr}{the average
#'   expression per module in the training set}\item{averageExprTest}{the
#'   average expression per module in the training set}\item{varExplained}{The
#'   variance explained by the first PC in each module}\item{validColors}{A copy
#'   of the input colors with entries corresponding to invalid modules set to
#'   grey if given, otherwise 0 if colors is numeric and "grey"
#'   otherwise.}\item{PC}{The 1st or 1st and 2nd PC from each module in the
#'   training set}\item{PCTest}{The 1st or 1st and 2nd PC from each module in
#'   the test set}\item{prcompObj}{The \code{prcomp} object returned by
#'   \code{\link[stats]{prcomp}}}\item{nclusters}{the number of modules
#'   (clusters)}}
#' @export
#' @examples
#' \dontrun{
#' #see u_cluster_similarity for examples
#' }
#' @references Zhang, B. and Horvath, S. (2005), "A General Framework for
#'   Weighted Gene Co-Expression Network Analysis", Statistical Applications in
#'   Genetics and Molecular Biology: Vol. 4: No. 1, Article 17
u_extract_summary <- function(x_train,
                              colors,
                              x_test,
                              y_train,
                              y_test,
                              impute = TRUE,
                              nPC,
                              excludeGrey = FALSE,
                              grey = if (is.numeric(colors)) 0 else "grey",
                              subHubs = TRUE, trapErrors = FALSE,
                              returnValidOnly = trapErrors, softPower = 6, scale = TRUE,
                              verbose = 0, indent = 0) {


  # x_train = result[["X_train"]] ; x_test = result[["X_test"]];
  # x_train_mod <- x_train %>% as.data.frame
  # x_test_mod = x_test %>% as.data.frame
  # gene_groups = result[["clustersAll"]]
  # x_train = x_train_mod[,gene_groups$gene];
  # colors = gene_groups$cluster;
  # x_test = x_test_mod[,gene_groups$gene]
  # impute = TRUE; nPC = 2; align = "along average";
  # excludeGrey = FALSE; grey = if (is.numeric(colors)) 0 else "grey";
  # subHubs = TRUE; trapErrors = FALSE; returnValidOnly = trapErrors;
  # softPower = 6; scale = TRUE; verbose = 0; indent = 0;
  spaces <- dynamicTreeCut::indentSpaces(indent)

  if (is.null(x_train)) {
    stop("moduleEigengenes: Error: x_train is NULL. ")
  }
  if (is.null(colors)) {
    stop("moduleEigengenes: Error: colors is NULL. ")
  }
  if (is.null(dim(x_train)) || length(dim(x_train)) != 2)
    stop("moduleEigengenes: Error: x_train must be two-dimensional.")
  if (dim(x_train)[2] != length(colors))
    stop("moduleEigengenes: Error: ncol(x_train) and length(colors) must be equal (one color per gene).")
  if (is.factor(colors)) {
    nl = nlevels(colors)
    nlDrop = nlevels(colors[, drop = TRUE])
    if (nl > nlDrop)
      stop(paste("Argument 'colors' contains unused levels (empty modules). ",
                 "Use colors[, drop=TRUE] to get rid of them."))
  }

  # maxVarExplained = 10
  # if (nPC > maxVarExplained)
  #   warning(paste("Given nPC is too large. Will use value",
  #                 maxVarExplained))
  #
  # nVarExplained = min(nPC, maxVarExplained)

  modlevels = levels(factor(colors))

  if (excludeGrey)
    if (sum(as.character(modlevels) != as.character(grey)) >
        0) {
      modlevels = modlevels[as.character(modlevels) !=
                              as.character(grey)]
    } else {
      stop(paste("Color levels are empty. Possible reason: the only color is grey",
                 "and grey module is excluded from the calculation."))
    }

  # these are the loadings aka the first and second eigenvector for each module
  # length of these vectors will vary depending on the size of the module
  eigenVectors <- vector("list", nPC*length(modlevels))

  # these are the actual PC's aka the data %*% eigenvector
  # each column will be a n-dimensional vector.. i.e. a value for each person
  #  this will contain the first 2 PCs for each module
  PC <- data.frame(matrix(NA, nrow = dim(x_train)[[1]],
                          ncol = nPC*length(modlevels)))

  PCTest <- data.frame(matrix(NA, nrow = dim(x_test)[[1]],
                              ncol = nPC*length(modlevels)))

  #   PLS <- data.frame(matrix(NA, nrow = dim(x_train)[[1]],
  #                           ncol = nPC*length(modlevels)))
  #
  #   PLSTest <- data.frame(matrix(NA, nrow = dim(x_test)[[1]],
  #                               ncol = nPC*length(modlevels)))

  # list to store prcomp objects
  prcompObj <- vector("list", length(modlevels))

  # list to store pls objects
  # plsObj <- vector("list", length(modlevels))


  # this is the average expression in a module for each subject
  # so this is a n x length(modlevels) matrix
  averExpr <- data.frame(matrix(NA, nrow = dim(x_train)[[1]],
                                ncol = length(modlevels)))

  averExprTest <- data.frame(matrix(NA, nrow = dim(x_test)[[1]],
                                    ncol = length(modlevels)))

  varExpl <- vector("double", nPC*length(modlevels))

  # validMEs = rep(TRUE, length(modlevels))
  # validAEs = rep(FALSE, length(modlevels))

  # these are the means and sds used for subsequent predictions
  means = vector("list", length(modlevels))
  sds = vector("list", length(modlevels))

  # isPC = rep(TRUE, length(modlevels))
  # isHub = rep(FALSE, length(modlevels))
  validColors = colors

  # names(eigenVectors) = paste(moduleColor.getMEprefix(), modlevels,
  #                          sep = "")
  names(PC) = paste(rep(paste0("pc",seq_len(nPC)), length(modlevels)),
                    rep(modlevels, each = nPC), sep = "_")
  names(averExpr) = paste("avg", modlevels, sep = "")
  #   names(PCTest) = paste(rep(paste0("pc",seq_len(nPC)), length(modlevels)),
  #                         rep(modlevels, each = nPC), sep = "_")
  names(averExprTest) = paste("avg", modlevels, sep = "")

  for (i in seq_len(length(modlevels))) {
    # i=2
    if (verbose > 1)
      dynamicTreeCut::printFlush(paste("moduleEigengenes : Working on ME for module",
                       modlevels[i]))
    modulename = modlevels[i]
    restrict1 = as.character(colors) == as.character(modulename)
    if (verbose > 2)
      dynamicTreeCut::printFlush(paste(spaces, " ...", sum(restrict1),
                       "genes"))

    datModule <- as.matrix(x_train[, restrict1])
    datModuleTest <- as.matrix(x_test[, restrict1])

    # xy_train <- data.frame(Y = as.matrix(y_train), x_train[, restrict1])
    # xy_test <- data.frame(Y = as.matrix(y_test), x_test[, restrict1])

    # dim(datModule)
    # dim(t(datModule))
    # dim(x_train)

    # using prcomp first (need to use untransposed data!)
    prcompObj[[i]] <- stats::prcomp(datModule, center = scale, scale. = scale)

    # plsObj[[i]] <- pls::plsr(Y ~ ., ncomp = nPC, data = xy_train, validation = "CV")

    # plot(prcompObj[[i]])
    # View(stats:::prcomp.default)
    # prcompObj[[i]]$x %>% dim
    # prcompObj[[i]] %>% names
    # prcompObj[[i]]$rotation %>% dim

    if (nPC == 1) {

      eigenVectors[[i]] <- prcompObj[[i]]$rotation[,1, drop = F]
      averExpr[,i] <- rowMeans(datModule, na.rm = TRUE)
      averExprTest[,i] <- rowMeans(datModuleTest, na.rm = TRUE)

      varExpl[[i]] <- factoextra::get_eigenvalue(prcompObj[[i]])[1,"variance.percent"]

      # corAve = cor(averExpr[,i], prcompObj[[i]]$rotation[,1],
      #              use = "p")
      # if (!is.finite(corAve)) corAve = 0
      # if (corAve < 0) prcompObj[[i]]$rotation[,1] = -prcompObj[[i]]$rotation[,1]

      PC[, i] <- stats::predict(prcompObj[[i]])[,1]
      PCTest[, i] <- stats::predict(prcompObj[[i]], newdata = datModuleTest)[,1]

      # PLS[, i] <- stats::predict(plsObj[[i]], ncomp = nPC, type = "scores")
      # PLSTest[, i] <- stats::predict(plsObj[[i]], ncomp = nPC, type = "scores", newdata = xy_test)[,1]

    } else if (nPC == 2) {
      eigenVectors[[2*i-1]] <- prcompObj[[i]]$rotation[,1, drop = F]
      eigenVectors[[2*i]] <- prcompObj[[i]]$rotation[,2, drop = F]
      averExpr[,i] <- rowMeans(datModule, na.rm = TRUE)
      averExprTest[,i] <- rowMeans(datModuleTest, na.rm = TRUE)

      varExpl[[2*i-1]] <- factoextra::get_eigenvalue(prcompObj[[i]])[1,"variance.percent"]
      varExpl[[2*i]] <- factoextra::get_eigenvalue(prcompObj[[i]])[2,"variance.percent"]
      # corAve = cor(averExpr[,i], prcompObj[[i]]$rotation[,1],
      #              use = "p")
      # if (!is.finite(corAve)) corAve = 0
      # if (corAve < 0) prcompObj[[i]]$rotation[,1] = -prcompObj[[i]]$rotation[,1]

      PC[, 2*i-1] <- stats::predict(prcompObj[[i]])[,1]
      PC[, 2*i] <- stats::predict(prcompObj[[i]])[,2]
      # PCTest[, 2*i-1] <- stats::predict(prcompObj[[i]], newdata = datModuleTest)[,1]
      # PCTest[, 2*i] <- stats::predict(prcompObj[[i]], newdata = datModuleTest)[,2]

      # plot(PC[, i], prcompObj[[i]]$x[,1])
      #means[i] <- prcompObj[[i]]$center
      #sds[i] <- prcompObj[[i]]$scale
    }

  }

  list(eigengenes = eigenVectors, averageExpr = averExpr,
       averageExprTest = averExprTest,
       varExplained = varExpl, validColors = validColors,
       PC = PC, PCTest = PCTest, prcompObj = prcompObj,
       # PLS = PLS, PLSTest = PLSTest,
       nclusters = length(modlevels))
}


#' Get selected terms from an earth object
#'
#' @description function to extract the selected terms from an earth object
#' @param obj object of class \code{earth} returned by the
#'   \code{\link[earth]{earth}} function
#' @export
#' @return character vector of selected terms from the MARS model
#' @details called internally by the \code{\link{s_mars_separate}} and
#'   \code{\link{s_mars_clust}} functions
u_extract_selected_earth <- function(obj) {
  any1 <- function(x) any(x != 0) # like any but no warning if x is double
  names(which(apply(obj$dirs[obj$selected.terms, , drop=FALSE], 2, any1)))
}









