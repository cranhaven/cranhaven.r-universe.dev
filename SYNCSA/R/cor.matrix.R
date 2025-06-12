#' @title Function to obtain the correlation between two matrices and partial matrix
#' correlation between three matrices.
#'
#' @description The functions cor.matrix and cor.matrix.partial are similar the function
#' \code{\link{mantel}} and \code{\link{mantel.partial}}, although the significance
#' of the statistics is evaluated differently from Mantel. The functions pro.matrix
#' and pro.matrix.partial use symmetric Procrustes as a measure of concordance between
#' data sets. The function cor.mantel is similar to the function \code{\link{mantel}}, but
#' allows the use of a set of predefined permutation. For more details, see
#' \code{\link{syncsa}}.
#'
#' @details The null model is based on permutations in the matrix mx2, typically the
#' matrices B, U and Q, except in the function cor.mantel when the permutations
#' are done in one of distance matrix.
#'
#' Null model described by Pillar et al. (2009) and Pillar & Duarte (2010). For
#' more details on the matrices and the null model, see \code{\link{syncsa}}.
#'
#' @encoding UTF-8
#' @importFrom vegan vegdist
#' @importFrom parallel makeCluster parRapply stopCluster
#' @importFrom stats as.dist cor
#' @aliases cor.matrix cor.matrix.partial pro.matrix pro.matrix.partial cor.mantel cor.procrustes
#' @param mx1 Matrix that multiplied by mx2 results in the matrix x.
#' @param mx2 Matrix that when multiplied by mx1 results in the matrix x. See
#' `details` below.
#' @param x Matrix that will be correlated with the matrix y.
#' @param my1 Matrix that multiplied by my2 results in the matrix y.
#' @param my2 Matrix that when multiplied by my1 results in the matrix y. See
#' `details` below.
#' @param y Matrix that will be correlated with the matrix x.
#' @param mz1 Matrix that multiplied by mz2 results in the matrix z.
#' @param mz2 Matrix that when multiplied by mz1 results in the matrix z. See
#' `details` below.
#' @param z Matrix whose effect will be removed from the correlation between x
#' and y.
#' @param dist.x Dissimilarity matrices of class dist.
#' @param dist.y Dissimilarity matrices of class dist.
#' @param method Correlation method, as accepted by cor: "pearson", "spearman"
#' or "kendall".
#' @param dist Dissimilarity index, as accepted by vegdist: "manhattan",
#' "euclidean", "canberra", "bray", "kulczynski", "jaccard", "gower",
#' "altGower", "morisita", "horn", "mountford", "raup" , "binomial" or "chao".
#' @param permutations Number of permutations in assessing significance.
#' @param norm Logical argument (TRUE or FALSE) to specify if x is standardized
#' within variables (Default norm = FALSE).
#' @param norm.y Logical argument (TRUE or FALSE) to specify if y is standardized
#' within variables (Default norm = FALSE).
#' @param norm.z Logical argument (TRUE or FALSE) to specify if z is standardized
#' within variables (Default norm = FALSE).
#' @param permute.my2 Logical argument (TRUE or FALSE) to specify if realize
#' parallel permutation in matrix my2.
#' @param permute.mz2 Logical argument (TRUE or FALSE) to specify if realize
#' parallel permutation in matrix mz2.
#' @param strata Argument to specify restricting permutations within species
#' groups (Default strata = NULL).
#' @param na.rm Logical argument (TRUE or FALSE) to specify if pairwise
#' deletion of missing observations when computing dissimilarities (Default
#' na.rm = FALSE).
#' @param seqpermutation A set of predefined permutation, with the same dimensions of
#' permutations (Default seqpermutation = NULL).
#' @param parallel Number of parallel processes.  Tip: use parallel::detectCores() (Default parallel = NULL).
#' @param newClusters Logical argument (TRUE or FALSE) to specify if make new parallel
#' processes or use predefined socket cluster. Only if parallel is different of NULL (Default newClusters = TRUE).
#' @param CL A predefined socket cluster done with parallel package.
#' @param put.together List to specify group of traits. Each group specify receive the
#' same weight that one trait outside any group, in the way each group is considered
#' as unique trait (Default put.together = NULL). This argument must be a list, see
#' examples in \code{\link{syncsa}}.
#' @return \item{Obs}{Correlation between matrices.} \item{p}{Significance
#' level based on permutations.}
#' @author Vanderlei Julio Debastiani <vanderleidebastiani@@yahoo.com.br>
#' @seealso \code{\link{syncsa}}, \code{\link{organize.syncsa}}, \code{\link{mantel}},
#' \code{\link{procrustes}}
#' @references Pillar, V.D.; Duarte, L.d.S. (2010). A framework for
#' metacommunity analysis of phylogenetic structure. Ecology Letters, 13,
#' 587-596.
#'
#' Pillar, V.D., Duarte, L.d.S., Sosinski, E.E. & Joner, F. (2009).
#' Discriminating trait-convergence and trait-divergence assembly patterns in
#' ecological community gradients. Journal of Vegetation Science, 20, 334:348.
#' @keywords SYNCSA
#' @export
cor.matrix <- function (mx1, mx2, x, my1 = NULL, my2 = NULL, y, permute.my2 = FALSE, method = "pearson",
                        dist = "euclidean", permutations = 999, norm = FALSE, norm.y = FALSE,
                        strata = NULL, na.rm = FALSE, seqpermutation = NULL, parallel = NULL,
                        newClusters = TRUE, CL =  NULL)
{
  if(!is.null(seqpermutation)){
    if(dim(seqpermutation)[1]!=permutations){
      stop("\n The seqpermutation must be the same dimension of permutations\n")
    }
  }
  mx1 <- as.matrix(mx1)
  mx2 <- as.matrix(mx2)
  x <- as.matrix(x)
  if(permute.my2){
    my1 <- as.matrix(my1)
    my2 <- as.matrix(my2)
  }
  y <- as.matrix(y)
  dist.y <- vegan::vegdist(y, method = dist, na.rm = na.rm)
  dist.x <- vegan::vegdist(x, method = dist, na.rm = na.rm)
  correlation <- stats::cor(dist.x, dist.y, method = method)
  N <- nrow(mx2)
  if(is.null(seqpermutation)){
    seqpermutation <- permut.vector(N, strata = strata, nset = permutations)
  }
  if(!is.null(CL)){
    parallel <- length(CL)
  }
  ptest <- function(samp, mx1, mx2, my1, my2, dist.y, permute.my2, norm, norm.y, dist, na.rm, method){
    # x.permut <- mx1 %*% mx2[samp, ,drop=FALSE]
    x.permut <- matmult.syncsa(mx1, mx2[samp, , drop = FALSE])
    if (norm) {
      matrix.permut <- apply(x.permut^2, 2, sum)
      x.permut <- sweep(x.permut, 2, sqrt(matrix.permut), "/")
    }
    if(permute.my2){
      # y.permut <- my1 %*% my2[samp, ,drop=FALSE]
      y.permut <- matmult.syncsa(my1, my2[samp, , drop = FALSE])
      if (norm.y) {
        matrix.permut <- apply(y.permut^2, 2, sum)
        y.permut <- sweep(y.permut, 2, sqrt(matrix.permut), "/")
      }
      dist.y.permut <- vegan::vegdist(y.permut, method = dist, na.rm = na.rm)
    }
    dist.x.permut <- vegan::vegdist(x.permut, method = dist, na.rm = na.rm)
    if(!permute.my2){
      cor.x.permut <- stats::cor(dist.x.permut, dist.y, method = method)
    }
    if(permute.my2){
      cor.x.permut <- stats::cor(dist.x.permut, dist.y.permut, method = method)
    }
    return(cor.x.permut)
  }
  if(is.null(parallel)){
    value <- matrix(NA, nrow = permutations, ncol = 1)
    for (i in 1: permutations) {
      value[i,] <- ptest(samp = seqpermutation[i,], mx1 = mx1, mx2 = mx2, my1 = my1, my2 = my2, dist.y = dist.y, permute.my2 = permute.my2, norm = norm, norm.y = norm.y, dist = dist, na.rm = na.rm, method = method)
    }
  } else {
    if (newClusters) {
      CL <- parallel::makeCluster(parallel, type = "PSOCK")
    }
    value <- cbind(parallel::parRapply(CL, seqpermutation, ptest, mx1 = mx1, mx2 = mx2, my1 = my1, my2 = my2, dist.y = dist.y, permute.my2 = permute.my2, norm = norm, norm.y = norm.y, dist = dist, na.rm = na.rm, method = method))
    if (newClusters){
      parallel::stopCluster(CL)
    }
  }
  signific <- (sum(abs(value) >= abs(correlation)) + 1)/(permutations + 1)
  res <- list(Obs = correlation, p = signific)
  return(res)
}
