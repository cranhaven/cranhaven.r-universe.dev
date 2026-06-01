# Rfun_keener
# J Gou
# 2020-02-24
#
#' @name keener
#' @title Keener's method for Rating and Ranking
#' @description Calculate ratings and provide rankings using Keener's method,  without using Laplace's Rule of Succession, and using Laplace's Rule of Succession
#' @param jpMat a Judge-Presenter matrix, or a User-Movie matrix
#' @param method a character string specifying Keener's method without applying a nonlinear skweing function, including "keener", "keenerwolrs"
#' @param irreducibility a non-negative parameter, which is the ratio of the value of each element in the pertubation matrix to the average value in the normalized proportaion matrix.
#' @param ties.method a character string specifying how ties are treated, including "average", "first", "last", "random", "max", "min", from base::rank
#'
#'
#' @details
#' \enumerate{
#' \item \code{keener}: Keener's method with Laplace's Rule of Succession
#' \item \code{keenerwolrs}: Keener's method without Laplace's Rule of Succession
#' }
#'
#' \enumerate{
#' \item \code{hitsjp}: HITS, using judge-presenter matrix, equivalent to \code{offdefsc}
#' \item \code{offdefnt}: Offense-Defense rating method, using judge-presenter matrix
#' }
#'
#' @author Jiangtao Gou
#' @export
#' @import stats
#' @import popdemo
#'
#' @references
#' Gou, J. and Wu, S. (2020). A Judging System for Project Showcase: Rating and Ranking with Incomplete Information. Technical Report.
#'
#' Keener, J. P. (1993). The Perron-Frobenius theorem and the ranking of football teams. SIAM Review 35, 80-93.
#'
#' Langville, A. N. and Meyer, C. D. (2012). Who's Number 1?: The Science of Rating and Ranking. Princeton University Press.
#'
#' @examples
#' library(popdemo)
#' jpMat <- matrix(data=c(5,4,3,0, 5,5,3,1, 0,0,0,5, 0,0,2,0, 4,0,0,3, 1,0,0,4),
#' nrow=6,
#' byrow=TRUE)
#' result <- keener(jpMat,
#' method = 'keener',
#' irreducibility = 0)
#' print(result)
#
keener <- function(jpMat, method = 'keener', irreducibility = 0.01, ties.method='average') {
  jpSummary <- readJudgePresenterMatrix(jpMat) ###
  rating <- rep(x=0, times=ncol(jpMat))
  ranking <- rating
  #
  normalizationVec <- base::rowSums(jpSummary$numGame != 0)
  normalizationMat <- matrix(normalizationVec,ncol=1) %*% matrix(rep(x=1,times=length(normalizationVec)),nrow=1)
  #<https://stackoverflow.com/questions/22286957/count-the-number-of-non-zero-elements-of-each-column>
  avePtRaw <- jpSummary$numPt/jpSummary$numGame
  avePt <- avePtRaw
  avePt[is.nan(avePtRaw)] <- 0
  denominator <- avePt + t(avePt)
  if (method == 'keenerwolrs') {
    propMat <- avePt/denominator
    propMat[is.nan(avePtRaw)] <- 0
  } # End of if method
  if (method == 'keener') {
    propMat <- (avePt+1)/(denominator+2)
    propMat[is.nan(avePtRaw)] <- 0
  } # End of if method
  # print(propMat) #Debug
  nPropMat <- propMat/normalizationMat
  #
  nPropAve <- sum(nPropMat)/sum(normalizationVec)
  if (irreducibility > 0) {
    nPropMat <- nPropMat + irreducibility*nPropAve*matrix(rep(x=1,times=ncol(jpMat)*ncol(jpMat)), nrow=ncol(jpMat))
  }
  #
  #<https://rdrr.io/cran/popdemo/man/isIrreducible.html>
  #<https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/j.2041-210X.2012.00222.x>
  if (!popdemo::isIrreducible(nPropMat)) {
    result <- list(rating=rating, ranking=ranking)
    return(result)
  }
  #
  vv <- base::eigen(nPropMat)
  #<https://cran.r-project.org/web/packages/matlib/vignettes/eigen-ex1.html>
  rating <- vv$vectors[,1]
  #<https://stat.ethz.ch/R-manual/R-devel/library/base/html/complex.html>
  rating <- base::Re(rating/sum(rating))
  ranking <- base::rank(-rating, na.last="keep", ties.method)
  #
  result <- list(rating=rating, ranking=ranking)
  return(result)
}
