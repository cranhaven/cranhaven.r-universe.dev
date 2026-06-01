# Rfun_offdefnt
# J Gou
# 2020-03-09
# nt: Netflix
#
#' @name offdefnt
#' @title Kleinberg's HITS algorithm for Rating and Ranking with Rectangular Matrix
#' @description Calculate ratings and provide rankings using Kleinberg's HITS algorithm, using a rectangular matrix for score matrix (judege-presenter)
#' @param jpMat a Judge-Presenter matrix, or a User-Movie matrix
#' @param method a character string specifying the HITS algorithm, including "hitsjp".
#' @param totalsupporteps a small number to guarantee the total support property
#' @param numiter a number of iterations
#' @param ties.method a character string specifying how ties are treated, including "average", "first", "last", "random", "max", "min", from base::rank
#'
#' @details
#' movie i is good and deserves a high rating mi if it gets high ratings from good (discriminating ) users. Similarly, user j is good and serves a high rating hj when his or her ratings match the true ratings of the movies.
#' \enumerate{
#' \item \code{hitsjp}: HITS, using judge-presenter matrix, equivalent to \code{offdefsc}
#' \item \code{offdefnt}: Offense-Defense rating method, using judge-presenter matrix
#' }
#'
#' @author Jiangtao Gou
#' @export
#' @import stats
#'
#' @references
#' Gou, J. and Wu, S. (2020). A Judging System for Project Showcase: Rating and Ranking with Incomplete Information. Technical Report.
#'
#' Kleinberg, J. M. (1999). Authoritative sources in a hyperlinked environment. Journal of the ACM 46, 604-632.
#'
#' Langville, A. N. and Meyer, C. D. (2012). Who's Number 1?: The Science of Rating and Ranking. Princeton University Press.
#'
#'
#' @examples
#' jpMat <- matrix(data=c(5,4,3,0, 5,5,3,1, 0,0,0,5, 0,0,2,0, 4,0,0,3, 1,0,0,4),nrow=6,byrow=TRUE)
#' method <- 'hitsjp'
#' totalsupporteps <- 0.01
#' numiter <- 10
#' ties.method <-'average'
#' result <- offdefnt(jpMat, method, totalsupporteps, numiter, ties.method)
#' print(result)
#
offdefnt <- function(jpMat, method='hitsjp', totalsupporteps=0, numiter=100, ties.method='average') {
  #
  rating <- rep(x=0, times=ncol(jpMat))
  ranking <- rating
  #
  if (method == "hitsjp" || method == "offdefnt") {
  jpMat <- jpMat + totalsupporteps*matrix(rep(x=1,times=nrow(jpMat)*ncol(jpMat)), ncol=ncol(jpMat))
  #
  uvec <- matrix(rep(x=1, times=nrow(jpMat)), ncol=1)
  mvec <- matrix(rep(x=1, times=ncol(jpMat)), ncol=1)
  for (k in 1:numiter) {
    mvec <- t(jpMat) %*% uvec
    mvec <- ncol(jpMat)/sum(mvec) * mvec
    uvec <- jpMat %*% mvec
    uvec <- nrow(jpMat)/sum(uvec) * uvec
  }
  rating <- mvec
  ranking <- rank(-rating, na.last="keep", ties.method)
  }
  result <- list(rating=rating, ranking=ranking)
  return(result)
}
