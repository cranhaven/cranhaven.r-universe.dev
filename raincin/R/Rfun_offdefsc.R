# Rfun_offdefsc
# J Gou
# 2020-03-09
# sc: score
#
#' @name offdefsc
#' @title Kleinberg's HITS algorithm for Rating and Ranking with Square Matrix
#' @description Calculate ratings and provide rankings using Kleinberg's HITS algorithm, using a square matrix for score matrix (presenter-presenter)
#' @param jpMat a Judge-Presenter matrix, or a User-Movie matrix
#' @param method a character string specifying the HITS algorithm, including "hitspp".
#' @param totalsupporteps a small number to guarantee the total support property
#' @param totalsupporttype an indicater: 1 stands for matrix ee^T and 2 stands for matrix ee^T - I
#' @param numiter a number of iterations
#' @param ties.method a character string specifying how ties are treated, including "average", "first", "last", "random", "max", "min", from base::rank
#'
#' @details
#' Large offense score means strong offense, and large defense score means weak defense
#' \enumerate{
#' \item \code{hitspp}: HITS, using presenter-presenter matrix, equivalent to \code{offdefsc}
#' \item \code{offdefsc}: Offense-Defense rating method, using presenter-presenter matrix
#' }
#'
#' @author Jiangtao Gou
#' @export
#' @import stats
#' @references
#' Gou, J. and Wu, S. (2020). A Judging System for Project Showcase: Rating and Ranking with Incomplete Information. Technical Report.
#'
#' Kleinberg, J. M. (1999). Authoritative sources in a hyperlinked environment. Journal of the ACM 46, 604-632.
#'
#' Langville, A. N. and Meyer, C. D. (2012). Who's Number 1?: The Science of Rating and Ranking. Princeton University Press.
#'
#' @examples
#' jpMat <- matrix(data=c(5,4,3,0, 5,5,3,1, 0,0,0,5, 0,0,2,0, 4,0,0,3, 1,0,0,4),nrow=6,byrow=TRUE)
#'  method <- 'hitspp'
#' totalsupporteps <- 0.01
#' totalsupporttype <- 1
#' numiter <- 10
#' ties.method <-'average'
#' result <- offdefsc(jpMat, method, totalsupporteps, totalsupporttype, numiter, ties.method)
#' print(result)
#
offdefsc <- function(jpMat, method='hitspp', totalsupporteps=0, totalsupporttype=1, numiter=100, ties.method='average') {
  #
  rating <- rep(x=0, times=ncol(jpMat))
  ranking <- rating
  #
  if (method == "hitspp" || method == "offdefsc") {
  jpSummary <- readJudgePresenterMatrix(jpMat) ###
  avePtRaw <- jpSummary$numPt/jpSummary$numGame
  avePt <- avePtRaw
  avePt[is.nan(avePtRaw)] <- 0
  scoreMat <- t(avePt)
  if (totalsupporttype == 1) {
    scoreMat <- scoreMat + totalsupporteps*matrix(rep(x=1,times=ncol(jpMat)*ncol(jpMat)), nrow=ncol(jpMat))
  } else if (totalsupporttype == 2) {
    scoreMat <- scoreMat + totalsupporteps* (matrix(rep(x=1,times=ncol(jpMat)*ncol(jpMat)), nrow=ncol(jpMat)) - diag(ncol(jpMat)))
  } else {
    scoreMat <- scoreMat
  }
  dvec <- matrix(rep(x=1, times=ncol(jpMat)), ncol=1)
  ovec <- matrix(rep(x=1, times=ncol(jpMat)), ncol=1)
  for (k in 1:numiter) {
    ovec <- t(scoreMat) %*% (1/dvec)
    ovec <- ncol(jpMat)/sum(ovec) * ovec
    dvec <- scoreMat %*% (1/ovec)
    dvec <- ncol(jpMat)/sum(dvec) * dvec
  }
  rating <- ovec/dvec
  ranking <- rank(-rating, na.last="keep", ties.method)
  }
  result <- list(rating=rating, ranking=ranking)
  return(result)
}
