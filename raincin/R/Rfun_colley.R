# Rfun_colley
# J Gou
# 2020-02-24
#
#' @name colley
#' @title Colley’s Method for Rating and Ranking
#' @description Calculate ratings and provide rankings using Colley’s method
#' @param jpMat a Judge-Presenter matrix, or a User-Movie matrix
#' @param method a character string specifying Colley's method, including "colley", "colleym", "colleynt" and "colleymnt"
#' @param ties.method a character string specifying how ties are treated, including "average", "first", "last", "random", "max", "min", from base::rank
#' @return A list of two vectors: a rating vector and a ranking vector
#' @details
#' \enumerate{
#' \item \code{colley}: Colley's method 
#' \item \code{colleym}: Colleyized Massey method
#' \item \code{colleynt}: Colley's method, no ties
#' \item \code{colleymnt}: Colleyized Massey method, no ties
#' }
#' @author Jiangtao Gou
#' @export
#' @import stats
#' @references
#' Colley, W. N. (2001). Colley's bias free college football ranking method: the Colley matrix explained.
#' 
#' Gou, J. and Wu, S. (2020). A Judging System for Project Showcase: Rating and Ranking with Incomplete Information. Technical Report.
#' 
#' Langville, A. N. and Meyer, C. D. (2012). Who's Number 1?: The Science of Rating and Ranking. Princeton University Press.
#' 
#' @examples 
#' jpMat <- matrix(data=c(5,4,3,0, 5,5,3,1, 0,0,0,5, 0,0,2,0, 4,0,0,3, 1,0,0,4), 
#' nrow=6, 
#' byrow=TRUE)
#' result <- colley(jpMat, method='colley')
#' print(result)
#
colley <- function(jpMat, method = 'colley', ties.method='average') {
  jpSummary <- readJudgePresenterMatrix(jpMat) ###
  rating <- rep(x=0, times=ncol(jpMat))
  ranking <- rating
  #
  # method A
  if (method == 'colleym') {
    countGame <- base::rowSums(jpSummary$numGame, na.rm=TRUE, dims=1)
    #<https://stat.ethz.ch/R-manual/R-devel/library/base/html/colSums.html>
    MasseyMat <- (-jpSummary$numGame)
    diag(MasseyMat) <- countGame
    ColleyMat <- MasseyMat + 2*diag(x=1,nrow=ncol(jpMat))
    #<https://stat.ethz.ch/R-manual/R-devel/library/base/html/diag.html>
    p <- base::rowSums(jpSummary$numPt, na.rm=TRUE, dims=1) - base::colSums(jpSummary$numPt, na.rm=TRUE, dims=1)
    rating <- solve(ColleyMat, p)
    ranking <- rank(-rating,na.last="keep", ties.method)
    #<https://www.statmethods.net/advstats/matrix.html>
  } # End of if method
  #
  # method B
  if (method == 'colleymnt') {
    countGame <- base::rowSums(jpSummary$numGame, na.rm=TRUE, dims=1) - base::rowSums(jpSummary$numTie, na.rm=TRUE, dims=1)
    #<https://stat.ethz.ch/R-manual/R-devel/library/base/html/colSums.html>
    MasseyMat <- (-jpSummary$numGame+jpSummary$numTie)
    diag(MasseyMat) <- countGame
    ColleyMat <- MasseyMat + 2*diag(x=1,nrow=ncol(jpMat))
    #<https://stat.ethz.ch/R-manual/R-devel/library/base/html/diag.html>
    p <- base::rowSums(jpSummary$numPt, na.rm=TRUE, dims=1) - base::colSums(jpSummary$numPt, na.rm=TRUE, dims=1)
    rating <- base::solve(ColleyMat, p)
    ranking <- base::rank(-rating,na.last="keep", ties.method)
    #<https://www.statmethods.net/advstats/matrix.html>
  } # End of if method
  #
  # method C
  if (method == 'colley') {
    countGame <- rowSums(jpSummary$numGame, na.rm=TRUE, dims=1)
    #<https://stat.ethz.ch/R-manual/R-devel/library/base/html/colSums.html>
    MasseyMat <- (-jpSummary$numGame)
    diag(MasseyMat) <- countGame
    ColleyMat <- MasseyMat + 2*diag(x=1,nrow=ncol(jpMat))
    #<https://stat.ethz.ch/R-manual/R-devel/library/base/html/diag.html>
    b <- 1 + 1/2*(rowSums(jpSummary$numWin, na.rm=TRUE, dims=1) - rowSums(jpSummary$numLoss, na.rm=TRUE, dims=1))
    rating <- solve(ColleyMat, b)
    ranking <- rank(-rating,na.last="keep", ties.method)
    #<https://www.statmethods.net/advstats/matrix.html>
  } # End of if method
  #
  # method D
  if (method == 'colleynt') {
    countGame <- rowSums(jpSummary$numGame, na.rm=TRUE, dims=1) - rowSums(jpSummary$numTie, na.rm=TRUE, dims=1)
    #<https://stat.ethz.ch/R-manual/R-devel/library/base/html/colSums.html>
    MasseyMat <- (-jpSummary$numGame+jpSummary$numTie)
    diag(MasseyMat) <- countGame
    ColleyMat <- MasseyMat + 2*diag(x=1,nrow=ncol(jpMat))
    #<https://stat.ethz.ch/R-manual/R-devel/library/base/html/diag.html>
    b <- 1 + 1/2*(rowSums(jpSummary$numWin, na.rm=TRUE, dims=1) - rowSums(jpSummary$numLoss, na.rm=TRUE, dims=1))
    rating <- solve(ColleyMat, b)
    ranking <- rank(-rating,na.last="keep", ties.method)
    #<https://www.statmethods.net/advstats/matrix.html>
  } # End of if method
  #
  result <- list(rating=rating, ranking=ranking)
  return(result)
}
# P.27 solution is incorrect
# matrix(c(-3,-1,-1,7),nrow=1) %*% matrix(c(.28, .33, .05,-.39),nrow=4)