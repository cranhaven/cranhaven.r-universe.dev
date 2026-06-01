# Rfun_massey
# J Gou
# 2020-02-24
#
#' @name massey
#' @title Massey's method for Rating and Ranking 
#' @description Calculate ratings and provide rankings using Massey's method, Masseyized Colley method, Massey's method--no ties, Masseyized Colley method--no ties
#' @param jpMat a Judge-Presenter matrix, or a User-Movie matrix
#' @param method a character string specifying Massey's method, including "massey", "masseyc", "masseynt" and "masseycnt"
#' @param ties.method a character string specifying how ties are treated, including "average", "first", "last", "random", "max", "min", from base::rank
#' 
#' @details
#' \enumerate{
#' \item \code{massey}: Massey's method 
#' \item \code{masseyc}: Masseyized Colley method
#' \item \code{masseynt}: Massey's method, no ties
#' \item \code{masseycnt}: Masseyized Colley method, no ties
#' }
#' 
#' 
#' @author Jiangtao Gou
#' @export
#' @import stats
#' @import popdemo
#' 
#' @references
#' Gou, J. and Wu, S. (2020). A Judging System for Project Showcase: Rating and Ranking with Incomplete Information. Technical Report.
#' 
#' Langville, A. N. and Meyer, C. D. (2012). Who's Number 1?: The Science of Rating and Ranking. Princeton University Press.
#' 
#' Massey, K. (1997). Statistical models applied to the rating of sports teams. Bachelor's Thesis, Blueeld College.
#' 
#' @examples
#' jpMat <- matrix(data=c(5,4,3,0, 5,5,3,1, 0,0,0,5, 0,0,2,0, 4,0,0,3, 1,0,0,4),
#' nrow=6,
#' byrow=TRUE)
#' result <- massey(jpMat, method='massey')
#' print(result)
#
massey <- function(jpMat, method = 'massey', ties.method='average') {
  jpSummary <- readJudgePresenterMatrix(jpMat) ###
  rating <- rep(x=0, times=ncol(jpMat))
  ranking <- rating
  #
  # method A
  if (method == 'massey') {
  countGame <- rowSums(jpSummary$numGame, na.rm=TRUE, dims=1)
  #<https://stat.ethz.ch/R-manual/R-devel/library/base/html/colSums.html>
  MasseyMat <- (-jpSummary$numGame)
  diag(MasseyMat) <- countGame
  #<https://stat.ethz.ch/R-manual/R-devel/library/base/html/diag.html>
  p <- base::rowSums(jpSummary$numPt, na.rm=TRUE, dims=1) - base::colSums(jpSummary$numPt, na.rm=TRUE, dims=1)
  MasseyMatS <- MasseyMat
  MasseyMatS[nrow(MasseyMatS),] <- rep(x=1,times=ncol(MasseyMatS))
  pS <- p
  pS[length(p)] <- 0
  rating <- base::solve(MasseyMatS, pS)
  ranking <- base::rank(-rating,na.last="keep", ties.method)
  #<https://www.statmethods.net/advstats/matrix.html>
  } # End of if method
  #
  # method B
  if (method == 'masseynt') {
    countGame <- base::rowSums(jpSummary$numGame, na.rm=TRUE, dims=1) - base::rowSums(jpSummary$numTie, na.rm=TRUE, dims=1)
    #<https://stat.ethz.ch/R-manual/R-devel/library/base/html/colSums.html>
    MasseyMat <- (-jpSummary$numGame+jpSummary$numTie)
    diag(MasseyMat) <- countGame
    #<https://stat.ethz.ch/R-manual/R-devel/library/base/html/diag.html>
    p <- base::rowSums(jpSummary$numPt, na.rm=TRUE, dims=1) - colSums(jpSummary$numPt, na.rm=TRUE, dims=1)
    MasseyMatS <- MasseyMat
    MasseyMatS[nrow(MasseyMatS),] <- rep(x=1,times=ncol(MasseyMatS))
    pS <- p
    pS[length(p)] <- 0
    rating <- base::solve(MasseyMatS, pS)
    ranking <- base::rank(-rating,na.last="keep", ties.method)
    #<https://www.statmethods.net/advstats/matrix.html>
  } # End of if method
  #
  # method C
  if (method == 'masseyc') {
    countGame <- base::rowSums(jpSummary$numGame, na.rm=TRUE, dims=1)
    #<https://stat.ethz.ch/R-manual/R-devel/library/base/html/colSums.html>
    MasseyMat <- (-jpSummary$numGame)
    diag(MasseyMat) <- countGame
    #<https://stat.ethz.ch/R-manual/R-devel/library/base/html/diag.html>
    b <- 1 + 1/2*(rowSums(jpSummary$numWin, na.rm=TRUE, dims=1) - rowSums(jpSummary$numLoss, na.rm=TRUE, dims=1))
    MasseyMatS <- MasseyMat
    MasseyMatS[nrow(MasseyMatS),] <- rep(x=1,times=ncol(MasseyMatS))
    bS <- b
    bS[length(b)] <- 0
    rating <- base::solve(MasseyMatS, bS)
    ranking <- base::rank(-rating,na.last="keep", ties.method)
    #<https://www.statmethods.net/advstats/matrix.html>
  } # End of if method
  #
  # method D
  if (method == 'masseycnt') {
    countGame <- base::rowSums(jpSummary$numGame, na.rm=TRUE, dims=1) - base::rowSums(jpSummary$numTie, na.rm=TRUE, dims=1)
    #<https://stat.ethz.ch/R-manual/R-devel/library/base/html/colSums.html>
    MasseyMat <- (-jpSummary$numGame+jpSummary$numTie)
    diag(MasseyMat) <- countGame
    #<https://stat.ethz.ch/R-manual/R-devel/library/base/html/diag.html>
    b <- 1 + 1/2*(rowSums(jpSummary$numWin, na.rm=TRUE, dims=1) - rowSums(jpSummary$numLoss, na.rm=TRUE, dims=1))
    MasseyMatS <- MasseyMat
    MasseyMatS[nrow(MasseyMatS),] <- rep(x=1,times=ncol(MasseyMatS))
    bS <- b
    bS[length(b)] <- 0
    rating <- base::solve(MasseyMatS, bS)
    ranking <- base::rank(-rating,na.last="keep", ties.method)
    #<https://www.statmethods.net/advstats/matrix.html>
  } # End of if method
  #
  result <- list(rating=rating, ranking=ranking)
  return(result)
}