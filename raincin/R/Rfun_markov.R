# Rfun_markov.R
# J Gou
# 2020-02-25
#
#' @name markov
#' @title Google's PageRank algorithm for Rating and Ranking
#' @description Calculate ratings and provide rankings using Google's PageRank algorithm
#' @param jpMat a Judge-Presenter matrix, or a User-Movie matrix
#' @param method a character string specifying Markov's method, including "markov", "markovvl", "markovlvpd", "markovwlvp".
#' @param dampingFactor the PageRank theory holds that an imaginary surfer who is randomly clicking on links will eventually stop clicking. The probability, at any step, that the person will continue is a damping factor. Web 0.85, NFL 0.60, NCAA basketball 0.50
#' @param ties.method a character string specifying how ties are treated, including "average", "first", "last", "random", "max", "min", from base::rank
#' 
#' @details
#' \enumerate{
#' \item \code{markov}: Markov's method, voting with losses, equivalent to \code{markovvl}
#' \item \code{markovvl}: Markov's method, voting with losses
#' \item \code{markovlvpd}: Markov's method, losers vote with point differentials
#' \item \code{markovwlvp}: Markov's method, winners and losers vote with points
#' }
#' @author Jiangtao Gou
#' @export
#' @import stats
#' 
#' @references
#' Brin, S. and Page, L. (1998). The anatomy of a large-scale hypertextual web search engine. Computer Networks and ISDN Systems 30, 107-117. Proceedings of the Seventh International World Wide Web Conference.
#' 
#' Gou, J. and Wu, S. (2020). A Judging System for Project Showcase: Rating and Ranking with Incomplete Information. Technical Report.
#' 
#' Langville, A. N. and Meyer, C. D. (2012). Who's Number 1?: The Science of Rating and Ranking. Princeton University Press.
#' 
#' @examples
#' jpMat <- matrix(data=c(5,4,3,0, 5,5,3,1, 0,0,0,5, 0,0,2,0, 4,0,0,3, 1,0,0,4),
#' nrow=6,
#' byrow=TRUE)
#' result <- markov(jpMat, 
#' method='markovvl', 
#' dampingFactor=0.85, 
#' ties.method='average')
#' print(result)
#
markov <- function(jpMat, method='markovvl', dampingFactor=0.85, ties.method='average') {
  jpSummary <- readJudgePresenterMatrix(jpMat) ###
  rating <- rep(x=0, times=ncol(jpMat))
  ranking <- rating
  #
  if (method == 'markovlvpd') {
  votingMat <- t(jpSummary$numPt) - jpSummary$numPt
  votingMat[which(votingMat < 0)] <- 0
  votesVec <- base::rowSums(votingMat, na.rm=TRUE, dims=1)
  idxZeroVotes <- which(votesVec==0) # index of row sum 0
  votesVec[idxZeroVotes] <- 1
  votingMat[idxZeroVotes,idxZeroVotes] <- 1
  #
  stochasticMat <- votingMat / (matrix(votesVec,ncol=1) %*% matrix(rep(x=1,times=length(votesVec)),nrow=1))
  } # End of if method 
  #
  if (method == 'markov' | method == 'markovvl') {
    votingMat <- jpSummary$numLoss + 1/2*jpSummary$numTie
    # votingMat[2,1] <- 0 #Debug, test a special example
    votesVec <- base::rowSums(votingMat,na.rm=TRUE, dims=1)
    idxZeroVotes <- which(votesVec==0) # index of row sum 0
    votesVec[idxZeroVotes] <- 1
    votingMat[idxZeroVotes,idxZeroVotes] <- 1
    #
    stochasticMat <- votingMat / (matrix(votesVec,ncol=1) %*% matrix(rep(x=1,times=length(votesVec)),nrow=1))
  } # End of if method 
  #
  if (method == 'markovwlvp') {
    votingMat <- t(jpSummary$numPt)
    votesVec <- base::rowSums(votingMat, na.rm=TRUE, dims=1)
    idxZeroVotes <- which(votesVec==0) # index of row sum 0
    votesVec[idxZeroVotes] <- 1
    votingMat[idxZeroVotes,idxZeroVotes] <- 1
    #
    stochasticMat <- votingMat / (matrix(votesVec,ncol=1) %*% matrix(rep(x=1,times=length(votesVec)),nrow=1))
  } # End of if method 
  #
  markovMat <- dampingFactor*stochasticMat + (1-dampingFactor)/ncol(jpMat)*matrix(rep(x=1,times=ncol(jpMat)*ncol(jpMat)), nrow=ncol(jpMat))
  #
  vv <- base::eigen(t(markovMat)) 
  #<https://cran.r-project.org/web/packages/matlib/vignettes/eigen-ex1.html>
  rating <- vv$vectors[,1]
  #<https://stat.ethz.ch/R-manual/R-devel/library/base/html/complex.html>
  rating <- base::Re(rating/sum(rating))
  ranking <- base::rank(-rating, na.last="keep", ties.method)
  #
  result <- list(rating=rating, ranking=ranking)
  return(result)
}