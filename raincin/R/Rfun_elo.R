# Rfun_elo.R
# J Gou
# 2020-02-25
#
#' @name elo
#' @title Elo’s Method for Rating and Ranking
#' @description Calculate ratings and provide rankings using Elo's system
#' @param jpMat a Judge-Presenter matrix, or a User-Movie matrix
#' @param method a character string specifying Elo's method, including "elo", "elow", "elos"
#' @param Kfactor a parameter to properly balance the deviation between actual and expected scroes against prior ratings
#' @param xiparameter a parameter affects the spread of the reatings in the logistic function
#' @param initScore a parameter describe the average rating
#' @param round a parameter indicates the number of iterations
#' @param ties.method a character string specifying how ties are treated, including "average", "first", "last", "random", "max", "min", from base::rank
#' 
#' 
#' @details
#' \enumerate{
#' \item \code{elo}: Elo's system, using win-tie-loss, equivalent to \code{elow}
#' \item \code{elow}: Elo's system, using win-tie-loss
#' \item \code{elos}: Elo's system, using game scores (each pair has one pair of scores)
#' }
#' 
#' @author Jiangtao Gou
#' @export
#' @import stats
#' @references
#' Elo, A. E. (1978). The Rating of Chessplayers, Past and Present. Arco Publishing Company, New York.
#' 
#' Gou, J. and Wu, S. (2020). A Judging System for Project Showcase: Rating and Ranking with Incomplete Information. Technical Report.
#' 
#' Langville, A. N. and Meyer, C. D. (2012). Who's Number 1?: The Science of Rating and Ranking. Princeton University Press.
#' 
#' @examples
#' jpMat <- matrix(data=c(5,4,3,0, 5,5,3,1, 0,0,0,5, 0,0,2,0, 4,0,0,3, 1,0,0,4),
#' nrow=6,
#' byrow=TRUE)
#' result <- elo(jpMat, 
#' method='elow', 
#' Kfactor=32, 
#' xiparameter=400, 
#' initScore=2000, 
#' round=10, 
#' ties.method='average')
#' print(result)
#
elo <- function(jpMat, method='elow', Kfactor=32, xiparameter=400, initScore=2000, round=100, ties.method='average') {
  jpSummary <- readJudgePresenterMatrix(jpMat) ###
  rating <- rep(x=0, times=ncol(jpMat))
  ranking <- rating
  score <- rep(x=initScore, times=ncol(jpMat)) # Old Score
  scoreNew <- score # New Score
  #
  if (method == 'elow' | method == 'elo') {
  Sw <- 1 # Win the game
  Sl <- 1 - Sw # Lose the game
  St <- 1/2 # Tie
  for (k in 1:round){
  for (i in 1:ncol(jpMat)) {
    for (j in 1:ncol(jpMat)) {
      if (jpSummary$numWin[i,j] > 0) {
        dscoreij <- score[i] - score[j] # difference between two scores
        dscoreji <- (-dscoreij)
        btlgtij <- 1/(1+10^(-dscoreij/xiparameter)) # base ten logistic function
        btlgtji <- 1 - btlgtij
        scoreNew[i] <- scoreNew[i] + jpSummary$numWin[i,j]*Kfactor*(Sw - btlgtij)
        scoreNew[j] <- scoreNew[j] + jpSummary$numWin[i,j]*Kfactor*(Sl - btlgtji)
        # print(scoreNew) # Debug
      } # End of if
    } # End of for j
  } # End of for i
  #
  for (i in 1:(ncol(jpMat)-1)) {
    for (j in (i+1):ncol(jpMat)) {
      if (jpSummary$numTie[i,j] > 0) {
        dscoreij <- score[i] - score[j] # difference between two scores
        dscoreji <- (-dscoreij)
        btlgtij <- 1/(1+10^(-dscoreij/xiparameter)) # base ten logistic function
        btlgtji <- 1 - btlgtij
        scoreNew[i] <- scoreNew[i] + jpSummary$numWin[i,j]*Kfactor*(St - btlgtij)
        scoreNew[j] <- scoreNew[j] + jpSummary$numWin[i,j]*Kfactor*(St - btlgtji)
        # print(scoreNew) # Debug
      } # End of if
    } # End of for j
  } # End of for i
  score <- scoreNew 
  } # End of for k
  } # End of if method
  #
  if (method == 'elos') {
    # From function keener
    avePtRaw <- jpSummary$numPt/jpSummary$numGame
    avePt[is.nan(avePtRaw)] <- 0
    denominator <- avePt + t(avePt)
    propMat <- (avePt+1)/(denominator+2)
    propMat[is.nan(avePtRaw)] <- 0
    # End of from function keener
    for (k in 1:round){
      for (i in 1:(ncol(jpMat)-1)) {
        for (j in (i+1):ncol(jpMat)) {
          if (propMat[i,j] > 0) {
            Sw <- propMat[i,j]
            Sl <- 1 - propMat[i,j]
            dscoreij <- score[i] - score[j] # difference between two scores
            dscoreji <- (-dscoreij)
            btlgtij <- 1/(1+10^(-dscoreij/xiparameter)) # base ten logistic function
            btlgtji <- 1 - btlgtij
            scoreNew[i] <- scoreNew[i] + jpSummary$numWin[i,j]*Kfactor*(Sw - btlgtij)
            scoreNew[j] <- scoreNew[j] + jpSummary$numWin[i,j]*Kfactor*(Sl - btlgtji)
            # print(scoreNew) # Debug
          } # End of if
        } # End of for j
      } # End of for i
      #
      score <- scoreNew 
    } # End of for k
  } # End of if method
  #
  rating <- scoreNew
  ranking <- rank(-rating, na.last="keep", ties.method)
  result <- list(rating=rating, ranking=ranking)
  return(result)
}
