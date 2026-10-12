utils::globalVariables(c("result", "outcome"))
###############################################
# --------------------------------------------#
# overtime                                    #
# --------------------------------------------#
###############################################

# --------------------------------------------
# Main Function for computing overtime results
# --------------------------------------------

#' Computing overtime results
#' 
#' Computes the final overtime outcome.
#' 
#' @inheritParams overview
#'
#' @return \code{list} with probabilities of final outcome.
#' 
#' 
#' @export
overtime <- function(chancesHome, chancesAway, probGoalHome, probGoalAway) {
  
  # compute all possible results
  allResults <- expand.grid(goalsHome = 0:chancesHome, goalsAway = 0:chancesAway)
  # compute probability of results
  allResults$probability <- apply(allResults, 1, function(x) {
    probHome <- dbinom(x[1], chancesHome, prob = probGoalHome)
    probAway <- dbinom(x[2], chancesAway, prob = probGoalAway)
    probHome * probAway
  }
  )
  
  allResults$result <- ifelse(allResults$goalsHome == allResults$goalsAway, "tiedProbability", 
                              ifelse(allResults$goalsHome > allResults$goalsAway, 'winProbabilityHome',
                                     'winProbabilityAway'))

  out <- ddply(allResults, .(result), summarize, probability = sum(probability))
  
  list('tiedProbability' = max(c(out[out[,1] == 'tiedProbability', 2]), 0),
       'winProbabilityHome' = max(c(out[out[,1] == 'winProbabilityHome', 2]), 0),
       'winProbabilityAway' = max(c(out[out[,1] == 'winProbabilityAway', 2]), 0)
  )
}


# --------------------------------------------
# Main Function for computing overtime results
# --------------------------------------------

#' Computing outcome of penalty shootout
#' 
#' Computes outcome of a penalty shootout.
#' 
#' @param initial number of initial penalties (default 5)
#' @inheritParams overview
#'
#' @return \code{list} with probabilities of final outcome (\code{winProbabilityHome}, 
#' \code{winProbabilityAway}).
#' 
#' @export
penaltyShootout <- function(probPenaltySaveHome, probPenaltySaveAway, initial=5) {
  if(all(c(probPenaltySaveHome, probPenaltySaveAway) == 0)) {
    probPenaltySaveHome <- probPenaltySaveAway <- 0.5
  }
  if(all(c(probPenaltySaveHome, probPenaltySaveAway) == 1)) {
    probPenaltySaveHome <- probPenaltySaveAway <- 0.5
  }
  
  qHome <- 1-probPenaltySaveHome
  qAway <- 1-probPenaltySaveAway
  
  # possible paths
  e <- vector('list', 0)
  for (i in 0:(initial-1)) {
    e[[i*2 + 1]] <- c(0, 1)
    e[[i*2 + 2]] <- c(0, -1)
  }
  
  paths <- expand.grid(e)
  
  pathsTrans <- t(apply(paths, 1, function(x) {
    cumX <- cumsum(as.numeric(x))
    # end of penalty kicks
    v <- which(abs(cumX) >= c(initial+1, rep(initial:2, each = 2), 1))
    end <- ifelse(length(v) > 0, v[1], 2*initial)
    # replace values with -99
    if (end < (2*initial)) {
      x[(end+1):(2*initial)] <- -99
    }
    r <- ifelse(cumX[2*initial] > 0, "winHome",
                ifelse(cumX[2*initial] == 0, "draw",
                       'winAway'))
    c(x, end, r)
  }))
  # colnames
  colnames(pathsTrans) <- c(paste("Penalty", 1:(2*initial), sep = "_"), "end", "outcome")
  # remove duplicates
  pathsTransWoD <- pathsTrans[!duplicated(pathsTrans), ]
  
  pathsProbs <- pathsTransWoD
  # rename cols
  pathsProbs[pathsProbs == 0] <- "noGoal"
  pathsProbs[pathsProbs %in% c(1, -1)] <- "Goal"
  pathsProbs[, (0:(initial-1) * 2 + 1)][pathsProbs[, (0:(initial-1) * 2 + 1)] == 'noGoal'] <- qHome
  pathsProbs[, (0:(initial-1) * 2 + 1)][pathsProbs[, (0:(initial-1) * 2 + 1)] == 'Goal'] <- probPenaltySaveHome
  pathsProbs[, (1:initial)*2][pathsProbs[, (1:initial)*2] == 'noGoal'] <- qAway
  pathsProbs[, (1:initial)*2][pathsProbs[, (1:initial)*2] == 'Goal'] <- probPenaltySaveAway
  pathsProbs[pathsProbs == -99] <- 1    
  
  # fill pathsTrans with probabilites
  probDist <- data.frame(t(apply(pathsProbs, 1, function(x) {
    r <- prod(as.numeric(x[1:(2*initial)]))
    c(x, r)
  })))
  
  # colnames
  colnames(probDist) <- c(paste("Penalty", 1:(2*initial), sep = "_"), "end", 'outcome', "probability")
  
  # summarized output after 5 penalties
  outAfter5 <- ddply(probDist, .(outcome), summarize, probability = sum(as.numeric(as.character(probability))))
  
  # further penalties after draw
  winHome <- probPenaltySaveHome/(probPenaltySaveHome + probPenaltySaveAway)
  winAway <- 1 - winHome
  
  out <- list(winProbabilityHome = outAfter5$probability[outAfter5$outcome == 'winHome'] + outAfter5$probability[outAfter5$outcome == 'draw'] * winHome,
              winProbabilityAway = outAfter5$probability[outAfter5$outcome == 'winAway'] + outAfter5$probability[outAfter5$outcome == 'draw'] * winAway)
  
  return(out)
}

  