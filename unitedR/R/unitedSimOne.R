#' @include simRedCard.R
#' @include formation.R
#' @include penaltyGoalsProb.R
#' @include unitedSimClass.R
#' @include overtime.R
NULL

utils::globalVariables(c("goalsHome", "goalsAway", "probability",
                         "chancesAwayDF", "chancesAwayMF", "chancesAwayST",
                         "chancesHomeDF", "chancesHomeMF",
                         "chancesHomeST", "probPenSaveAway", "probPenSaveHome"))

###############################################
# --------------------------------------------#
# unitedSimOne                                #
# --------------------------------------------#
###############################################

# --------------------------------------------
# Main Function for simulation line ups
# --------------------------------------------

#' Simulating a formation
#'
#' Simulates a formation against another formation.
#'
#' @inheritParams overview
#'
#' @return Creates an object of the \code{unitedSim} class.
#'
#' @seealso \code{\link{unitedSim}}
#'
#' @examples
#' home <- formation(10, NA, c(7,5,3), c(8,8), c(10,10,8))
#' away <- formation(5, 8, c(8,8), c(10,10), c(10,10,10),
#'  hardness = c(0,0,0,0,1))
#' set.seed(123)
#' unitedSimOne(home, away)
#' # results with overtime
#' # Note: Only key statistics are adjusted for overtime
#' unitedSimOne(home, away, overtime = TRUE)
#' # simulating the game
#' unitedSimOne(home, away, r = 100)
#'
#' @export
unitedSimOne <- function(home, away, r, penaltyProb = 0.1, preventGoalGK = 1/14, preventGoalSW = 1/15,
                         hardnessMatrix, overtime = FALSE) {
  stopifnot(validObject(home), validObject(away), is(home, "formation"),
            is(home, "formation"), is.numeric(preventGoalGK), is.numeric(preventGoalSW))
  if (preventGoalGK >= 1/13) stop("preventGoalGK must be smaller than 1/13.")
  if (preventGoalGK < 0) stop("preventGoalGK must be greater than zero.")
  if (preventGoalSW >= 1/13) stop("preventGoalSW must be smaller than 1/13.")
  if (preventGoalSW < 0) stop("preventGoalSW must be greater than zero.")
  if (penaltyProb < 0 || penaltyProb > 1) stop("Probability for a penalty should be in [0,1]")

  ## set default value for hardness matrix
  if (missing(hardnessMatrix)) {
    hardnessMatrix <- matrix(c(90,10,0,0,0,0,0,0,70,30,0,0,0,0,0,0,50,40,10,
                               0,0,0,0,0,30,50,20,0,0,0,0,0,20,40,30,10,0,0,
                               0,0, 10,30,40,20,0,0,0,0,0,20,40,30,10,0,0,0,0,
                               10,30,40,20,0,0,0,0,0,20,40,30,10,0,0,0,0,10,20,
                               40,20,10,0,0,0,0,10,40,20,20,10), nrow = 8)
  }

  if (missing(r)) {
    if (sum(home@hardness) > 1 || sum(away@hardness > 1)) {
      warning("It is recommended to simulate hardness and penalties, calculations are exact for one possible lineup.")
    }

    homeLineup <- getLineup(home)
    awayLineup <- getLineup(away)

    # simulate red cards
    homeLineupSim <- simRedCard(home, homeLineup, hardnessMatrix)
    awayLineupSim <- simRedCard(away, awayLineup, hardnessMatrix)

    # save the number of red cards
    redCardsHome <- homeLineupSim$numberRedCards
    redCardsAway <- awayLineupSim$numberRedCards

    homeLineupSim <- homeLineupSim$lineup
    awayLineupSim <- awayLineupSim$lineup

    chancesHomeS <- round((homeLineupSim[3:5] - awayLineupSim[5:3] -
                          c(0, 0, awayLineupSim[2])) * c(1/4, 1/2, 1) + 0.00001)
    chancesHome <- sum(chancesHomeS[chancesHomeS > 0])

    chancesAwayS <- round((awayLineupSim[3:5] - homeLineupSim[5:3] -
                          c(0, 0, homeLineupSim[2])) * c(1/4, 1/2, 1) + 0.00001)
    chancesAway <- sum(chancesAwayS[chancesAwayS > 0])

    # possible penalties home
    posPenaltiesHome <- sum(away@hardness)
    posPenaltiesAway <- sum(home@hardness)

    # probability of a goal of a penalty
    penaltyProbGoalHome <- 1 - (awayLineupSim[1] * 0.05)
    penaltyProbGoalAway <- 1 - (homeLineupSim[1] * 0.05)

    # probability distribution of all possible goals by penalties for both teams
    goalsPenaltyDistrHome <- penaltyGoalsProb(posPenaltiesHome, penaltyProbGoalHome, penaltyProb)
    goalsPenaltyDistrAway <- penaltyGoalsProb(posPenaltiesAway, penaltyProbGoalAway, penaltyProb)

    # possible allocations of the penalties
    penaltyAllocations <- expand.grid(home = 0:posPenaltiesHome, away = 0:posPenaltiesAway)
    penaltyAllocations$probability <- apply(penaltyAllocations, 1, function(x) {
        probHome <- goalsPenaltyDistrHome$probability[which(goalsPenaltyDistrHome$goals == x[1])]
        probAway <- goalsPenaltyDistrAway$probability[which(goalsPenaltyDistrAway$goals == x[2])]
        probHome * probAway
      }
    )

    # compute all possible results
    allResults <- expand.grid(goalsHome = 0:chancesHome, goalsAway = 0:chancesAway)
    # compute probability of a goal
    probGoalAway <- (1-homeLineupSim[1] * preventGoalGK) * (1-homeLineupSim[2] * preventGoalSW)
    probGoalHome <- (1-awayLineupSim[1] * preventGoalGK) * (1-awayLineupSim[2] * preventGoalSW)
    # compute probability of results
    allResults$probability <- apply(allResults, 1, function(x) {
        probHome <- dbinom(x[1], chancesHome, prob = probGoalHome)
        probAway <- dbinom(x[2], chancesAway, prob = probGoalAway)
        probHome * probAway
      }
    )

    # list for all possible results including penalties
    allResWithPen <- vector("list", nrow(penaltyAllocations))
    for (i in 1:nrow(penaltyAllocations)) {
      resultsPenalty <- allResults
      resultsPenalty$goalsHome <- resultsPenalty$goalsHome + penaltyAllocations$home[i]
      resultsPenalty$goalsAway <- resultsPenalty$goalsAway + penaltyAllocations$away[i]
      resultsPenalty$probability <- resultsPenalty$probability * penaltyAllocations$probability[i]
      allResWithPen[[i]] <- resultsPenalty
    }

    # merge the data.frames
    finalPossibleResults <- do.call("rbind", allResWithPen)

    finalPossibleResults <- ddply(finalPossibleResults, .(goalsHome, goalsAway), summarize,
                                probability = sum(probability))


    # sort results by probability of apprearance
    finalPossibleResults <- finalPossibleResults[order(finalPossibleResults$probability, decreasing = TRUE), ]
    # add a cumsum of the probabilities
    finalPossibleResults$cumsumProb <- cumsum(finalPossibleResults$probability)
    # add points for home
    finalPossibleResults$pointsHome <- ifelse(finalPossibleResults$goalsHome > finalPossibleResults$goalsAway, 3,
                                            ifelse(finalPossibleResults$goalsHome == finalPossibleResults$goalsAway, 1,
                                                   0))
    # add points for away
    finalPossibleResults$pointsAway <- ifelse(finalPossibleResults$goalsHome < finalPossibleResults$goalsAway, 3,
                                            ifelse(finalPossibleResults$goalsHome == finalPossibleResults$goalsAway, 1,
                                                   0))
    # add training points (TP) for home
    finalPossibleResults$tpHome <- ifelse(finalPossibleResults$goalsHome > finalPossibleResults$goalsAway, 1,
                                        ifelse(finalPossibleResults$goalsHome == finalPossibleResults$goalsAway, 0.5,
                                               0))
    # add traings points (TP) for away
    finalPossibleResults$tpAway <- ifelse(finalPossibleResults$goalsHome < finalPossibleResults$goalsAway, 1,
                                        ifelse(finalPossibleResults$goalsHome == finalPossibleResults$goalsAway, 0.5,
                                               0))

    # add red cards
    finalPossibleResults$redCardsHome <- redCardsHome
    finalPossibleResults$redCardsAway <- redCardsAway
    finalPossibleResults$draw <- ifelse(finalPossibleResults$goalsHome == finalPossibleResults$goalsAway, TRUE, FALSE)
    finalPossibleResults$chancesHomeST <- max(c(chancesHomeS[3], 0))
    finalPossibleResults$chancesHomeMF <- max(c(chancesHomeS[2], 0))
    finalPossibleResults$chancesHomeDF <- max(c(chancesHomeS[1], 0))
    finalPossibleResults$chancesAwayST <- max(c(chancesAwayS[3], 0))
    finalPossibleResults$chancesAwayMF <- max(c(chancesAwayS[2], 0))
    finalPossibleResults$chancesAwayDF <- max(c(chancesAwayS[1], 0))
    finalPossibleResults$probGoalHome <- probGoalHome
    finalPossibleResults$probGoalAway <- probGoalAway
    finalPossibleResults$probPenSaveHome <- homeLineupSim[1] * 0.05
    finalPossibleResults$probPenSaveAway <- awayLineupSim[1] * 0.05

    # output
    if (!overtime) {
      output <- new("unitedSim",
                    results = finalPossibleResults,
                    averageTrainingPointsHome = round(sum(finalPossibleResults$tpHome * finalPossibleResults$probability), digits = 4),
                    averageTrainingPointsAway = round(sum(finalPossibleResults$tpAway * finalPossibleResults$probability), digits = 4),
                    averagePointsHome = round(sum(finalPossibleResults$pointsHome * finalPossibleResults$probability), digits = 4),
                    averagePointsAway = round(sum(finalPossibleResults$pointsAway * finalPossibleResults$probability), digits = 4),
                    winProbabilityHome = round(sum((finalPossibleResults$pointsHome == 3) * finalPossibleResults$probability), digits = 4),
                    winProbabilityAway = round(sum((finalPossibleResults$pointsAway == 3) * finalPossibleResults$probability), digits = 4),
                    tiedProbability = round(sum((finalPossibleResults$pointsAway == 1) * finalPossibleResults$probability), digits = 4),
                    averageRedCardsHome =  sum(finalPossibleResults$redCardsHome * finalPossibleResults$probability),
                    averageRedCardsAway =  sum(finalPossibleResults$redCardsAway * finalPossibleResults$probability),
                    home = home,
                    away = away)

    } else {
      # computing chances and penalty probabilites for overtime
      chancesHome <- sum(floor(c(finalPossibleResults$chancesHomeDF[1], finalPossibleResults$chancesHomeMF[1], finalPossibleResults$chancesHomeST[1])/3 + 0.5))
      chancesAway <- sum(floor(c(finalPossibleResults$chancesAwayDF[1], finalPossibleResults$chancesAwayMF[1], finalPossibleResults$chancesAwayST[1])/3 + 0.5))
      probGoalHome <- finalPossibleResults$probGoalHome[1]
      probGoalAway <- finalPossibleResults$probGoalAway[1]
      probPenaltySaveHome <- finalPossibleResults$probPenSaveHome[1]
      probPenaltySaveAway <- finalPossibleResults$probPenSaveAway[1]
      tiedProb <- sum((finalPossibleResults$pointsHome == 1) * finalPossibleResults$probability)

      # computing overtime and penalty shootout
      o <- overtime(chancesHome, chancesAway, probGoalHome, probGoalAway)
      pS <- penaltyShootout(probPenaltySaveHome, probPenaltySaveAway)

      # compute final win probabilities
      winProbabilityHome = sum((finalPossibleResults$pointsHome == 3) * finalPossibleResults$probability) +
        tiedProb * (o$winProbabilityHome + o$tiedProbability * pS$winProbabilityHome)
      winProbabilityAway = sum((finalPossibleResults$pointsAway == 3) * finalPossibleResults$probability) +
        tiedProb * (o$winProbabilityAway + o$tiedProbability * pS$winProbabilityAway)

      output <- new("unitedSim",
                    results = finalPossibleResults,
                    averageTrainingPointsHome = sum(finalPossibleResults$tpHome * finalPossibleResults$probability),
                    averageTrainingPointsAway = sum(finalPossibleResults$tpAway * finalPossibleResults$probability),
                    averagePointsHome = sum(finalPossibleResults$pointsHome * finalPossibleResults$probability),
                    averagePointsAway = sum(finalPossibleResults$pointsAway * finalPossibleResults$probability),
                    winProbabilityHome = winProbabilityHome,
                    winProbabilityAway = winProbabilityAway,
                    tiedProbability = 0,
                    averageRedCardsHome =  sum(finalPossibleResults$redCardsHome * finalPossibleResults$probability),
                    averageRedCardsAway =  sum(finalPossibleResults$redCardsAway * finalPossibleResults$probability),
                    home = home,
                    away = away)

    }

    return(output)
    # include red Cards when simulating
  } else {
    stopifnot(is.numeric(r), round(r) == r, length(r) == 1)
    homeLineup <- getLineup(home)
    awayLineup <- getLineup(away)
    simulatedResults <- t(sapply(1:r, function(x) {
              # simulate red cards
              homeLineupSim <- simRedCard(home, homeLineup, hardnessMatrix)
              awayLineupSim <- simRedCard(away, awayLineup, hardnessMatrix)

              # save the number of red cards
              redCardsHome <- homeLineupSim$numberRedCards
              redCardsAway <- awayLineupSim$numberRedCards

              homeLineupSim <- homeLineupSim$lineup
              awayLineupSim <- awayLineupSim$lineup

              chancesHomeS <- round((homeLineupSim[3:5] - awayLineupSim[5:3] -
                                      c(0, 0, awayLineupSim[2])) * c(1/4, 1/2, 1))
              chancesHome <- sum(chancesHomeS[chancesHomeS > 0])

              chancesAwayS <- round((awayLineupSim[3:5] - homeLineupSim[5:3] -
                                      c(0, 0, homeLineupSim[2])) * c(1/4, 1/2, 1))
              chancesAway <- sum(chancesAwayS[chancesAwayS > 0])

              #  penalties home
              penaltiesHome <- rbinom(1, sum(away@hardness), penaltyProb)
              penaltiesAway <- rbinom(1, sum(home@hardness), penaltyProb)

              # probability of a goal by penalty
              penaltyProbGoalHome <- 1 - (awayLineupSim[1] * 0.05)
              penaltyProbGoalAway <- 1 - (homeLineupSim[1] * 0.05)

              # probability of a goal with a chance
              probGoalAway <- (1-homeLineupSim[1] * preventGoalGK) * (1-homeLineupSim[2] * preventGoalSW)
              probGoalHome <- (1-awayLineupSim[1] * preventGoalGK) * (1-awayLineupSim[2] * preventGoalSW)

              # simulate the game
              penaltyGoalsHome <- rbinom(1, penaltiesHome, penaltyProbGoalHome)
              penaltyGoalsAway <- rbinom(1, penaltiesAway, penaltyProbGoalAway)
              goalsHomeGame <- rbinom(1, chancesHome, probGoalHome)
              goalsAwayGame <- rbinom(1, chancesAway, probGoalAway)
              c(penaltyGoalsHome + goalsHomeGame, penaltyGoalsAway + goalsAwayGame, redCardsHome, redCardsAway,
                max(c(chancesHomeS[3], 0)), max(c(chancesHomeS[2], 0)), max(c(chancesHomeS[1], 0)),
                max(c(chancesAwayS[3], 0)), max(c(chancesAwayS[2], 0)), max(c(chancesAwayS[1], 0)),
                probGoalHome, probGoalAway,
                1-penaltyProbGoalAway, 1-penaltyProbGoalHome)
      }
    ))
    simulatedResults <- as.data.frame(simulatedResults)
    if (overtime) {
      colnames(simulatedResults) <- c("goalsHome", "goalsAway", "redCardsHome", "redCardsAway",
                                      'chancesHomeST', 'chancesHomeMF', 'chancesHomeDF',
                                      'chancesAwayST', 'chancesAwayMF', 'chancesAwayDF',
                                      'probGoalHome', 'probGoalAway',
                                      'probPenSaveHome', 'probPenSaveAway')
    } else {
      simulatedResults <- simulatedResults[,1:4]
      colnames(simulatedResults) <- c("goalsHome", "goalsAway", "redCardsHome", "redCardsAway")
    }

    simulatedResults$probability <- 1/r

    if (overtime) {
      simulatedResults <- ddply(simulatedResults,
                                .(goalsHome, goalsAway, redCardsHome, redCardsAway, chancesHomeST, chancesHomeMF, chancesHomeDF, chancesAwayST, chancesAwayMF, chancesAwayDF, probGoalHome, probGoalAway, probPenSaveHome, probPenSaveAway),
                                summarize, probability = sum(probability))
    } else {
      simulatedResults <- ddply(simulatedResults,
                                .(goalsHome, goalsAway, redCardsHome, redCardsAway),
                                summarize, probability = sum(probability))
    }

    # sort results by probability of appeareance
    simulatedResults <- simulatedResults[order(simulatedResults$probability, decreasing = TRUE), ]
    # add a cumsum of the probabilities
    simulatedResults$cumsumProb <- cumsum(simulatedResults$probability)
    # add points for home
    simulatedResults$pointsHome <- ifelse(simulatedResults$goalsHome > simulatedResults$goalsAway, 3,
                                              ifelse(simulatedResults$goalsHome == simulatedResults$goalsAway, 1,
                                                     0))
    # add points for away
    simulatedResults$pointsAway <- ifelse(simulatedResults$goalsHome < simulatedResults$goalsAway, 3,
                                              ifelse(simulatedResults$goalsHome == simulatedResults$goalsAway, 1,
                                                     0))
    # add training points (TP) for home
    simulatedResults$tpHome <- ifelse(simulatedResults$goalsHome > simulatedResults$goalsAway, 1,
                                          ifelse(simulatedResults$goalsHome == simulatedResults$goalsAway, 0.5,
                                                 0))
    # add traings points (TP) for away
    simulatedResults$tpAway <- ifelse(simulatedResults$goalsHome < simulatedResults$goalsAway, 1,
                                          ifelse(simulatedResults$goalsHome == simulatedResults$goalsAway, 0.5,
                                                 0))

    # change colnames according to the not simulated case
    if (overtime) {
      simulatedResults <- simulatedResults[, c(1:2, 15:20, 3:14)]
    } else {
      simulatedResults <- simulatedResults[, c(1:2, 5:10, 3:4)]
    }

    # add column for draw
    simulatedResults$draw <- ifelse(simulatedResults$goalsHome == simulatedResults$goalsAway, TRUE, FALSE)



    # output
    if (!overtime) {
      output <- new("unitedSimR",
                    results = simulatedResults,
                    averageTrainingPointsHome = sum(simulatedResults$tpHome * simulatedResults$probability),
                    averageTrainingPointsAway = sum(simulatedResults$tpAway * simulatedResults$probability),
                    averagePointsHome = sum(simulatedResults$pointsHome * simulatedResults$probability),
                    averagePointsAway = sum(simulatedResults$pointsAway * simulatedResults$probability),
                    winProbabilityHome = sum((simulatedResults$pointsHome == 3) * simulatedResults$probability),
                    winProbabilityAway = sum((simulatedResults$pointsAway == 3) * simulatedResults$probability),
                    tiedProbability = sum((simulatedResults$pointsAway == 1) * simulatedResults$probability),
                    averageRedCardsHome =  sum(simulatedResults$redCardsHome * simulatedResults$probability),
                    averageRedCardsAway =  sum(simulatedResults$redCardsAway * simulatedResults$probability),
                    r = r,
                    home = home,
                    away = away)
    } else {
      warning('overtime only working correctly without hardness')
      output <- new("unitedSimR",
                    results = simulatedResults,
                    averageTrainingPointsHome = sum(simulatedResults$tpHome * simulatedResults$probability),
                    averageTrainingPointsAway = sum(simulatedResults$tpAway * simulatedResults$probability),
                    averagePointsHome = sum(simulatedResults$pointsHome * simulatedResults$probability),
                    averagePointsAway = sum(simulatedResults$pointsAway * simulatedResults$probability),
                    winProbabilityHome = sum((simulatedResults$pointsHome == 3) * simulatedResults$probability),
                    winProbabilityAway = sum((simulatedResults$pointsAway == 3) * simulatedResults$probability),
                    tiedProbability = sum((simulatedResults$pointsAway == 1) * simulatedResults$probability),
                    averageRedCardsHome =  sum(simulatedResults$redCardsHome * simulatedResults$probability),
                    averageRedCardsAway =  sum(simulatedResults$redCardsAway * simulatedResults$probability),
                    r = r,
                    home = home,
                    away = away)
    }


    return(output)
  }
}
