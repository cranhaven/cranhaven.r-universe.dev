#' Overview over the parameters used in the \code{unitedR} package
#' 
#' This list of parameters yields a comprehensive overview of the parameters
#' used in the \code{unitedR} package.
#'
#' @param away away team (an object of the \code{S4}class \code{formation})
#' @param chancesAway goalscoring chances of away team
#' @param chancesHome goalscoring chances of home team
#' @param DF \code{numeric} vector for the strengths of the players in the 
#' defense
#' @param formation object of the \code{S4}class \code{formation}
#' @param GK \code{integer} for the strength goalkeeper
#' @param hardness \code{numeric} vector of length five with integers for the used hardness
#' @param hardnessMatrix \code{matrix} matrix with eleven columns which contain the probability 
#' for yellow cards dependent on the used hardness
#' @param home home team (an object of the \code{S4}class \code{formation})
#' @param homeAdv \code{numeric} vector of length five with integers for the used hardness
#' @param L \code{list} with elements of class \code{formation}
#' @param MF \code{numeric} vector for the strengths of the players in the 
#' midfield
#' @param overtime \code{logical}, if True overtime win probabilites are calculated. Only
#' available if total hardness is zero or one.
#' @param penaltyGoalProb probability of a goal by a singular penalty
#' @param penaltyProb occurrence probability of a penalty
#' @param posPenalties number of possible penalties in a game
#' @param preventGoalGK factor multiplicied with the strength of the GK for computing the 
#' probability of preventing a goal by the goalkeeper
#' @param preventGoalSW factor multiplicied with the strength of the SW for computing the 
#' probability of preventing a goal by the sweeper
#' @param probGoalAway probability of scoring a goal for away team
#' @param probGoalHome probability of scoring a goal for home team
#' @param probPenaltySaveAway probability of saving a penalty for away team
#' @param probPenaltySaveHome probability of saving a penalty for home team 
#' @param r number of replications for the simulation of hardness and penalties, can 
#' be \code{missing} (exact results will be computed)
#' @param ST \code{numeric} vector of integers for the strenghts of the strikers
#' @param SW \code{vector} for the strength of the sweeper, can be 
#' \code{NA} or a \code{numeric}
#' @param x a variable \code{x}.
#'
#' @name overview
NULL