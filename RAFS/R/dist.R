# NULL defaults are used around here to allow calling without ignored params,
# while preserving the full accepted interface.

# Symmetric Target Information Gain (STIG) variants

#' Symmetric Target Information Gain (STIG) computed directly
#'
#' To be used as one of the \code{dist_funs} in \code{\link{run_rafs}}.
#'
#' This function computes the STIG metric directly from the data, maximising it over 30 discretisations.
#'
#' @param relevant_train_data input data where columns are variables and rows are observations (all numeric); assumed to contain only relevant data
#' @param train_decision decision variable as a binary sequence of length equal to number of observations
#' @param seed a numerical seed
#' @return A matrix of distances (dissimilarities).
#' @importFrom MDFS ComputeInterestingTuples GetRange
#' @export
stig_dist <- function(relevant_train_data, train_decision, seed) {
  # to ensure higher fidelity (the same as for VI) as we don't need the original statistic properties of IG
  range <- GetRange(n = nrow(relevant_train_data), dimensions = 2, divisions = 1)

  ComputeInterestingTuples(
    relevant_train_data,
    train_decision,
    dimensions = 2,
    divisions = 1,
    discretizations = 30,
    seed = seed,
    range = range,
    return.matrix = TRUE,
    stat_mode = "VI"
  )
}


#' Symmetric Target Information Gain (STIG) computed directly but with pre-computed 1D conditional entropy (aka stable)
#'
#' To be used as one of the \code{dist_funs} in \code{\link{run_rafs}}.
#'
#' This function computes the STIG metric directly from the data, maximising it over 30 discretisations, but reusing
#' the common 1D conditional entropy.
#'
#' @param relevant_train_data input data where columns are variables and rows are observations (all numeric); assumed to contain only relevant data
#' @param train_decision decision variable as a binary sequence of length equal to number of observations
#' @param seed a numerical seed
#' @return A matrix of distances (dissimilarities).
#' @importFrom MDFS ComputeInterestingTuples ComputeMaxInfoGains GetRange
#' @export
stig_stable_dist <- function(relevant_train_data, train_decision, seed) {
  # to ensure higher fidelity (the same as for VI) as we don't need the original statistic properties of IG
  range <- GetRange(n = nrow(relevant_train_data), dimensions = 2, divisions = 1)

  results_1d <- ComputeMaxInfoGains(
    relevant_train_data,
    train_decision,
    dimensions = 1,
    divisions = 1,
    discretizations = 30,
    seed = seed,
    range = range,
    pc.xi = 0.25
  )

  ComputeInterestingTuples(
    relevant_train_data,
    train_decision,
    dimensions = 2,
    divisions = 1,
    discretizations = 30,
    seed = seed,
    range = range,
    return.matrix = TRUE,
    stat_mode = "VI",
    I.lower = results_1d$IG,
    pc.xi = 0.125
  )
}


#' Symmetric Target Information Gain (STIG) computed from single Information Gains (IGs)
#'
#' To be used as one of the \code{dist_funs} in \code{\link{run_rafs}}.
#'
#' This function computes the STIG metric from single Information Gains (IGs) maximised over 30 discretisations and then summed pair-wise.
#'
#' This function is similar to \code{\link{stig_dist}} but the results differ slightly. We recommend the direct computation in general.
#'
#' @param relevant_train_data input data where columns are variables and rows are observations (all numeric); assumed to contain only relevant data
#' @param train_decision decision variable as a binary sequence of length equal to number of observations
#' @param seed a numerical seed
#' @return A matrix of distances (dissimilarities).
#' @importFrom MDFS ComputeInterestingTuples GetRange
#' @export
stig_from_ig_dist <- function(relevant_train_data, train_decision, seed) {
  # to ensure higher fidelity (the same as for VI) as we don't need the original statistic properties of IG
  range <- GetRange(n = nrow(relevant_train_data), dimensions = 2, divisions = 1)

  x <- ComputeInterestingTuples(
    relevant_train_data,
    train_decision,
    dimensions = 2,
    divisions = 1,
    discretizations = 30,
    seed = seed,
    range = range,
    return.matrix = TRUE,
    stat_mode = "MI"
  )

  x + t(x)
}


#' Variation of Information (VI)
#'
#' To be used as one of the \code{dist_funs} in \code{\link{run_rafs}}.
#'
#' This function computes the Variation of Information (VI) averaged over 30 discretisations.
#'
#' @param relevant_train_data input data where columns are variables and rows are observations (all numeric); assumed to contain only relevant data
#' @param train_decision decision variable as a binary sequence of length equal to number of observations
#' @param seed a numerical seed
#' @return A matrix of distances (dissimilarities).
#' @importFrom MDFS ComputeInterestingTuples
#' @export
vi_dist <- function(relevant_train_data, train_decision = NULL, seed) {
  ComputeInterestingTuples(
    relevant_train_data,
    dimensions = 2,
    divisions = 1,
    discretizations = 30,
    seed = seed,
    return.matrix = TRUE,
    stat_mode = "VI",
    average = TRUE
  )
}

#' Feature dissimilarity based on Pearson's Correlation (cor)
#'
#' To be used as one of the \code{dist_funs} in \code{\link{run_rafs}}.
#'
#' @param relevant_train_data input data where columns are variables and rows are observations (all numeric); assumed to contain only relevant data
#' @param train_decision decision variable as a binary sequence of length equal to number of observations
#' @param seed a numerical seed
#' @return A matrix of distances (dissimilarities).
#' @importFrom stats cor
#' @export
cor_dist <- function(relevant_train_data, train_decision = NULL, seed = NULL) {
  1 - cor(relevant_train_data)^2
}
