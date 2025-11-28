#' Construct matrix of prior limits
#'
#' Takes as input the minimum and maximum values of the prior distribution for
#' all relevant parameters and constructs a matrix of prior limits.
#'
#' The output matrix contains all parameters of a given model and, for each
#' parameter, it contains the minimum and maximum value of the prior.
#'
#' @param model a character, either 2pops", "Single" or "Parallel" indicating
#'   which model was simulated.
#' @param inputParams A vector containing the minimum and maximum values of the
#'   prior distribution for each parameter in the model. The input of the
#'   \code{CreateParameters} function can be converted into a vector and used
#'   here.
#'
#' @return a matrix where each row is a different parameter. Note also that each
#'   row is named after the corresponding parameter. For each row, the first
#'   column contains the minimum value of that parameter and the second column
#'   contains the maximum value.
#'
#' @examples
#' # create a vector of input parameters for a model with two populations
#' inputs <- c(Nref = c(25000, 25000), ratio = c(0.1, 3), pool = c(5, 250), seq = c(0.0001, 0.001),
#' split = c(0, 3), CW = c(1e-13, 1e-3), WC = c(1e-13, 1e-3), bT = c(0, 0.2))
#'
#' # construct a matrix with the limits of the prior distribution
#' priorsMatrix(model = "2pops", inputParams = inputs)
#'
#' @export
priorsMatrix <- function(model, inputParams) {

  # check if the input is correct - the selected model should be one of the following
  if(model %in% c("Single", "Parallel", "2pops") == FALSE)
    stop(paste("The selected model should be either Single, Parallel or 2pops. Please check"))

  # if model is Single or Parallel
  if(model %in% c("Single", "Parallel") == TRUE) {

    # set the number of populations to 6
    nPops <- 6
    # set the number of splits to 2
    nSplits <- 2

  } else {

    # set the number of populations to 2
    nPops <- 2
    # set the number of splits to 1
    nSplits <- 1
  }

  # create two vectors, one with the min and another with the max value of the prior distribution for the population sizes
  # there is a correction applied to the minimum value, otherwise NA values are produced when transforming the data
  minPop <- rep.int(x = (inputParams["ratio1"] - 0.001), times = nPops)
  maxPop <- rep.int(x = inputParams["ratio2"], times = nPops)

  # create two vectors, one with the minimum and another with the maximum value of the prior distribution for the split times
  minSplits <- rep.int(x = inputParams["split1"], times = nSplits)
  maxSplits <- rep.int(x = inputParams["split2"], times = nSplits)

  # combine all the minimum values, so far, from the various parameters into a single vector
  minPriors <- unname(c(inputParams["Nref1"], minPop, minSplits, inputParams["pool1"], inputParams["seq1"]))
  # combine all the maximum values, so far, from the various parameters into a single vector
  maxPriors <- unname(c(inputParams["Nref2"], maxPop, maxSplits, inputParams["pool2"], inputParams["seq2"]))

  # add the the minimum values of the migration between divergent ecotypes
  minPriors <- unname(c(minPriors, inputParams["CW1"], inputParams["WC1"]))
  # add the the maximum values of the migration between divergent ecotypes
  maxPriors <- unname(c(maxPriors, inputParams["CW2"], inputParams["WC2"]))

  # stop the function if we are working with the simple 2 pops model - only requires 10 parameters
  if(model == "2pops") {

    # assume that the proportion of the genome with unrestricted migration is 1 - proportion without migration
    # this is the minimum proportion of the genome with unrestricted migration
    minNoBarrier <- 1 - (inputParams["bT2"] + 0.001)
    # the maximum proportion of the genome with unrestricted migration is:
    maxNoBarrier <- 1 - inputParams["bT1"]

    # set the minimum value of the total barrier against gene flow
    min_bT <- inputParams["bT1"] - 0.001
    # set the maximum value of the total barrier against gene flow
    max_bT <- inputParams["bT2"] + 0.001

    # combine all the minimum values, so far, from the various parameters into a single vector
    minPriors <- unname(c(minPriors, minNoBarrier, min_bT))
    # combine all the maximum values, so far, from the various parameters into a single vector
    maxPriors <- unname(c(maxPriors, maxNoBarrier + 0.001, max_bT))

    # Create a matrix with the minimums in one column and the maximums in the other column for this particular model
    priors <- cbind(minPriors, maxPriors)
    # add names to the rows
    rownames(priors) <- c("Nref", "N1", "N2", "Split", "Pool_Error", "Error", "mCW", "mWC", "pM", "pNO")
    # stop the function and output the priors matrix
    stop(return(priors))
  }

  # create two vectors, one with the minimum and another with the maximum value
  # of the prior distribution for proportion of the genome without migration
  minNoBarrier <- (1 - inputParams["bT2"])*(1-(inputParams["bCW2"] + inputParams["bWC2"])) - 0.001
  maxNoBarrier <- (1 - inputParams["bT1"])*(1-(inputParams["bCW1"] + inputParams["bWC1"])) + 0.001

  # create two vectors, one with the minimum and another with the maximum value
  # of the prior distribution for proportion of the genome without migration from the crab to the wave ecotype
  minBarrierCW <- (1 - inputParams["bT1"])*inputParams["bCW1"]
  maxBarrierCW <- (1 - inputParams["bT1"])*inputParams["bCW2"]

  # create two vectors, one with the minimum and another with the maximum value
  # of the prior distribution for proportion of the genome without migration from the wave to the crab ecotype
  minBarrierWC <- (1 - inputParams["bT1"])*inputParams["bWC1"]
  maxBarrierWC <- (1 - inputParams["bT1"])*inputParams["bWC2"]

  # combine all the minimum values, so far, for the barriers against gene flow
  minBarriers <- unname(c(minNoBarrier, minBarrierCW, minBarrierWC))
  # combine all the maximum values, so far, for the barriers against gene flow
  maxBarriers <- unname(c(maxNoBarrier, maxBarrierCW, maxBarrierWC))

  # combine all the minimum values, so far, from the various parameters into a single vector
  minPriors <- unname(c(minPriors, inputParams["CC1"], inputParams["WW1"]))
  # combine all the maximum values, so far, from the various parameters into a single vector
  maxPriors <- unname(c(maxPriors, inputParams["CC2"], inputParams["WW2"]))

  # set the minimum value of the total barrier against gene flow
  min_bT <- inputParams["bT1"] - 0.001
  # set the minimum value of the total barrier against gene flow
  max_bT <- inputParams["bT2"] + 0.001

  # combine all the minimum values from the various parameters into a single vector
  minPriors <- unname(c(minPriors, inputParams["ANC1"], minBarriers, min_bT))

  # combine all the maximum values from the various parameters into a single vector
  maxPriors <- unname(c(maxPriors, inputParams["ANC2"], maxBarriers, max_bT))

  # create a matrix with the minimums in one column and the maximums in the other column for this particular model
  priors <- cbind(minPriors, maxPriors)

  # add names to the rows
  rownames(priors) <- c("Nref", "N1", "N2", "N3", "N4", "NA1", "NA2", "Split", "Dsplit", "Pool_Error", "Error",
                        "mCW", "mWC", "mCC", "mWW", "mAA", "pM", "pCW", "pWC", "pNO")

  # output the priors matrix
  priors
}


#' Compute scaled migration rates
#'
#' Computes and adds scaled migration rates to a matrix of simulated parameter
#' values.
#'
#' Migration rates are scaled according to the size of the population receiving
#' the migrants and added to a matrix with the simulated parameter values. This
#' is performed for the three available models and according to the specific
#' model conformation.
#'
#' @param model a character, either 2pops", "Single" or "Parallel" indicating
#'   which model was simulated.
#' @param Nref a numeric value indicating the effective population size of the
#'   reference population.
#' @param parameters is a matrix of simulated parameter values i.e. numbers from
#'   the simulations. Each row or vector entry should be a different simulation
#'   and each column of a matrix should be a different parameter.
#'
#' @return a matrix of simulated parameter values with added columns containing
#'   the scaled migration rates.
#'
#' @examples
#' # compute scaled migration for a two-population model
#' scaled.migration(parameters = myparams, model = "2pops", Nref = 10000)
#'
#' @export
scaled.migration <- function(parameters, model, Nref = NA) {

  # check if the input is correct - the selected model should be one of the following
  if(model %in% c("Single", "Parallel", "2pops") == FALSE)
    stop(paste("The selected model should be either Single, Parallel or 2pops. Please check"))

  # if no Nref is supplied as input, the function assumes that a column named "Ne" is present on the parameters matrix
  if(is.na(Nref))
    Nref <- parameters[, "Nref"]

  # if we have a two-populations model
  if(model == "2pops") {

    # compute the migration rate, corrected according to the size of the population receiving the migrants
    mig_CW <- 4*(Nref*parameters[, "N2"])*parameters[, "mCW"] # -m 2 1 mCW
    # compute the migration rate in the other direction - a different population is receiving the migrants
    mig_WC <- 4*(Nref*parameters[, "N1"])*parameters[, "mWC"] # -m 1 2 mWC

    # add the scaled migration rates to the parameters matrix - as columns
    parameters <- cbind(parameters, mig_CW, mig_WC)
    # stop the function and output the parameters vector
    stop(return(parameters))
  }

  # if model is Single
  if(model == "Single") {

    # compute scaled migration rates for the first site
    # compute the migration rate, corrected according to the size of the population receiving the migrants
    mig_CW1 <- 4*(Nref*parameters[, "N3"])*parameters[, "mCW1"] # -m 3 1 mCW
    # compute the migration rate in the other direction - a different population is receiving the migrants
    mig_WC1 <- 4*(Nref*parameters[, "N1"])*parameters[, "mWC1"] # -m 1 3 mWC

    # compute scaled migration rates for the second site
    # compute the migration rate, corrected according to the size of the population receiving the migrants
    mig_CW2 <- 4*(Nref*parameters[, "N4"])*parameters[, "mCW2"] # -m 4 2 mCW
    # compute the migration rate in the other direction - a different population is receiving the migrants
    mig_WC2 <- 4*(Nref*parameters[, "N2"])*parameters[, "mWC2"] # -m 2 4 mWC

    # compute scaled migration rates between the C ecotypes in different locations
    # compute the migration rate, corrected according to the size of the population receiving the migrants
    mig_CC1 <- 4*(Nref*parameters[, "N1"])*parameters[, "mCC"] # -m 1 2 mig_CC
    # compute the migration rate in the other direction - a different population is receiving the migrants
    mig_CC2 <- 4*(Nref*parameters[, "N2"])*parameters[, "mCC"] # -m 2 1 mig_CC

    # compute scaled migration rates between the W ecotypes in different locations
    # compute the migration rate, corrected according to the size of the population receiving the migrants
    mig_WW3 <- 4*(Nref*parameters[, "N3"])*parameters[, "mWW"] # -m 3 4 mig_WW
    # compute the migration rate in the other direction - a different population is receiving the migrants
    mig_WW4 <- 4*(Nref*parameters[, "N4"])*parameters[, "mWW"] # -m 4 3 mig_WW

    # compute scaled migration rates between the ancestral populations
    # compute the migration rate, corrected according to the size of the population receiving the migrants
    mig_AA1 <- 4*(Nref*parameters[, "NA1"])*parameters[, "mAA"] # -em tmAA 2 3 mig_AA
    # compute the migration rate in the other direction - a different population is receiving the migrants
    mig_AA2 <- 4*(Nref*parameters[, "NA2"])*parameters[, "mAA"] # -em tmAA 3 2 mig_AA

    # add the scaled migration rates to the parameters matrix - as columns
    parameters <- cbind(parameters, mig_CW1, mig_WC1, mig_CW2, mig_WC2, mig_CC1, mig_CC1, mig_WW3, mig_WW4, mig_AA1, mig_AA2)
    # stop the function and output the parameters vector
    stop(return(parameters))
  }

  # if model is Parallel
  if(model == "Parallel") {

    # compute scaled migration rates for the first site
    # compute the migration rate, corrected according to the size of the population receiving the migrants
    mig_CW1 <- 4*(Nref*parameters[, "N2"])*parameters[, "mCW1"] # -m 2 1 mCW
    # compute the migration rate in the other direction - a different population is receiving the migrants
    mig_WC1 <- 4*(Nref*parameters[, "N1"])*parameters[, "mWC1"] # -m 1 2 mWC

    # compute scaled migration rates for the second site
    # compute the migration rate, corrected according to the size of the population receiving the migrants
    mig_CW2 <- 4*(Nref*parameters[, "N4"])*parameters[, "mCW2"] # -m 4 3 mCW
    # compute the migration rate in the other direction - a different population is receiving the migrants
    mig_WC2 <- 4*(Nref*parameters[, "N3"])*parameters[, "mWC2"] # -m 3 4 mWC

    # compute scaled migration rates between the C ecotypes in different locations
    # compute the migration rate, corrected according to the size of the population receiving the migrants
    mig_CC1 <- 4*(Nref*parameters[, "N1"])*parameters[, "mCC"] # -m 1 3 mig_CC
    # compute the migration rate in the other direction - a different population is receiving the migrants
    mig_CC3 <- 4*(Nref*parameters[, "N3"])*parameters[, "mCC"] # -m 3 1 mig_CC

    # compute scaled migration rates between the W ecotypes in different locations
    # compute the migration rate, corrected according to the size of the population receiving the migrants
    mig_WW2 <- 4*(Nref*parameters[, "N2"])*parameters[, "mWW"] # -m 2 4 mig_WW
    # compute the migration rate in the other direction - a different population is receiving the migrants
    mig_WW4 <- 4*(Nref*parameters[, "N4"])*parameters[, "mWW"] # -m 4 2 mig_WW

    # compute scaled migration rates between the ancestral populations
    # compute the migration rate, corrected according to the size of the population receiving the migrants
    mig_AA1 <- 4*(Nref*parameters[, "NA1"])*parameters[, "mAA"] # -em tmAA 2 3 mig_AA
    # compute the migration rate in the other direction - a different population is receiving the migrants
    mig_AA2 <- 4*(Nref*parameters[, "NA2"])*parameters[, "mAA"] # -em tmAA 3 2 mig_AA

    # add the scaled migration rates to the parameters matrix - as columns
    parameters <- cbind(parameters, mig_CW1, mig_WC1, mig_CW2, mig_WC2, mig_CC1, mig_CC1, mig_WW3, mig_WW4, mig_AA1, mig_AA2)
    # stop the function and output the parameters vector
    stop(return(parameters))
  }
}


#' Compute scaled migration rate limits
#'
#' Computes and adds scaled migration rates to a matrix with the limits of the
#' prior distributions.
#'
#' Migration rates are scaled according to the size of the population receiving
#' the migrants and added to a matrix with the prior limits. The minimum and
#' maximum possible size of the population and of the migration rate are used to
#' compute the minimum and maximum possible values of the scaled migration
#' rates. This is performed for the three available models and according to the
#' specific model conformation.
#'
#' @param limits is a matrix with two columns and as many rows as there are
#'   parameters. Each row should contain the minimum value of the prior for a
#'   given parameter in the first column and the maximum value in the second
#'   column.
#' @param model a character, either 2pops", "Single" or "Parallel" indicating
#'   which model was simulated.
#' @param Nref a numeric value indicating the effective population size of the
#'   reference population.
#'
#' @return a matrix where each row is a different parameter. This matrix is
#'   similar to the input argument `limits` but with added rows containing the
#'   scaled migration rates.
#'
#' @examples
#' # create a vector of input parameters for a model with two populations
#' inputs <- c(Nref = c(25000, 25000), ratio = c(0.1, 3), pool = c(5, 250), seq = c(0.0001, 0.001),
#' split = c(0, 3), CW = c(1e-13, 1e-3), WC = c(1e-13, 1e-3), bT = c(0, 0.2))
#'
#' # construct a matrix with the limits of the prior distribution
#' limits <- priorsMatrix(model = "2pops", inputParams = inputs)
#'
#' # compute and add the prior limits of the scaled migration
#' scaledPrior(limits = limits, model = "2pops")
#'
#' @export
scaledPrior <- function(limits, model, Nref = NA) {

  if(is.na(Nref)) {

    # get the minimum value of the Ne parameter
    min.Nref <- limits["Nref", "minPriors"]
    # get the maximum value of the Ne parameter
    max.Nref <- limits["Nref", "maxPriors"]

  } else { # if Nref is not NA

    # if Nref is a vector
    if(length(Nref) == 2) {
      # get the minimum value
      min.Nref <- Nref[1]
      # and the maximum value
      max.Nref <- Nref[2]

    } else { # if Nref is a single value

      # get the minimum value
      min.Nref <- Nref
      # and the maximum value
      max.Nref <- Nref
    }
  }

  if(model == "2pops") {

    # compute the minimum scaled migration rate, according to the size of the population receiving the migrants
    min.mig_CW <- 4*(min.Nref*limits["N2", "minPriors"])*limits["mCW", "minPriors"] # -m 2 1 mCW
    # compute the minimum scaled migration rate in the other direction - a different population is receiving the migrants
    min.mig_WC <- 4*(min.Nref*limits["N1", "minPriors"])*limits["mWC", "minPriors"] # -m 1 2 mWC

    # compute the minimum scaled migration rate, according to the size of the population receiving the migrants
    max.mig_CW <- 4*(max.Nref*limits["N2", "maxPriors"])*limits["mCW", "maxPriors"] # -m 2 1 mCW
    # compute the minimum scaled migration rate in the other direction - a different population is receiving the migrants
    max.mig_WC <- 4*(max.Nref*limits["N1", "maxPriors"])*limits["mWC", "maxPriors"] # -m 1 2 mWC

    # add the scaled migration rates to the priors matrix - as rows
    priors <- rbind(limits, mig_CW = c(min.mig_CW, max.mig_CW), mig_WC = c(min.mig_WC, max.mig_WC))
    # stop the function and output the priors matrix
    stop(return(priors))
  }

  if(model == "Parallel") {

    # for the first location
    # compute the minimum scaled migration rate, according to the size of the population receiving the migrants
    min.mig_CW1 <- 4*(min.Nref*limits["N2", "minPriors"])*limits["mCW", "minPriors"] # -m 2 1 mCW
    # compute the minimum scaled migration rate in the other direction - a different population is receiving the migrants
    min.mig_WC1 <- 4*(min.Nref*limits["N1", "minPriors"])*limits["mWC", "minPriors"] # -m 1 2 mWC
    # compute the maximum scaled migration rate, according to the size of the population receiving the migrants
    max.mig_CW1 <- 4*(max.Nref*limits["N2", "maxPriors"])*limits["mCW", "maxPriors"] # -m 2 1 mCW
    # compute the maximum scaled migration rate in the other direction - a different population is receiving the migrants
    max.mig_WC1 <- 4*(max.Nref*limits["N1", "maxPriors"])*limits["mWC", "maxPriors"] # -m 1 2 mWC

    # for the second location
    # compute the minimum scaled migration rate, according to the size of the population receiving the migrants
    min.mig_CW2 <- 4*(min.Nref*limits["N4", "minPriors"])*limits["mCW", "minPriors"] # -m 4 3 mCW
    # compute the minimum scaled migration rate in the other direction - a different population is receiving the migrants
    min.mig_WC2 <- 4*(min.Nref*limits["N3", "minPriors"])*limits["mWC", "minPriors"] # -m 3 4 mWC
    # compute the maximum scaled migration rate, according to the size of the population receiving the migrants
    max.mig_CW2 <- 4*(max.Nref*limits["N4", "maxPriors"])*limits["mCW", "maxPriors"] # -m 4 3 mCW
    # compute the maximum scaled migration rate in the other direction - a different population is receiving the migrants
    max.mig_WC2 <- 4*(max.Nref*limits["N3", "maxPriors"])*limits["mWC", "maxPriors"] # -m 3 4 mWC

    # compute scaled migration rates between the C ecotypes in different locations
    # compute the minimum scaled migration rate, according to the size of the population receiving the migrants
    min.mig_CC1 <- 4*(min.Nref*limits["N1", "minPriors"])*limits["mCC", "minPriors"] # -m 1 3 mig_CC
    # compute the minimum scaled migration rate in the other direction - a different population is receiving the migrants
    min.mig_CC3 <- 4*(min.Nref*limits["N3", "minPriors"])*limits["mCC", "minPriors"] # -m 3 1 mig_CC
    # compute the maximum scaled migration rate, according to the size of the population receiving the migrants
    max.mig_CC1 <- 4*(max.Nref*limits["N1", "maxPriors"])*limits["mCC", "maxPriors"] # -m 1 3 mig_CC
    # compute the maximum scaled migration rate in the other direction - a different population is receiving the migrants
    max.mig_CC3 <- 4*(max.Nref*limits["N3", "maxPriors"])*limits["mCC", "maxPriors"] # -m 3 1 mig_CC

    # compute scaled migration rates between the W ecotypes in different locations
    # compute the minimum scaled migration rate, according to the size of the population receiving the migrants
    min.mig_WW2 <- 4*(min.Nref*limits["N2", "minPriors"])*limits["mWW", "minPriors"] # -m 2 4 mig_WW
    # compute the minimum scaled migration rate in the other direction - a different population is receiving the migrants
    min.mig_WW4 <- 4*(min.Nref*limits["N4", "minPriors"])*limits["mWW", "minPriors"] # -m 4 2 mig_WW
    # compute the maximum scaled migration rate, according to the size of the population receiving the migrants
    max.mig_WW2 <- 4*(max.Nref*limits["N2", "maxPriors"])*limits["mWW", "maxPriors"] # -m 2 4 mig_WW
    # compute the maximum scaled migration rate in the other direction - a different population is receiving the migrants
    max.mig_WW4 <- 4*(max.Nref*limits["N4", "maxPriors"])*limits["mWW", "maxPriors"] # -m 4 2 mig_WW

    # compute scaled migration rates between the ancestral populations
    # compute the minimum scaled migration rate, according to the size of the population receiving the migrants
    min.mig_AA1 <- 4*(min.Nref*limits["NA1", "minPriors"])*limits["mAA", "minPriors"] # -em tmAA 2 3 mig_AA
    # compute the minimum scaled migration rate in the other direction - a different population is receiving the migrants
    min.mig_AA2 <- 4*(min.Nref*limits["NA2", "minPriors"])*limits["mAA", "minPriors"] # -em tmAA 3 2 mig_AA
    # compute the maximum scaled migration rate, according to the size of the population receiving the migrants
    max.mig_AA1 <- 4*(max.Nref*limits["NA1", "maxPriors"])*limits["mAA", "maxPriors"] # -em tmAA 2 3 mig_AA
    # compute the maximum scaled migration rate in the other direction - a different population is receiving the migrants
    max.mig_AA2 <- 4*(max.Nref*limits["NA2", "maxPriors"])*limits["mAA", "maxPriors"] # -em tmAA 3 2 mig_AA

    # create a matrix with the scaled migration rates
    scaled <- rbind(mig_CW1 = c(min.mig_CW1, max.mig_CW1), mig_WC1 = c(min.mig_WC1, max.mig_WC1), mig_CW2 = c(min.mig_CW2, max.mig_CW2),
                    mig_WC2 = c(min.mig_WC2, max.mig_WC2), mig_CC1 = c(min.mig_CC1, max.mig_CC1), mig_CC3 = c(min.mig_CC3, max.mig_CC3),
                    mig_WW2 = c(min.mig_WW2, max.mig_WW2), mig_WW4 = c(min.mig_WW4, max.mig_WW4), mig_AA1 = c(min.mig_AA1, max.mig_AA1),
                    mig_AA2 = c(min.mig_AA2, max.mig_AA2))

    # add the scaled migration rates to the priors matrix - as rows
    priors <- rbind(limits, scaled)
    # stop the function and output the priors matrix
    stop(return(priors))
  }

  if(model == "Single") {

    # for the first location
    # compute the minimum scaled migration rate, according to the size of the population receiving the migrants
    min.mig_CW1 <- 4*(min.Nref*limits["N3", "minPriors"])*limits["mCW", "minPriors"] # -m 3 1 mCW
    # compute the minimum scaled migration rate in the other direction - a different population is receiving the migrants
    min.mig_WC1 <- 4*(min.Nref*limits["N1", "minPriors"])*limits["mWC", "minPriors"] # -m 1 3 mWC
    # compute the maximum scaled migration rate, according to the size of the population receiving the migrants
    max.mig_CW1 <- 4*(max.Nref*limits["N3", "maxPriors"])*limits["mCW", "maxPriors"] # -m 3 1 mCW
    # compute the maximum scaled migration rate in the other direction - a different population is receiving the migrants
    max.mig_WC1 <- 4*(max.Nref*limits["N1", "maxPriors"])*limits["mWC", "maxPriors"] # -m 1 3 mWC

    # for the second location
    # compute the minimum scaled migration rate, according to the size of the population receiving the migrants
    min.mig_CW2 <- 4*(min.Nref*limits["N4", "minPriors"])*limits["mCW", "minPriors"] # -m 4 2 mCW
    # compute the minimum scaled migration rate in the other direction - a different population is receiving the migrants
    min.mig_WC2 <- 4*(min.Nref*limits["N2", "minPriors"])*limits["mWC", "minPriors"] # -m 2 4 mWC
    # compute the maximum scaled migration rate, according to the size of the population receiving the migrants
    max.mig_CW2 <- 4*(max.Nref*limits["N4", "maxPriors"])*limits["mCW", "maxPriors"] # -m 4 2 mCW
    # compute the maximum scaled migration rate in the other direction - a different population is receiving the migrants
    max.mig_WC2 <- 4*(max.Nref*limits["N2", "maxPriors"])*limits["mWC", "maxPriors"] # -m 2 4 mWC

    # compute scaled migration rates between the C ecotypes in different locations
    # compute the minimum scaled migration rate, according to the size of the population receiving the migrants
    min.mig_CC1 <- 4*(min.Nref*limits["N1", "minPriors"])*limits["mCC", "minPriors"] # -m 1 2 mig_CC
    # compute the minimum scaled migration rate in the other direction - a different population is receiving the migrants
    min.mig_CC2 <- 4*(min.Nref*limits["N2", "minPriors"])*limits["mCC", "minPriors"] # -m 2 1 mig_CC
    # compute the maximum scaled migration rate, according to the size of the population receiving the migrants
    max.mig_CC1 <- 4*(max.Nref*limits["N1", "maxPriors"])*limits["mCC", "maxPriors"] # -m 1 2 mig_CC
    # compute the maximum scaled migration rate in the other direction - a different population is receiving the migrants
    max.mig_CC2 <- 4*(max.Nref*limits["N2", "maxPriors"])*limits["mCC", "maxPriors"] # -m 2 1 mig_CC

    # compute scaled migration rates between the W ecotypes in different locations
    # compute the minimum scaled migration rate, according to the size of the population receiving the migrants
    min.mig_WW3 <- 4*(min.Nref*limits["N3", "minPriors"])*limits["mWW", "minPriors"] # -m 3 4 mig_WW
    # compute the minimum scaled migration rate in the other direction - a different population is receiving the migrants
    min.mig_WW4 <- 4*(min.Nref*limits["N4", "minPriors"])*limits["mWW", "minPriors"] # -m 4 3 mig_WW
    # compute the maximum scaled migration rate, according to the size of the population receiving the migrants
    max.mig_WW3 <- 4*(max.Nref*limits["N3", "maxPriors"])*limits["mWW", "maxPriors"] # -m 3 4 mig_WW
    # compute the maximum scaled migration rate in the other direction - a different population is receiving the migrants
    max.mig_WW4 <- 4*(max.Nref*limits["N4", "maxPriors"])*limits["mWW", "maxPriors"] # -m 4 3 mig_WW

    # compute scaled migration rates between the ancestral populations
    # compute the minimum scaled migration rate, according to the size of the population receiving the migrants
    min.mig_AA1 <- 4*(min.Nref*limits["NA1", "minPriors"])*limits["mAA", "minPriors"] # -em tmAA 2 3 mig_AA
    # compute the minimum scaled migration rate in the other direction - a different population is receiving the migrants
    min.mig_AA2 <- 4*(min.Nref*limits["NA2", "minPriors"])*limits["mAA", "minPriors"] # -em tmAA 3 2 mig_AA
    # compute the maximum scaled migration rate, according to the size of the population receiving the migrants
    max.mig_AA1 <- 4*(max.Nref*limits["NA1", "maxPriors"])*limits["mAA", "maxPriors"] # -em tmAA 2 3 mig_AA
    # compute the maximum scaled migration rate in the other direction - a different population is receiving the migrants
    max.mig_AA2 <- 4*(max.Nref*limits["NA2", "maxPriors"])*limits["mAA", "maxPriors"] # -em tmAA 3 2 mig_AA

    # create a matrix with the scaled migration rates
    scaled <- rbind(mig_CW1 = c(min.mig_CW1, max.mig_CW1), mig_WC1 = c(min.mig_WC1, max.mig_WC1), mig_CW2 = c(min.mig_CW2, max.mig_CW2),
                    mig_WC2 = c(min.mig_WC2, max.mig_WC2), mig_CC1 = c(min.mig_CC1, max.mig_CC1), mig_CC3 = c(min.mig_CC3, max.mig_CC3),
                    mig_WW2 = c(min.mig_WW2, max.mig_WW2), mig_WW4 = c(min.mig_WW4, max.mig_WW4), mig_AA1 = c(min.mig_AA1, max.mig_AA1),
                    mig_AA2 = c(min.mig_AA2, max.mig_AA2))

    # add the scaled migration rates to the priors matrix - as rows
    priors <- rbind(limits, scaled)
    # stop the function and output the priors matrix
    stop(return(priors))
  }
}


#' Normalize data - adjust values measured on different scales
#'
#' Adjusts values that are measured over different scales.
#'
#' This function is used to scale the summary statistics and the target for ABC
#' inference. This scaling ensures that the different summary statistics are all
#' in the same scale when performing model selection or parameter inference.
#'
#' @param x is a numeric vector.
#' @param y is a numeric vector.
#'
#' @return a numeric vector with the same length as \code{x}.
#'
#' @keywords internal
#'
#' @export
normalise <- function(x, y) {

  # if the variance of y is equal to zero
  if(stats::var(y) == 0)
    return (x - mean(y)) # normalize by subtracting the mean of y from x
  else # if the variance of y is not equal to zero
    return ((x - (mean(y))) / sqrt(stats::var(y)))
  # normalize by subtracting the mean of y from x and dividing by the sqrt of the variance
}


#' Compute euclidean distance
#'
#' Computes the euclidean distance between two integers or numeric vectors.
#'
#' @param a is an integer or a numeric vector.
#' @param b is another integer or numeric vector.
#'
#' @return a numeric value representing the euclidean distance between the two
#'   inputs.
#'
#' @keywords internal
#'
#' @export
euclidean <- function(a, b) {

  # compute euclidean distance between the two vectors
  sqrt(sum((a - b)^2))
}


#' Apply a transformation to the parameters
#'
#' This function applies a transformation to the parameter values.
#'
#' The transformation should be applied before parameter estimation using an
#' Approximate Bayesian Computation framework to ensure that the estimates do
#' not fall outside the boundaries set by the prior distribution.
#'
#' @param x is the parameter vector (long vector of numbers from the
#'   simulations). These are the values to be transformed.
#' @param min is the minimum value of the prior for this parameter.
#' @param max is the maximum value of the prior for this parameter.
#'
#' @return a numeric vector with the same length as \code{x} with the parameter
#'   values transformed.
#'
#' @keywords internal
#'
#' @export
tranf <- function(x, min, max) {

  # transform the parameter values - change their scale
  -log(1/tan(((x-min)/(max-min))*(pi/2)))
}


#' Back-transform the parameters values
#'
#' This function applies a back-transformation to the parameter values.
#'
#' The back-transformation should be applied after parameter estimation using an
#' Approximate Bayesian Computation framework. It will return the parameter
#' values back to their original scale.
#'
#' @param y is the parameter vector (long vector of numbers from the
#'   simulations). These are the values to be back-transformed.
#' @param min is the minimum value of the prior for this parameter.
#' @param max is the maximum value of the prior for this parameter.
#'
#' @return a numeric vector with the same length as \code{y} with the parameter
#'   values back-transformed.
#'
#' @keywords internal
#'
#' @export
inverse_trans <- function(y, min, max) {

  # back-transform the parameter values - convert back to original scale
  ((2*(max-min)*atan(exp(y)))/pi)+min
}


#' Transform matrix of parameter values
#'
#' This function applies a transformation to the parameter values.
#'
#' The transformation should be applied before parameter estimation using an
#' Approximate Bayesian Computation framework to ensure that the estimates do
#' not fall outside the boundaries set by the prior distribution.
#'
#' @param original a matrix of parameter values i.e. numbers obtained from the
#'   simulations. Each column should be a different parameter and each row a
#'   different simulation. This is the matrix that you wish to transform so that
#'   the parameter values are all in the same scale.
#' @param limits is a matrix with two columns and as many rows as there are
#'   parameters. Each row should contain the minimum value of the prior for a
#'   given parameter in the first column and the maximum value in the second
#'   column.
#'
#' @return a matrix with the same dimensions as the \code{original} matrix but
#'   with the parameter values transformed.
#'
#' @keywords internal
#'
#' @export
Tmatrix <- function(original, limits) {

  # get the number of columns of the matrix to be transformed
  ncols <- ncol(original)

  # get the number of columns of the matrix to be transformed
  nrows <- nrow(original)

  # apply the tranf() function to the original matrix:
  # taking into account the min and max of the prior distributions
  transformed <- vapply(1:ncols, FUN = function(i)
    tranf(x = original[, i], min = limits[i,1], max = limits[i, 2]), FUN.VALUE = numeric(nrows))

  # output the transformed matrix
  transformed
}


#' Back-transform matrix of parameter values
#'
#' This function applies a back-transformation to the parameter values.
#'
#' The back-transformation should be applied after parameter estimation using an
#' Approximate Bayesian Computation framework. It will return the parameter
#' values back to their original scale.
#'
#' @param transformed a matrix of transformed parameter values. Each column
#'   should be a different parameter and each row a different simulation. This
#'   is the matrix that you wish to back-transform so that the parameter values
#'   are all in the original scale.
#' @param limits is a matrix with two columns and as many rows as there are
#'   parameters. Each row should contain the minimum value of the prior for a
#'   given parameter in the first column and the maximum value in the second
#'   column.
#'
#' @return a matrix with the same dimensions as the \code{transformed} matrix
#'   but with the parameter values back-transformed.
#'
#' @keywords internal
#'
#' @export
BTmatrix <- function(transformed, limits) {

  # get the number of columns of the matrix to be back-transformed
  ncols <- ncol(transformed)

  # get the number of columns of the matrix to be back-transformed
  nrows <- nrow(transformed)

  # apply the inverse_trans() function to the transformed matrix:
  # back transform the matrix - get back to the original scale of the values
  original <- vapply(1:ncols, FUN = function(i)
    inverse_trans(y = transformed[, i], min = limits[i,1], max = limits[i,2]), FUN.VALUE = numeric(nrows))

  # check if the transformed matrix had any column names
  if(is.null(colnames(transformed)) == FALSE)
    colnames(original) <- colnames(transformed) # if column names existed, add the same names to the original matrix

  # output the back transformed matrix
  original
}


#' Calculate the mode of a distribution
#'
#' Computes and outputs the mode of the input distribution.
#'
#' The [locfit::locfit()] function is used to fit a local regression to the
#' distribution. The [stats::predict()] function is then used to predict the
#' y-axis values of the locfit and the mode is defined as the value where that
#' prediction is maximized. Note that if this function is not able to fit a
#' local regression to the distribution, then the mode of the distribution will
#' be assumed to be equal to the median.
#'
#' @param x is a numeric vector containing the values of the distribution.
#' @param xlim is a vector with two entries.The first entry is the minimum of
#'   the \code{x} distribution and the second entry is the maximum value of the
#'   \code{x} distribution. Ideally these values should be the minimum and
#'   maximum value of the prior for this particular parameter.
#' @param weights this is an optional input consisting of a vector with the
#'   prior weights for the locfit function.
#' @param alpha numeric value with the alpha parameter of the locfit function.
#'   The default value is 0.7
#' @param precision value indicating the number of entries evaluated. The larger
#'   the value the higher the precision. The default value is 1000.
#'
#' @return a numeric value of the mode of the input distribution.
#'
#' @examples
#' # create a random distribution
#' x <- rnorm(n = 100, mean = 2, sd = 25)
#'
#' # compute the mode of the distribution
#' getmode(x = x, xlim = c(min(x), max(x)))
#'
#' @export
getmode <- function(x, xlim, weights = NULL, alpha = 0.7, precision = 1000) {

  # get the minimum and maximum of x
  min.x <- xlim[1]; max.x <- xlim[2]

  # when no weights were supplied
  if(is.null(weights)) {

    # estimate density using the locfit function - use the try method to catch any possible errors
    loc <- try(locfit::locfit(~x, alpha = alpha, xlim = c(min.x, max.x), maxk = 300), silent = TRUE)

  } else { # when weights are supplied as input

    # estimate density using the locfit function - use the try method to catch any possible errors
    loc <- try(locfit::locfit(~x, alpha = alpha, xlim = c(min.x, max.x), weights = weights, maxk = 300), silent = TRUE)
  }

  # if an error has occurred when using the locfit function
  if("try-error" %in% class(loc)) {
    # then assume that the mode is equal to the median
    mode <- stats::median(x)

  } else {

    # sequence of values at which the locfit density is evaluated
    aux <- seq(min.x, max.x, length.out = precision)
    # predict the y-axis values (density) at each aux value in x-axis
    predy <- stats::predict(loc, aux)
    # get the mode as the value where predy is maximized
    mode <- aux[which(predy == max(predy))]
  }

  # output the mode of the posterior distribution
  mode
}


#' Compute mode of a locfit object
#'
#' This function computes and outputs the the mode of a locfit object.
#'
#' The [stats::predict()] function is used to predict the y-axis values of the
#' locfit object and the mode is defined as the value where that prediction is
#' maximized.
#'
#' @param locx is a locfit object.
#' @param xlim is a vector with two entries.The first entry is the minimum of
#'   the distribution and the second entry is the maximum value of the
#'   distribution.
#' @param precision value indicating the number of entries evaluated. The larger
#'   the value the higher the precision. The default value is 1000.
#'
#' @return a numeric value of the mode of the input locfit object.
#'
#' @examples
#' # create a random distribution
#' x <- rnorm(n = 1000, mean = 2, sd = 25)
#'
#' # perform a local regression
#' loc <- locfit::locfit(~x)
#'
#' # compute the mode of the locfit object
#' mode_locfit(locx = loc, xlim = c(min(x), max(x)))
#'
#' @export
mode_locfit <- function(locx, xlim, precision = 1000) {

  # sequence of values at which the locfit density is evaluated
  aux <- seq(xlim[1],xlim[2], length.out = precision)
  # predict the y-axis values (density) at each aux value in x-axis
  predy <- stats::predict(locx, aux)
  # get the mode as the value where predy is maximized
  aux[which(predy == max(predy))]
}


#' Compute weighted point estimates
#'
#' Computes the weighted mean, median and quantiles of a distribution.
#'
#' This function requires the [MetricsWeighted::weighted_quantile()] function
#' and the weights to compute the weighted arithmetic mean and the weighted
#' quantiles. By default, this function computes the 5%, 25%, 50% (corresponding
#' to the median), 75% and 95% quantiles.
#'
#' @param x numeric vector of size n with the observations.
#' @param w numeric vector of size n with non-negative weights. Note that this
#'   vector needs to have the same length as the `x` vector.
#' @param prob numeric vector of probabilities with values in \code{[0,1]}.
#'
#' @return numeric vector with weighted mean, median and quantiles of size
#'   \code{2 + length(prob)}.
#'
#' @keywords internal
#'
#' @export
weighted_stats <- function(x, w, prob = c(0.05, 0.25, 0.75, 0.95)) {

  # compute the weighted mean
  wmean <- stats::weighted.mean(x = x, w = w)
  # and the weighted median
  wmedian <- MetricsWeighted::weighted_quantile(x = x, w = w, probs = 0.5)
  # compute also the weighted quantiles
  wq <- MetricsWeighted::weighted_quantile(x = x, w = w, probs = prob)
  # output the weighted point estimates
  c(wmean, wmedian, wq)
}


#' Remove columns with zero variance
#'
#' Removes summary statistics with zero variance.
#'
#' Checks the variance of the summary statistics in the observed data and
#' removes summary statistics with zero variance. Those summary statistics are
#' removed from both the matrix of observed values and the matrix of simulated
#' values.
#'
#' @param observed is a matrix of observed summary statistics. Each column
#'   should be a different summary statistic.
#' @param sumstats is a matrix of simulated summary statistics. Each column
#'   should be a different summary statistic and each row a different
#'   simulation.
#'
#' @return a list with two named entries. One entry contains the matrix of
#'   observed summary statistics and the other the simulated summary statistics.
#'
#' @keywords internal
#'
#' @export
removeVar <- function(observed, sumstats) {

  # check the variance of all sumstats in the data
  vars <- apply(X = observed, MARGIN = 2, stats::var)
  # sumstats with zero variance should be removed from the data
  toremove <- vars == 0
  # remove those sumstats from the matrix with the observed
  observed <- observed[, !toremove, drop = FALSE]
  # and remove also the same sumstats from the simulated sumstats
  sumstats <- sumstats[, !toremove, drop = FALSE]

  # output both matrices combined in a list
  list(observed = observed, sumstats = sumstats)
}


#' Parameter estimation with Approximate Bayesian Computation using rejection
#' sampling
#'
#' This function performs multivariate parameter estimation based on summary
#' statistics using an Approximate Bayesian Computation (ABC) algorithm. The
#' algorithm used here is the rejection sampling algorithm. The output of this
#' function can be tailored towards a posterior local linear regression method
#' correction.
#'
#' The rejection sampling algorithm generates random samples from the posterior
#' distributions of the parameters of interest. Note that to use this function,
#' the usual steps of ABC parameter estimation have to be performed. Briefly,
#' data should have been simulated based on random draws from the prior
#' distributions of the parameters of interest and a set of summary statistics
#' should have been calculated from that data. The same set of summary
#' statistics should have been calculated from the observed data to be used as
#' the `target` input in this function. Parameter values are accepted if the
#' Euclidean distance between the set of summary statistics computed from the
#' simulated data and the set of summary statistics computed from the observed
#' data is sufficiently small. The percentage of accepted simulations is
#' determined by `tol`.
#'
#' @param target a vector with the target summary statistics. These are usually
#'   the set of observed summary statistics.
#' @param params is a vector or matrix of simulated parameter values i.e.
#'   numbers from the simulations. Each row or vector entry should be a
#'   different simulation and each column of a matrix should be a different
#'   parameter.
#' @param sumstats is a vector or matrix of simulated summary statistics. Each
#'   row or vector entry should be a different simulation and each column of a
#'   matrix should be a different statistic.
#' @param tol is the tolerance rate, indicating the required proportion of
#'   points accepted nearest the target values.
#' @param regression logical, indicating whether the user intends to perform a
#'   local linear regression correction after the rejection step. If set to
#'   FALSE (default) the output of this function will contain just the results
#'   of the rejection step. If set to TRUE, the output will contain more details
#'   required for the regression step.
#'
#' @return a list with the results of the rejection sampling algorithm. The
#'   elements of the list depend of the logical value of `regression`.
#'
#'   \item{s.target}{a scaled vector of the observed summary statistics. This
#'   element only exists if regression is TRUE.}
#'
#'   \item{unadjusted}{parameter estimates obtained with the rejection
#'   sampling.}
#'
#'   \item{ss}{set of accepted summary statistics from the simulations.}
#'
#'   \item{s.sumstat}{set of scaled accepted summary statistics from the
#'   simulations. This element only exists if regression is TRUE.}
#'
#'   \item{dst}{euclidean distances in the region of interest.}
#'
#' @examples
#' # load the matrix with parameter values
#' data(params)
#' # load the matrix with simulated parameter values
#' data(sumstats)
#'
#' # select a random simulation to act as target just to test the function
#' target <- sumstats[10 ,]
#'
#' # Parameter estimation using rejection sampling
#' rejABC(target = target, params = params, sumstats = sumstats[-10, ], tol = 0.01)
#'
#' @export
rejABC <- function(target, params, sumstats, tol, regression = FALSE) {

  # get the number of summary statistics
  nss <- length(sumstats[1, ])

  # when dealing with a single parameter - in the form of a vector - convert that vector into a matrix with one column
  if(is.vector(params)) params <- matrix(params, ncol = 1)

  # checks that the function is used correctly
  if(length(target) != nss)
    stop("number of summary statistics in 'target' has to be the same as in 'sumstats")

  # stop if zero var in sumstat
  cond1 <- !any(as.logical(apply(sumstats, 2, function(x) length(unique(x))-1)))
  if(cond1) stop("zero variance in the summary statistics.")

  # scale everything
  # scale the matrix containing the summary statistics
  scaled.sumstat <- apply(X = sumstats, MARGIN = 2, FUN = function(col) normalise(x = col, y = col))
  # and the vector with the target
  scaled.target <- vapply(1:nss, FUN = function(i) normalise(target[i], sumstats[, i]), FUN.VALUE = numeric(1))

  # calculate euclidean distance
  dst <- apply(X = scaled.sumstat, MARGIN = 1, FUN = function(row) euclidean(a = row, b = scaled.target))

  # wt1 defines the region we're interested in
  # first define the threshold value
  abstol <- stats::quantile(x = dst, probs = tol)
  # create a vector of TRUE/FALSE indicating which values are below (or equal to) the threshold
  wt1 <- dst <= abstol

  if(regression == FALSE)
    # this function doesn't bother with the regression and just does the rejection method
    # create the output under the rejection method
    out <- list(unadjusted = params[wt1, ], ss = sumstats[wt1, ], dst = dst[wt1])
  else
    # create an output that can then be utilized in a posterior local linear regression algorithm
    out <- list(s.target = scaled.target, unadjusted = params[wt1, ], ss = sumstats[wt1, ],
                s.sumstat = scaled.sumstat[wt1, ], dst = dst[wt1])

  # output the list containing the results of the function
  out
}


#' Parameter estimation with Approximate Bayesian Computation using rejection
#' sampling and recording just the index of accepted simulations
#'
#' This function performs multivariate parameter estimation based on summary
#' statistics using an Approximate Bayesian Computation (ABC) algorithm. The
#' algorithm used here is the rejection sampling algorithm. This is a simplified
#' version of the [rejABC()] function that records only the index of the
#' accepted simulations.
#'
#' The rejection sampling algorithm generates random samples from the posterior
#' distributions of the parameters of interest. Note that to use this function,
#' the usual steps of ABC parameter estimation have to be performed. Briefly,
#' data should have been simulated based on random draws from the prior
#' distributions of the parameters of interest and a set of summary statistics
#' should have been calculated from that data. The same set of summary
#' statistics should have been calculated from the observed data to be used as
#' the `target` input in this function. Parameter values are accepted if the
#' Euclidean distance between the set of summary statistics computed from the
#' simulated data and the set of summary statistics computed from the observed
#' data is sufficiently small. The percentage of accepted simulations is
#' determined by `tol`.
#'
#' @param target a vector with the target summary statistics. These are usually
#'   the set of observed summary statistics.
#' @param params is a vector or matrix of simulated parameter values i.e.
#'   numbers from the simulations. Each row or vector entry should be a
#'   different simulation and each column of a matrix should be a different
#'   parameter.
#' @param sumstats is a vector or matrix of simulated summary statistics. Each
#'   row or vector entry should be a different simulation and each column of a
#'   matrix should be a different statistic.
#' @param tol is the tolerance rate, indicating the required proportion of
#'   points accepted nearest the target values.
#'
#' @return a list with two named entries
#'
#'   \item{index}{the index of the accepted simulations.}
#'
#'   \item{dst}{euclidean distances in the region of interest.}
#'
#' @examples
#' # load the matrix with parameter values
#' data(params)
#' # load the matrix with simulated parameter values
#' data(sumstats)
#'
#' # select a random simulation to act as target just to test the function
#' target <- sumstats[10 ,]
#'
#' # Parameter estimation using rejection sampling
#' index.rejABC(target = target, params = params, sumstats = sumstats[-10, ], tol = 0.01)
#'
#'
#' @export
index.rejABC <- function(target, params, sumstats, tol) {

  # get the number of summary statistics
  nss <- length(sumstats[1, ])

  # when dealing with a single parameter - in the form of a vector - convert that vector into a matrix with one column
  if(is.vector(params)) params <- matrix(params, ncol = 1)

  # checks that the function is used correctly
  if(length(target) != nss)
    stop("number of summary statistics in 'target' has to be the same as in 'sumstats")

  # stop if zero var in sumstat
  cond1 <- !any(as.logical(apply(sumstats, 2, function(x) length(unique(x))-1)))
  if(cond1) stop("zero variance in the summary statistics.")

  # scale everything
  # scale the matrix containing the summary statistics
  scaled.sumstat <- apply(X = sumstats, MARGIN = 2, FUN = function(col) normalise(x = col, y = col))
  # and the vector with the target
  scaled.target <- vapply(1:nss, FUN = function(i) normalise(target[i], sumstats[, i]), FUN.VALUE = numeric(1))

  # calculate euclidean distance
  dst <- apply(X = scaled.sumstat, MARGIN = 1, FUN = function(row) euclidean(a = row, b = scaled.target))

  # wt1 defines the region we're interested in
  # first define the threshold value
  abstol <- stats::quantile(x = dst[dst > 0], probs = tol)
  # create a vector of TRUE/FALSE indicating which values are below (or equal to) the threshold
  wt1 <- dst > 0 & dst <= abstol

  # create the output
  out <- list(index = which(wt1), dst = dst[wt1])
  # output the list containing the results of the function
  out
}


#' Parameter estimation with Approximate Bayesian Computation using local linear
#' regression
#'
#' This function performs multivariate parameter estimation based on summary
#' statistics using an Approximate Bayesian Computation (ABC) algorithm. The
#' algorithm used here is the local linear regression algorithm.
#'
#' Note that to use this function, the usual steps of ABC parameter estimation
#' have to be performed. Briefly, data should have been simulated based on
#' random draws from the prior distributions of the parameters of interest and a
#' set of summary statistics should have been calculated from that data. The
#' same set of summary statistics should have been calculated from the observed
#' data to be used as the target for parameter inference. A previous rejection
#' sampling step should also have been performed, where parameter values were
#' accepted if the Euclidean distance between the set of summary statistics
#' computed from the simulated data and the set of summary statistics computed
#' from the observed data was sufficiently small. Then, the output of the
#' rejection step is used as the input for this function and a local linear
#' regression method is used to correct for the imperfect match between the
#' summary statistics computed from the simulated data and the summary
#' statistics computed from the observed data.
#'
#' The parameter values accepted in the rejection step are weighted by a smooth
#' function (kernel) of the distance between the simulated and observed summary
#' statistics and corrected according to a linear transformation. This function
#' calls the function [stats::lm()] to accomplish this.
#'
#' @param rej is a list with the results of the rejection sampling algorithm.
#'   The output of the [rejABC()] function is the ideal input here.
#' @param tol is the tolerance rate, indicating the required proportion of
#'   points accepted nearest the target values. Note that the default value here
#'   is 1 because all points accepted in the rejection step should be used for
#'   the regression.
#' @param parameter is a parameter vector (long vector of numbers from the
#'   simulations). Each vector entry should correspond to a different
#'   simulation. This is the dependent variable for the regression.
#' @param simple logical, if TRUE a simplified output with only the essential
#'   information will be produced. If FALSE (default) the output will contain
#'   more information.
#'
#' @return a list with the results from the regression correction
#'
#'   \item{adjusted}{regression adjusted parameter values.}
#'
#'   \item{unadjusted}{parameter estimates obtained with the rejection
#'   sampling.}
#'
#'   \item{wt}{regression weights.}
#'
#'   \item{ss}{set of accepted summary statistics from the simulations.}
#'
#'   \item{predmean}{estimates of the posterior mean for each parameter.}
#'
#'   \item{fv}{fitted value from the regression.}
#'
#' @examples
#' # load the matrix with parameter values
#' data(params)
#' # load the matrix with simulated parameter values
#' data(sumstats)
#'
#' # select a random simulation to act as target just to test the function
#' target <- sumstats[10 ,]
#'
#' # parameter estimation using rejection sampling
#' rej <- rejABC(target = target, params = params, sumstats = sumstats[-10, ],
#' tol = 0.01, regression = TRUE)
#'
#' # parameter estimation using local linear regression
#' # note that you should select a parameter from the unadjusted matrix
#' regABC(rej = rej, parameter = rej$unadjusted[, 1])
#'
#' @export
regABC <- function(rej, parameter, tol = 1, simple = FALSE) {

  # get the (scaled) target from the rejection list
  target <- rej$s.target
  # now, obtain the accepted summary statistics from the rejection list
  sumstats <- rej$ss
  # get the accepted, from the rejection step, (scaled) summary statistics - from the rejection list
  scaled.sumstat <- rej$s.sumstat
  # obtain the vector containing the accepted euclidean distances from the rejection list
  dst <- rej$dst

  # first define the threshold value
  abstol <- stats::quantile(x = dst, probs = tol)
  # create a vector of TRUE/FALSE indicating which values are below (or equal to) the threshold
  wt1 <- dst <= abstol

  # calculate the weights for the linear regression
  regwt <- 1 - (dst[wt1]^2 / abstol^2)
  # use lm to fit a linear model and carry out the regression
  fit1 <- stats::lm(parameter[wt1]~scaled.sumstat[wt1, ], weights = regwt)

  # compute the estimate of the posterior mean
  predmean <- sum(as.numeric(fit1$coeff) * c(1, target), na.rm = TRUE)

  # if a simplified output is desired
  if(simple == TRUE) {

    # create a simplified output under the local linear regression method
    out <- list(adjusted = unname(fit1$residuals + predmean), wt = regwt, predmean = predmean)

  } else {

    # create the output under the local linear regression method
    out <- list(adjusted = unname(fit1$residuals + predmean), unadjusted = parameter[wt1], wt = regwt,
                ss = sumstats[wt1, ], predmean = predmean, fv = parameter[wt1] - fit1$residuals)
  }

  # output the list containing the results of the function
  out
}


#' Compute summary statistics from observed data
#'
#' Computes a defined set of summary statistics from observed data. Those
#' summary statistics can then be used as the target for parameter estimation or
#' model selection with Approximate Bayesian Computation.
#'
#' Summary statistics are computed for a given subset of the data. Ideally, this
#' subset is composed of randomly selected windows of the observed data. Those
#' random windows should be selected from multiple contigs, treating each contig
#' as a different locus.
#'
#' @param randomWindows a list with randomly selected loci of observed data.
#'   This list should contain five elements: `freqs`, `positions`, `rMajor`,
#'   `rMinor` and `coverage`. Each of those elements should contain one entry
#'   per locus with the appropriate information.
#' @param nPops is an integer indicating how many different populations are
#'   present in the dataset you are analysing.
#' @param stat.names optional character vector with the names of the summary
#'   statistics from the simulated data. If available, these names will be added
#'   to the summary statistics computed from the observed data.
#'
#' @return a vector of observed summary statistics. These summary statistics are
#'   computed from blocks of observed data present in the `randomWindows` input
#'   argument. If the `stat.names` input argument is available, the summary
#'   statistics will be named.
#'
#' @keywords internal
#'
#' @export
statsContig <- function(randomWindows, nPops, stat.names = NA) {

  # get the frequencies from the input
  Pop_Pi <- randomWindows[["freqs"]]
  # and the number of reads
  reads <- randomWindows[["rMinor"]]
  # and the coverage
  coverage <- randomWindows[["coverage"]]

  # The list entries are on the nSites x nPops format. We need to transpose the entries
  # this changes the matrices - now each row is a population and each column is a different site
  Pop_Pi <- lapply(Pop_Pi, function(locus) t(locus))
  # transpose also the number of reads
  reads <- lapply(reads, function(locus) t(locus))
  # and the depth of coverage
  coverage <- lapply(coverage, function(locus) t(locus))

  # check if the data is on the correct format
  if(any(sapply(Pop_Pi, nrow) != nPops) | any(sapply(coverage, nrow) != nPops))
    stop(paste("Number of rows is not identical to the number of populations. Please check"))

  # get the number of loci
  nLoci <- length(Pop_Pi)

  # compute the fraction of sites showing a fixed difference between the populations
  Sf <- lapply(1:nLoci, function(locus)
    fixed(minor = reads[[locus]], total = coverage[[locus]], nPops))
  # combine the previous list into a single matrix - where each column is a different comparison
  Sf <- do.call(rbind, Sf)
  # get the mean, across all loci, for the fraction of sites showing a fixed difference
  Sf <- colMeans(Sf, na.rm = TRUE)

  # calculate the fraction of sites showing an exclusive polymorphism to a given population
  Sx <- lapply(1:nLoci, function(locus)
    exclusive(minor = reads[[locus]], total = coverage[[locus]], nPops))
  # combine the previous list into a single matrix - where each column is a different comparison
  Sx <- do.call(rbind, Sx)
  # compute the mean (across loci) for each population
  Sx <- colMeans(Sx, na.rm = TRUE)

  # compute the fraction of sites with a polymorphism shared between the populations
  SS <- lapply(1:nLoci, function(locus)
    shared(minor = reads[[locus]], total = coverage[[locus]], nPops))
  # combine the previous list into a single matrix - where each column is a different comparison
  SS <- do.call(rbind, SS)
  # get the mean, across all loci, for the fraction of sites shared polymorphism between the populations
  SS <- colMeans(SS, na.rm = TRUE)

  # Compute the mean expected heterozygosity for each population and locus
  ExpHet <- meanExpected_Het(Pop_Pi)
  # Compute the mean (across loci) for each population
  PopHet <- do.call(rbind,ExpHet)
  # Compute mean expected heterozygosity for each population across all loci
  HetMean <- colMeans(PopHet, na.rm = TRUE)
  # And the standard deviation
  HetSD <- apply(PopHet, 2, stats::sd, na.rm = TRUE)

  # Compute the heterozygosity between populations
  HetBetween <- Het_Between(Pop_Pi)
  # Compute the mean (across loci) for each pairwise comparison
  PopBetHet <- do.call(rbind,HetBetween)
  MeanHetBet <- colMeans(PopBetHet, na.rm = TRUE)
  # And the standard deviation
  SDHetBet <- apply(PopBetHet, 2, stats::sd, na.rm = TRUE)

  # Calculate the FST value between populations
  FST <- popsFST(nPops = nPops, Pop_Pi, coverage)
  # The previous returns a matrix where each pairwise comparison has a FST value and the rest is NAs
  # This matrix can be reduced to a vector
  FST <- lapply(FST, FUN = function(x) {
    x[!is.na(x)]})
  # Then, compute the mean FST value between pops
  FST <- do.call(rbind,FST)
  MeanFST <- colMeans(FST)
  # And the standard deviation
  SDFST <- apply(FST, 2, stats::sd)
  # calculate the 5% and the 95% quantiles for the FST distribution
  FSTQ1 <- apply(FST, MARGIN = 2, function(col) unname(stats::quantile(col, probs = 0.05)))
  FSTQ2 <- apply(FST, MARGIN = 2, function(col) unname(stats::quantile(col, probs = 0.95)))

  # check if the 5% FST quantile is below 0 - and if it is, replace the value by a zero
  FSTQ1[FSTQ1 < 0] <- 0

  # if 4 populations were used
  if(nPops == 4) {

    # calculate DSTAT values over a list - each entry is a different locus
    dstat <- lapply(Pop_Pi, function(pi) D.statPool(pi))
    # combine all the values into a single matrix
    tempdstat <- do.call(rbind,dstat)
    # compute the mean value for a single simulation
    dstat <- colMeans(tempdstat, na.rm = TRUE)
    # And the standard deviation across locus
    SD_dstat <- apply(tempdstat, MARGIN = 2 , stats::sd, na.rm = TRUE)

    # create the output
    output <- unname(c(Sf, Sx, SS, HetMean, HetSD, MeanHetBet, SDHetBet, MeanFST, SDFST, FSTQ1, FSTQ2, dstat, SD_dstat))

  } else {

    # create the output - if less than 4 pops and thus no DSTAT
    output <- unname(c(Sf, Sx, SS, HetMean, HetSD, MeanHetBet, SDHetBet, MeanFST, SDFST, FSTQ1, FSTQ2))

  }

  # add names to the summary statistics if they are available
  if(all(!is.na(stat.names)))
    names(output) <- stat.names

  # output the summary statistics calculated from blocks across several contigs
  output
}


#' Parameter estimation with Approximate Bayesian Computation for a single
#' target
#'
#' Perform multivariate parameter estimation based on summary statistics using
#' an Approximate Bayesian Computation (ABC) algorithm. This function always
#' uses a rejection sampling algorithm while a local linear regression algorithm
#' might or might not be used.
#'
#' To use this function, the usual steps of ABC parameter estimation have to be
#' performed. Briefly, data should have been simulated based on random draws
#' from the prior distributions of the parameters of interest and a set of
#' summary statistics should have been calculated from that data. The same set
#' of summary statistics should have been calculated from the observed data to
#' be used as the `target` input in this function. Parameter values are accepted
#' if the Euclidean distance between the set of summary statistics computed from
#' the simulated data and the set of summary statistics computed from the
#' observed data is sufficiently small. The percentage of accepted simulations
#' is determined by `tol`. This function performs a simple rejection by calling
#' the [rejABC()] function.
#'
#' When `method` is "regression", a local linear regression method is used to
#' correct for the imperfect match between the summary statistics computed from
#' the simulated data and the summary statistics computed from the observed
#' data. The output of the [rejABC()] function is used as the input of the
#' [regABC()] function to apply this correction. The parameter values accepted
#' in the rejection step are weighted by a smooth function (kernel) of the
#' distance between the simulated and observed summary statistics and corrected
#' according to a linear transformation.
#'
#' @param target a vector with the target summary statistics. These are usually
#'   computed from observed data or selected from a random simulation when
#'   performing cross-validation.
#' @param params is a vector or matrix of simulated parameter values i.e.
#'   numbers from the simulations. Each row or vector entry should be a
#'   different simulation and each column of a matrix should be a different
#'   parameter. This is the dependent variable for the regression, if a
#'   regression step is performed.
#' @param sumstats is a vector or matrix of simulated summary statistics. Each
#'   row or vector entry should be a different simulation and each column of a
#'   matrix should be a different statistic. These act as the independent
#'   variables if a regression step is performed.
#' @param limits is a matrix with two columns and as many rows as there are
#'   parameters. Each row should contain the minimum value of the prior for a
#'   given parameter in the first column and the maximum value in the second
#'   column.
#' @param tol is the tolerance rate, indicating the required proportion of
#'   points accepted nearest the target values.
#' @param method either "rejection" or "regression" indicating whether a
#'   regression step should be performed during ABC parameter estimation.
#'
#' @return the returned object is a list containing the following components:
#'
#'   \item{unadjusted}{parameter estimates obtained with the rejection
#'   sampling.}
#'
#'   \item{rej.prediction}{point estimates of the posterior obtained with the
#'   rejection sampling.}
#'
#'   \item{adjusted}{regression adjusted parameter values.}
#'
#'   \item{loc.prediction}{point estimates of the regression adjusted
#'   posterior.}
#'
#'   \item{ss}{set of accepted summary statistics from the simulations.}
#'
#'   \item{wt}{regression weights.}
#'
#' @examples
#' # load the matrix with parameter values
#' data(params)
#' # load the matrix with simulated parameter values
#' data(sumstats)
#' # load the matrix with the prior limits
#' data(limits)
#'
#' # select a random simulation to act as target just to test the function
#' target <- sumstats[10 ,]
#' # we should remove the random simulation from the sumstats and params matrices
#' sumstats <- sumstats[-10, ]; params <- params[-10, ]
#'
#' # parameter estimation for a single target
#' singleABC(target = target, params = params, sumstats = sumstats, limits = limits,
#' tol = 0.01, method = "regression")
#'
#' @export
singleABC <- function(target, params, sumstats, limits, tol, method) {

  # check if the method is defined correctly
  if(method != "rejection" & method != "regression")
    stop("method must be either rejection or regression")

  # if method is set as regression then create a variable reg and set it as TRUE
  if(method == "regression")
    reg <- TRUE
  else # or else set that variable to FALSE
    reg <- FALSE

  # when dealing with a single summary statistic - in vector format - convert that vector into a matrix with one column
  if(is.vector(sumstats)) sumstats <- matrix(sumstats, ncol = 1)

  # get the number of parameters
  nparams <- ncol(params)
  # get the names of the parameters
  param.names <- colnames(params)

  # apply the Tmatrix function to the matrix containing the parameter values -
  # taking into account the min and max of the prior distributions
  temp.params <- Tmatrix(original = params, limits = limits)
  # set the column names of this transformed matrix of parameter values
  colnames(temp.params) <- param.names

  # perform ABC using the user defined target - applying the rejection method
  rej.infer <- rejABC(target = target, params = temp.params, sumstats = sumstats, tol = tol, regression = reg)

  # set the default output of the function - if no regression step is performed
  adjusted <- NA; loc.stats <- NA; wt <- NA

  # if reg is set to true - apply a local linear regression method correction
  if(reg == TRUE) {

    # define the number of points accepted by the local linear regression
    nPoints <- nrow(params)*tol

    # perform local linear regression for each column of the unadjusted matrix - each column is a different parameter
    reg.infer <- apply(rej.infer$unadjusted, MARGIN = 2, FUN = function(col)
      # tolerance is set at 1 - this means that we accept all points that were accepted during the rejection method
      tmp <- regABC(rej = rej.infer, parameter = col, tol = 1))

    # only the first nPoints rows are transformed - they contain estimates while the remaining rows contain the weights
    adjusted <- sapply(reg.infer, function(param) param[["adjusted"]])
    # back-transform the adjusted parameter values - get back to the original scale of the parameters
    adjusted <- BTmatrix(transformed = adjusted, limits = limits)

    # get the weights from the linear regression -
    # keep only the first column because, for each trial, the weights are the same for all parameters
    wt <- sapply(reg.infer, function(param) param[["wt"]])[, 1]

    # compute summary statistics of the posterior distribution for each parameter
    # i.e. compute the mode, median and mean of the posterior distributions
    loc.stats <- poststat(posterior = adjusted, limits = limits, method = "regression", wtreg = wt)
  }

  # get the estimates obtained with the rejection method
  unadjusted <- rej.infer[["unadjusted"]]

  # back-transform the estimates obtained with the rejection method - get back to the original scale of the parameters
  unadjusted <- BTmatrix(transformed = unadjusted, limits = limits)

  # compute summary statistics of the posterior distribution for each parameter
  # i.e. compute the mode, median and mean of the posterior distributions
  rej.stats <- poststat(posterior = unadjusted, limits = limits, method = "rejection")

  # get the accepted summary statistics
  ss <- rej.infer[["ss"]]

  # create the final output of this function
  out <- list(unadjusted = unadjusted, rej.prediction = rej.stats, adjusted = adjusted,
              loc.prediction = loc.stats, ss = ss, wt = wt)

  # output the result of this function
  out
}


#' Parameter estimation with Approximate Bayesian Computation for multiple
#' targets
#'
#' Perform multivariate parameter estimation based on summary statistics using
#' an Approximate Bayesian Computation (ABC) algorithm. This function always
#' uses a rejection sampling algorithm while a local linear regression algorithm
#' might or might not be used.
#'
#' To use this function, the usual steps of ABC parameter estimation have to be
#' performed. Briefly, data should have been simulated based on random draws
#' from the prior distributions of the parameters of interest and a set of
#' summary statistics should have been calculated from that data. The same set
#' of summary statistics should have been calculated from the observed data to
#' be used as the `targets` in this function. Parameter values are accepted if
#' the Euclidean distance between the set of summary statistics computed from
#' the simulated data and the set of summary statistics computed from the
#' observed data is sufficiently small. The percentage of accepted simulations
#' is determined by `tol`. This function performs a simple rejection by calling
#' the [rejABC()] function.
#'
#' When `method` is "regression", a local linear regression method is used to
#' correct for the imperfect match between the summary statistics computed from
#' the simulated data and the summary statistics computed from the observed
#' data. The output of the [rejABC()] function is used as the input of the
#' [regABC()] function to apply this correction. The parameter values accepted
#' in the rejection step are weighted by a smooth function (kernel) of the
#' distance between the simulated and observed summary statistics and corrected
#' according to a linear transformation.
#'
#' Please note that this functions performs parameter estimation for multiple
#' `targets`. The `targets` should contain multiple rows and each row will be
#' treated as an independent target for parameter estimation.
#'
#' @param targets a matrix of observed summary statistics. Each row will be
#'   considered a different target for parameter estimation. Each column should
#'   be a different summary statistics and these statistics should correspond to
#'   the statistics in the `sumstats` input.
#' @param params is a vector or matrix of simulated parameter values i.e.
#'   numbers from the simulations. Each row or vector entry should be a
#'   different simulation and each column of a matrix should be a different
#'   parameter. This is the dependent variable for the regression, if a
#'   regression step is performed.
#' @param sumstats is a vector or matrix of simulated summary statistics. Each
#'   row or vector entry should be a different simulation and each column of a
#'   matrix should be a different statistic. These act as the independent
#'   variables if a regression step is performed.
#' @param limits is a matrix with two columns and as many rows as there are
#'   parameters. Each row should contain the minimum value of the prior for a
#'   given parameter in the first column and the maximum value in the second
#'   column.
#' @param tol is the tolerance rate, indicating the required proportion of
#'   points accepted nearest the target values.
#' @param method either "rejection" or "regression" indicating whether a
#'   regression step should be performed during ABC parameter estimation.
#' @param parallel logical, indicating whether this function should be run using
#'   parallel execution. The default setting is FALSE, meaning that this
#'   function will utilize a single core.
#' @param ncores a non-negative integer that is required when `parallel` is
#'   TRUE. It specifies the number of cores to use for parallel execution.
#'
#' @return the returned object is a list containing the following components:
#'
#'   \item{target}{parameter estimates obtained with the rejection sampling.}
#'
#'   \item{ss}{set of accepted summary statistics from the simulations.}
#'
#'   \item{unadjusted}{parameter estimates obtained with the rejection
#'   sampling.}
#'
#'   \item{adjusted}{regression adjusted parameter values.}
#'
#'   \item{predmean}{estimates of the posterior mean for each parameter.}
#'
#'   \item{weights}{regression weights.}
#'
#' @examples
#' # load the matrix with parameter values
#' data(params)
#' # load the matrix with simulated parameter values
#' data(sumstats)
#' # load the matrix with the prior limits
#' data(limits)
#'
#' # select some random simulations to act as target just to test the function
#' targets <- sumstats[c(11:20) ,]
#' # we should remove those random simulation from the sumstats and params matrices
#' sumstats <- sumstats[-c(11:20), ]; params <- params[-c(11:20), ]
#'
#' # parameter estimation for multiple targets
#' multipleABC(targets = targets, params = params, sumstats = sumstats, limits = limits,
#' tol = 0.01, method = "regression")
#'
#' @export
multipleABC <- function(targets, params, sumstats, limits, tol, method, parallel = FALSE, ncores = NA) {

  # check if the method is defined correctly
  if(method != "rejection" & method != "regression")
    stop("method must be either rejection or regression")

  # if method is set as regression then create a variable reg and set it as TRUE
  if(method == "regression")
    reg <- TRUE
  else # or else set that variable to FALSE
    reg <- FALSE

  # when dealing with a single summary statistic - in vector format:
  # convert that vector into a matrix with one column
  if(is.vector(sumstats)) sumstats <- matrix(sumstats, ncol = 1)

  # get the number of parameters
  nparams <- ncol(params)
  # get the names of the parameters
  param.names <- colnames(params)

  # apply the Tmatrix function to the matrix containing the parameter values -
  # taking into account the min and max of the prior distributions
  temp.params <- Tmatrix(original = params, limits = limits)
  # set the names of the columns of this matrix as the parameter names
  colnames(temp.params) <- param.names

  # get the number of evaluations to be performed - how many different targets exist?
  nvals <- nrow(targets)

  # set the default output of the function - if no regression step is performed
  predmean <- NA; adjusted <- NA; wts <- NA

  # if parallel is TRUE, this functions performs parallel execution of the rejection sampling
  if(parallel == TRUE) {

    # so %dopar% doesn't need to be attached
    `%dopar%` <- foreach::`%dopar%`

    # check if the number of cores are specified
    if(is.na(ncores))
      stop("Please specify the number of cores to use for parallel execution")

    # set the number of cores to utilize
    doParallel::registerDoParallel(cores = ncores)

    # perform parameter estimation with Approximate Bayesian Computation - using the rejection method in parallel
    rej.infer <- foreach::foreach(i = 1:nvals) %dopar% {
      rejABC(target = targets[i, ], params = temp.params, sumstats = sumstats, tol = tol, regression = reg) }

  } else {

    # perform parameter estimation with Approximate Bayesian Computation - using the rejection method
    rej.infer <- apply(X = targets, MARGIN = 1, FUN = function(row)
      rejABC(target = row, params = temp.params, sumstats = sumstats, tol = tol, regression = reg))
  }

  # if reg is set to true - apply a local linear regression method correction
  if(reg == TRUE) {

    # if parallel is TRUE, this functions performs parallel execution of the regression algorithm
    if(parallel == TRUE) {

      # set binding for global variable
      i <- NULL

      # perform parameter estimation with Approximate Bayesian Computation:
      # using the regression algorithm in parallel
      reg.infer <- foreach::foreach(i = 1:nvals) %dopar% {
        # perform local linear regression for each column of the unadjusted matrix:
        # each column is a different parameter
        apply(rej.infer[[i]]$unadjusted, MARGIN = 2, FUN = function(col) {
          # tolerance is set at 1:
          # this means that we accept all points that were accepted during the rejection method
          regABC(rej = rej.infer[[i]], parameter = col, tol = 1, simple = TRUE) })
      }

    } else {

      # perform parameter estimation with Approximate Bayesian Computation - using the regression algorithm
      reg.infer <- lapply(1:nvals, FUN = function(i) {
        # perform local linear regression for each column of the unadjusted matrix:
        # each column is a different parameter
        apply(rej.infer[[i]]$unadjusted, MARGIN = 2, FUN = function(col) {
          # tolerance is set at 1:
          # this means that we accept all points that were accepted during the rejection method
          regABC(rej = rej.infer[[i]], parameter = col, tol = 1, simple = TRUE) })
        })
    }

    # get the adjusted parameter inferences - these are the inferences after the regression correction
    adjusted <- lapply(reg.infer, function(t) sapply(t, function(p) cbind(p$adjusted)))
    # back-transform the adjusted values - to get back to the original scale of the parameters
    adjusted <- lapply(adjusted, function(trial) BTmatrix(transformed = trial, limits = limits))

    # get the estimate of the posterior mean for each parameter:
    # combine the estimates for each target into a single matrix
    # each row corresponds to a different target
    predmean <- t(sapply(reg.infer, function(t) sapply(t, function(p) cbind(p$predmean))))
    # back-transform the estimates - to get back to the original scale of the parameters
    predmean <- BTmatrix(transformed = predmean, limits = limits)

    # get the weights of the linear regression
    wts <- lapply(X = lapply(reg.infer, function(trial) lapply(trial, function(param) param$wt)), `[[`, 1)
  }

  # get the unadjusted parameter inferences - these are the inferences made using the rejection method
  unadjusted <- lapply(rej.infer , function(i) i[["unadjusted"]])
  # we need to back transform the unadjusted values - to get back to the original scale of the parameters
  unadjusted <- lapply(unadjusted, function(trial) BTmatrix(transformed = trial, limits = limits))

  # get the accepted summary statistics
  ss <- lapply(rej.infer, function(trial) trial[["ss"]])

  # create the final output of this function
  out <- list(target = targets, ss = ss, unadjusted = unadjusted, adjusted = adjusted,
              predmean = predmean, weights = wts)

  # output the result of this function
  out
}


#' Parameter estimation with Approximate Bayesian Computation with several
#' targets
#'
#' Perform multivariate parameter estimation based on summary statistics using
#' an Approximate Bayesian Computation (ABC) algorithm. This function always
#' uses a rejection sampling algorithm while a local linear regression algorithm
#' might or might not be used.
#'
#' To use this function, the usual steps of ABC parameter estimation have to be
#' performed. Briefly, data should have been simulated based on random draws
#' from the prior distributions of the parameters of interest and a set of
#' summary statistics should have been calculated from that data. This function
#' requires as input the observed data and computes the same set of summary
#' statistics from that observed data. Multiple sets of observed summary
#' statistics are computed from `ntrials` sets of `nLoci` blocks of size
#' `window`. Parameter estimation is performed for each one of those sets of
#' observed summary statistics i.e. each set corresponds to a different target.
#'
#' After computing this set of observed summary statistics, a simple rejection
#' is performed by calling the [rejABC()] function. In this step, parameter
#' values are accepted if the Euclidean distance between the set of summary
#' statistics computed from the simulated data and the set of summary statistics
#' computed from the observed data is sufficiently small. The percentage of
#' accepted simulations is determined by `tol`.
#'
#' When `method` is "regression", a local linear regression method is used to
#' correct for the imperfect match between the summary statistics computed from
#' the simulated data and the summary statistics computed from the observed
#' data. The output of the [rejABC()] function is used as the input of the
#' [regABC()] function to apply this correction. The parameter values accepted
#' in the rejection step are weighted by a smooth function (kernel) of the
#' distance between the simulated and observed summary statistics and corrected
#' according to a linear transformation.
#'
#' @param nPops is an integer indicating how many different populations are
#'   present in the dataset you are analysing.
#' @param freqs is a list containing the allelic frequencies. Each entry of that
#'   list should represent a different contig and be a matrix where each row
#'   corresponds to a different site and each column to a different population.
#' @param positions is a list containing the position of the SNPs. Each entry
#'   should represent a different contig and be a vector containing the position
#'   of each SNP present in the contig.
#' @param range is a list containing the range of the contig. Each entry should
#'   represent a different contig and be a vector with two entries: the first
#'   detailing the minimum position of the contig and the second the maximum
#'   position of the contig.
#' @param rMajor a list containing the number of major allele reads. Each entry
#'   should represent a different contig. For each contig (matrix), each row
#'   should be a different site and each column a different population.
#' @param rMinor a list containing the number of minor allele reads. Each entry
#'   should represent a different contig. For each contig (matrix), each row
#'   should be a different site and each column a different population.
#' @param coverage is a list containing the depth of coverage. Each entry should
#'   represent a different contig and be a matrix with the sites as rows and the
#'   different populations as columns.
#' @param window is a non-negative integer indicating the size, in base pairs,
#'   of the block of the contig to keep.
#' @param nLoci is a non-negative integer indicating how many different contigs
#'   should be kept in the output. If each randomly selected `window` is a
#'   different loci, then how many different `window` should be selected?
#' @param limits is a matrix with two columns and as many rows as there are
#'   parameters. Each row should contain the minimum value of the prior for a
#'   given parameter in the first column and the maximum value in the second
#'   column.
#' @param params is a vector or matrix of simulated parameter values i.e.
#'   numbers from the simulations. Each row or vector entry should be a
#'   different simulation and each column of a matrix should be a different
#'   parameter. This is the dependent variable for the regression, if a
#'   regression step is performed.
#' @param sumstats is a vector or matrix of simulated summary statistics. Each
#'   row or vector entry should be a different simulation and each column of a
#'   matrix should be a different statistic. These act as the independent
#'   variables if a regression step is performed.
#' @param ntrials indicates how many different trials should be performed. Each
#'   trial corresponds to a different target for the parameter estimation.
#' @param tol is the tolerance rate, indicating the required proportion of
#'   points accepted nearest the target values.
#' @param method either "rejection" or "regression" indicating whether a
#'   regression step should be performed during ABC parameter estimation.
#' @param parallel logical, indicating whether this function should be run using
#'   parallel execution. The default setting is FALSE, meaning that this
#'   function will utilize a single core.
#' @param ncores a non-negative integer that is required when `parallel` is
#'   TRUE. It specifies the number of cores to use for parallel execution.
#'
#' @return a list with seven different entries.
#'
#'   \item{target}{observed summary statistics.}
#'
#'   \item{ss}{set of accepted summary statistics from the simulations.}
#'
#'   \item{unadjusted}{parameter estimates obtained with the rejection
#'   sampling.}
#'
#'   \item{adjusted}{regression adjusted parameter values.}
#'
#'   \item{predmean}{estimates of the posterior mean for each parameter.}
#'
#'   \item{weights}{regression weights.}
#'
#'   \item{position}{position of each SNP used for calculating the observed
#'   summary statistics.}
#'
#' @examples
#' # Note that this example is limited to a few of the options available
#' # you should check the poolABC vignette for more details
#'
#' # this creates a variable with the path for the toy example data
#' mypath <- system.file('extdata', package = 'poolABC')
#'
#' # import data for two populations from all files
#' mydata <- importContigs(path = mypath, pops = c(8, 10))
#'
#' # to perform parameter inference for two populations using the rejection method
#' # and with a tolerance of 0.01
#' myabc <- ABC(nPops = 2, ntrials = 10, freqs = mydata$freqs, positions = mydata$positions,
#' range = mydata$range, rMajor = mydata$rMajor, rMinor = mydata$rMinor, coverage = mydata$coverage,
#' window = 1000, nLoci = 4, limits, params, sumstats, tol = 0.01, method = "rejection")
#'
#' # the previous will perform parameter inference for 10 different targets (ntrials = 100)
#' # each of those trials will be comprised of 4 loci, each with 1000 base pairs
#' 
#' # to perform parameter inference for two populations using the regression method
#' # and with a tolerance of 0.01
#' myabc <- ABC(nPops = 2, ntrials = 10, freqs = mydata$freqs, positions = mydata$positions,
#' range = mydata$range, rMajor = mydata$rMajor, rMinor = mydata$rMinor, coverage = mydata$coverage,
#' window = 1000, nLoci = 4, limits, params, sumstats, tol = 0.01, method = "regression")
#'
#' @seealso
#' For more details see the poolABC vignette:
#' \code{vignette("poolABC", package = "poolABC")}
#'
#' @export
ABC <- function(nPops, ntrials, freqs, positions, range, rMajor, rMinor, coverage, window, nLoci, limits,
                params, sumstats, tol, method, parallel = FALSE, ncores = NA) {

  # check if the method is correctly defined
  if(method != "rejection" & method != "regression")
    stop("method must be either rejection or regression")

  # get the names of the sumstats used
  stat.names <- colnames(sumstats)
  # get the names of the parameters
  param.names <- colnames(params)
  # get the number of parameters
  nparams <- ncol(params)

  # use the pickWindows function to select random bp windows
  # combine this with the replicate function to select multiple sets of random loci
  randomWindows <- replicate(n = ntrials, expr =
                               pickWindows(freqs, positions, range, rMajor, rMinor, coverage, window = window, nLoci = nLoci),
                             simplify = FALSE)

  # get the position of the SNPs from the previous list
  pos <- lapply(randomWindows, function(trial) unname(trial[["positions"]]))

  # calculate summary statistics from real data
  windowStats <- lapply(randomWindows, function(trial) statsContig(trial, nPops = nPops))
  # transform the targets (i.e. windowStats) into a matrix, where each column is a sumstat
  # and each row a different target subset
  windowStats <- do.call(rbind, windowStats)

  # remove summary statistics with no variance
  temp <- removeVar(observed = windowStats, sumstats = sumstats)
  # recover the two objects - observed and simulated summary statistics
  windowStats <- temp$observed; sumstats <- temp$sumstats
  # update the name of the simulated summary statistics
  stat.names <- colnames(sumstats)

  # perform parameter estimation with Approximate Bayesian Computation
  out <- multipleABC(targets = windowStats, params = params, sumstats = sumstats, limits = limits, tol = tol,
                     method = method, parallel = parallel, ncores = ncores)

  # add the position of the SNPs to the list
  out$positions <- pos

  # output the result of the function
  out
}


#' Plot multiple posterior distributions
#'
#' Plots, in the same plot, the density of multiple posterior distributions of a
#' given parameter.
#'
#' After using the [multipleABC()] or [ABC()] functions to perform parameter
#' estimation with Approximate Bayesian Computation with several targets, this
#' function can be used for a quick visualization of the quality of an ABC
#' analysis. Multiple posterior distributions, each obtained for a different
#' target, are plotted in the same plot, allowing for a visualization of the
#' shape of the posteriors and a quick inspection of whether all the posteriors
#' converge to the same estimate.
#'
#' @param posteriors is a list with samples from the posterior distributions
#'   obtained for each target. Each entry of the list is a matrix where each row
#'   corresponds to a different accepted simulations and each column corresponds
#'   to a different parameter.
#' @param index an non-negative integer indicating which parameter to plot. It
#'   corresponds to the desired column of a matrix in the `posteriors` input.
#'   So, to plot the first parameter, corresponding to the first column in the
#'   `posteriors` input select 1. To plot the second parameter, select 2 and so
#'   on.
#' @param limits is a matrix with two columns and as many rows as there are
#'   parameters. Each row should contain the minimum value of the prior for a
#'   given parameter in the first column and the maximum value in the second
#'   column.
#' @param weights is an optional list input containing the weights from the
#'   local linear regression method. Each entry of the list should be a numeric
#'   vector with the weights for each accepted simulation.
#'
#' @return a plot with multiple posterior distributions, each obtained for a
#'   different target, for the selected parameter.
#'
#' @examples
#' # load the matrix with parameter values
#' data(params)
#' # load the matrix with simulated parameter values
#' data(sumstats)
#' # load the matrix with the prior limits
#' data(limits)
#'
#' # select some random simulations to act as target just to test the function
#' targets <- sumstats[c(11:20) ,]
#' # we should remove those random simulation from the sumstats and params matrices
#' sumstats <- sumstats[-c(11:20), ]; params <- params[-c(11:20), ]
#'
#' # parameter estimation for a single target
#' myabc <- multipleABC(targets = targets, params = params, sumstats = sumstats, limits = limits,
#' tol = 0.01, method = "regression")
#'
#' # plot multiple posteriors
#' plot_Posteriors(posteriors = myabc$adjusted, index = 1, limits = limits, weights = myabc$weights)
#'
#' # note that this is just an example!
#' # we don't have enough simulations to obtain credible results
#'
#' @export
plot_Posteriors <- function(posteriors, index, limits, weights = NULL) {

  # check if the limits input is in the correct format
  if(length(dim(limits)) < 2)
    stop("the limits input should be a matrix with each parameter in a different row!")

  # check if the names of the parameters in the posteriors match the names in the limits
  if(any(colnames(posteriors[[1]]) != rownames(limits))) {
    stop("parameter names are not the same for the posteriors and the limits!")
  }

  # get the name of the parameter of interest
  param.name <- colnames(posteriors[[1]])[index]

  # get the number of evaluations performed - i.e. the total number of different posterior distributions
  nvals <- length(posteriors)

  # get the minimum and the maximum value of the prior for that particular parameter
  minPrior <- limits[index, 1]
  maxPrior <- limits[index, 2]

  # get the posterior distribution only for the parameter of interest
  posteriors <- lapply(posteriors, function(entry) entry[, index])

  # when regression weights are supplied as input
  if(!is.null(weights)) {

    # use those weights as input in the locfit function
    fitPosterior <- lapply(1:nvals, function(i)
      locfit::locfit(~posteriors[[i]], xlim = c(minPrior, maxPrior), alpha = 0.5, weights = weights[[i]]))

  } else { # when no regression weights are supplied as input

    # run the locfit function without prior weights
    fitPosterior <- lapply(1:nvals, function(i)
      locfit::locfit(~posteriors[[i]], xlim = c(minPrior, maxPrior), alpha = 0.5))
  }

  # to obtain the maximum value on the y axis for the posterior distribution
  # we first need to generate a sequence of numbers on the x axis scale
  tmp <- seq(minPrior, maxPrior, length.out = 1000)
  # and then use the predict function to calculate the values on the y axis that correspond to that sequence
  temp <- lapply(fitPosterior, function(fit) stats::predict(fit, tmp))
  # then, we can get the maximum from this prediction
  ymax <- sapply(temp, function(x) x[which(x == max(x))])
  # to get the maximum value for the plot, we just need to use the highest value
  ymax <- max(ymax)

  # initialize a blank plot - with correct labels and axis
  graphics::plot(1, type = "n", xlab = "Values of the parameter", ylab = "Density",
                 xlim = c(minPrior, maxPrior), ylim = c(0, ymax))

  # plot the density estimation of the posterior distribution on the blank plot created
  # if plotting more than 12 lines, then we cant use different colours
  if (nvals > 12) {

    # plot each of the posteriors distributions using a grey colour
    for (i in 1:length(fitPosterior)) {
      graphics::lines(fitPosterior[[i]], lty = 2, col = "grey40")
    }

  } else { # when plotting less than 12 distributions, we can use a different colour per line

    # create a vector with the colours to use
    mycols <- RColorBrewer::brewer.pal(n = 12, name = "Set3")

    # plot each of the posteriors distributions using a different colour
    for (i in 1:length(fitPosterior)) {
      graphics::lines(fitPosterior[[i]], lty = 2, lwd = 2, col = mycols[i])
    }
  }

  # add a title to the plot
  graphics::title(main = paste("Comparison of posterior distributions for", paste(param.name)))
}


#' Plot the density estimation of a given parameter
#'
#' Plots the density estimation of a single parameter for quick visualization of
#' the quality of an ABC analysis.
#'
#' This function can be used for a quick visualization of the posterior
#' distribution obtained for a single target with the [singleABC()] function.
#' Alternatively, if parameter estimation was performed with the [multipleABC()]
#' function, the multiple posterior distributions, each obtained for a different
#' target, will be combined into a single matrix and all values will be
#' considered samples from the same posterior distribution.
#'
#' @param prior is a vector or matrix of simulated parameter values i.e. numbers
#'   from the simulations. Each row or vector entry should be a different
#'   simulation and each column of a matrix should be a different parameter.
#'   This corresponds to the prior distribution and it should contain all the
#'   simulated parameter values.
#' @param posterior is either a list or a matrix with samples from the posterior
#'   distributions obtained for each target. If in list format, each entry
#'   should be a matrix where each row corresponds to a different accepted
#'   simulations and each column corresponds to a different parameter.
#' @param limits is a matrix with two columns and as many rows as there are
#'   parameters. Each row should contain the minimum value of the prior for a
#'   given parameter in the first column and the maximum value in the second
#'   column.
#' @param index is an non-negative integer indicating which parameter to plot.
#'   It corresponds to the desired column of a matrix in the `posteriors` input.
#'   So, to plot the first parameter, corresponding to the first column in the
#'   `posteriors` input select 1. To plot the second parameter, select 2 and so
#'   on.
#' @param weights is an optional list input containing the weights from the
#'   local linear regression method. Each entry of the list should be a numeric
#'   vector with the weights for each accepted simulation.
#'
#' @return a plot of the density estimation of a given parameter. This plot will
#'   include a title with the name of the parameter. It will also include the
#'   density of the prior distribution for that parameter.
#'
#' @examples
#' # load the matrix with parameter values
#' data(params)
#' # load the matrix with simulated parameter values
#' data(sumstats)
#' # load the matrix with the prior limits
#' data(limits)
#'
#' # select a random simulation to act as target just to test the function
#' target <- sumstats[15 ,]
#' # we should remove the random simulation from the sumstats and params matrices
#' sumstats <- sumstats[-15, ]; params <- params[-15, ]
#'
#' # parameter estimation for a single target
#' myabc <- singleABC(target = target, params = params, sumstats = sumstats, limits = limits,
#' tol = 0.01, method = "regression")
#'
#' # plot the density estimation of a given parameter
#' plot_param(prior = params, posterior = myabc$adjusted, limits = limits,
#' index = 6, weights = myabc$weights)
#'
#' # note that this is just an example!
#' # we don't have enough simulations to obtain credible results
#'
#' @export
plot_param <- function(prior, posterior, limits, index, weights = NULL) {

  # if needed, transform the posterior input into a matrix
  # where each column is a parameter and each row a different accepted simulation
  if(is.list(posterior))
    posterior <- do.call(rbind, posterior)

  # if needed, transform the weights input into a vector
  if(is.list(weights))
    weights <- unlist(weights)

  # get the name of the parameter of interest
  param.name <- colnames(posterior)[index]

  # check if the names of the parameters in the posteriors match the names in the limits
  if(any(colnames(posterior) != colnames(prior))) {
    stop("parameter names are not the same for the posterior and the prior!")
  }

  # check if the limits input is in the correct format
  if(length(dim(limits)) < 2)
    stop("the 'limits' input should be a matrix with each parameter in a different row!")

  # check if the parameter of interest matches the correct row of the limits input
  if(param.name != rownames(limits)[index])
    stop("the name of the parameter of interest does not match the correct row of the 'limits' matrix!")

  # get the values for the parameter of interest
  posterior <- posterior[, index]
  prior <- prior[, index]

  # get the minimum and the maximum value of the prior for that particular parameter
  minPrior <- limits[index, 1]; maxPrior <- limits[index, 2]

  # obtain the fit for the prior distribution
  fitPrior <- locfit::locfit(~prior, xlim = c(minPrior, maxPrior), alpha = 0.5)

  # when regression weights are supplied as input
  if(!is.null(weights)) {

    # use those weights as input in the locfit function
    fitPosterior <- locfit::locfit(~posterior, xlim = c(minPrior, maxPrior), alpha = 0.5, weights = weights)

  } else { # when no regression weights are supplied as input

    # run the locfit function without prior weights
    fitPosterior <- locfit::locfit(~posterior, xlim = c(minPrior, maxPrior), alpha = 0.5)
  }

  # create a vector of colours for the plot
  mycols <- RColorBrewer::brewer.pal(n = 6, name = "Paired")

  # initialize the plot but don't actually plot anything
  graphics::plot(fitPosterior, xlim = c(minPrior, maxPrior), type = "n", xlab = "Values of the parameter")

  # plot the density estimation of the posterior distribution on the created plot
  graphics::lines(fitPosterior, lwd = 2, col = mycols[6])

  # plot the density estimation of the prior distribution on the created plot
  graphics::lines(fitPrior, lty = 5, lwd = 2, col = mycols[2])

  # add a title to the plot
  graphics::title(main = paste("Posterior distribution for the", paste(param.name)))

  # add a legend relating the colors with the different distributions
  graphics::legend("topright", legend = c("Posterior", "Prior"), lwd = c(2, 2), lty = c(1, 5),
                   col = c(mycols[6], mycols[2]), box.lty = 0, inset = c(0.01, 0.01))
}


#' Plot the fit of a summary statistic to the target
#'
#' @param sumstat is a vector or matrix of simulated summary statistics. If this
#'   input is a vector, then each entry should correspond to a different
#'   simulation. If it is a matrix, then each row should be a different
#'   simulation and each column a different statistic. Note that this should be
#'   the entire set of simulated values.
#' @param target is an integer or a numeric vector containing the target of the
#'   parameter inference. If a single integer, then this should be the target
#'   summary statistic corresponding to the input `sumstat` vector. If this
#'   input is a vector, then the order of the entries in the vector should be
#'   the same as the order of the columns of the `sumstat` matrix input. Either
#'   way, this input should contain the value of the summary statistics
#'   calculated from observed data.
#' @param accepted is a vector or matrix of accepted summary statistics. If this
#'   input is a vector, then each entry should correspond to a different
#'   simulation. If it is a matrix, then each row should be a different
#'   simulation and each column a different statistic. Note that this should be
#'   summary statistics of the accepted simulations during parameter inference.
#' @param index is an optional non-negative integer. This input is only required
#'   when the `sumstat` and `accepted` inputs are matrices. In that instance, it
#'   will indicate which summary statistic to plot. It corresponds to the
#'   desired column of the `sumstat` and `accepted` matrices and to the entry of
#'   the `target` vector.
#' @param colour logical, indicating whether the plot should be a colour version
#'   (default) or a grayscale plot.
#'
#' @return a plot with the fit of the simulated summary statistics to the
#'   observed value. Both the density estimation of the entire simulated summary
#'   statistics and the accepted summary statistics are contrasted with the
#'   observed value.
#'
#' @importFrom rlang .data
#'
#' @examples
#' # load the matrix with parameter values
#' data(params)
#' # load the matrix with simulated parameter values
#' data(sumstats)
#' # load the matrix with the prior limits
#' data(limits)
#'
#' # select a random simulation to act as target just to test the function
#' target <- sumstats[10 ,]
#' # we should remove the random simulation from the sumstats and params matrices
#' sumstats <- sumstats[-10, ]; params <- params[-10, ]
#'
#' # parameter estimation for a single target
#' myabc <- singleABC(target = target, params = params, sumstats = sumstats,
#' limits = limits, tol = 0.01, method = "regression")
#'
#' # check the fit of a summary statistic to the target
#' plot_stats(sumstat = sumstats, target = target, accepted = myabc$ss, index = 5)
#'
#' # note that we performed parameter estimation for a single target
#' # because this function will only work when using a matrix
#'
#' @export
plot_stats <- function(sumstat, target, accepted, index = NA, colour = TRUE) {

  # if the class of the sumstat is not numeric, then the index should be supplied
  if(all(class(sumstat) != "numeric") & is.na(index))
    stop("Please define the index when using matrices with more than one summary statistic")

  # if the index is available
  if(!is.na(index)) {

    # check if the names of the sumstats in the matrix containing the simulated values match the names in the target
    if(any(colnames(sumstat) != names(target)))
      stop("sumstat names are not the same for the global summmary statistics and the target!")

    # check if the target input is in the correct format
    if(!inherits(target, "numeric"))
      stop("the 'target' input should be a vector with each sumstat value in a different entry")

    # get the name of the parameter of interest
    stat.name <- colnames(sumstat)[index]

    # check if the parameter of interest matches the correct row of the limits input
    if(stat.name != colnames(accepted)[index])
      stop("the name of the sumstat of interest does not match the correct column of the 'accepted' matrix!")

    # get the sumstat values for the stat of interest
    # 1. from the global sumstats i.e. all of the values from the simulations
    stat <- sumstat[, index]
    # 2. from the target i.e. the values computed from the real data as used as the target for the parameter inference
    target <- target[index]
    # 3. from the accepted sumstats i.e. the values that were close enough to the target during parameter inference
    accepted <- accepted[, index]
    # combine all of the different values into a single vector
    final <- c(stat, accepted)

  } else {

    # get the name of the parameter of interest
    stat.name <- names(target)

    # get the sumstat values for the stat of interest
    stat <- sumstat
    # combine all of the different values into a singe vector
    final <- c(stat, accepted)
  }

  # create a vector with the information about the origin of each of the statistic values
  info <- c(rep("global", times = length(stat)), rep("accepted", times = length(accepted)))
  # combine the vector with the statistics values with the vector containing the origin info into a single dataframe
  final <- data.frame(stats = final, info = info)

  # create a title for the plot
  main <- paste(stat.name, "from all simulations, accepted ones and target")

  # use ggplot to, well, plot the different sets of summary statistics
  p <- ggplot2::ggplot(final, ggplot2::aes(.data$stats, fill = .data$info)) + ggplot2::geom_density(alpha = 0.5) +
    # add the target, as a vertical line indicating the value, to the plot
    ggplot2::geom_vline(ggplot2::aes(xintercept = target, color = "target"), size = 1) +
    # set the desired color for the vertical line
    ggplot2::scale_color_manual(values = c("target" = "black")) +
    # add a label to the x-axis and add a title to the plot
    ggplot2::xlab(stat.name) + ggplot2::ggtitle(main) +
    # modify the visual aspect of the plot
    ggplot2::theme_light(base_size = 15, base_family = "", base_line_size = 0.2, base_rect_size = 1) +
    # define a series of visual settings - including size of legend text and ticks on the axis
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 20),axis.text.y = ggplot2::element_text(size = 20),
                   axis.title.x = ggplot2::element_text(size = 15, margin = ggplot2::margin(t = 8)),
                   axis.title.y = ggplot2::element_text(size = 15, margin = ggplot2::margin(r = 8)),
                   plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14)) +
    # remove some of the grid lines and remove the title from the legends
    ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank(), panel.grid.minor.y = ggplot2::element_blank(),
                   legend.title = ggplot2::element_blank())

  # if we want a version with no colours
  if(colour == FALSE) {
    # change the plot to contain only shades of grey
    p <- p + ggplot2::scale_fill_grey(start = 0.2, end = 0.9)
  }

  # output the plot
  p
}


#' Merge posterior distributions
#'
#' After using the [multipleABC()] function to perform parameter estimation with
#' Approximate Bayesian Computation for several targets, this function can be
#' used to merge the different posterior distributions.
#'
#' The posterior density will be estimated after simply merging the posteriors
#' computed from all target subset of loci and after weighting the posterior of
#' each target by its distance to the overall summary statistic mean. In other
#' words, each posterior will be weighted according to the distance between the
#' mean summary statistics of the subset of loci for which that posterior was
#' computed and the mean across all loci, giving more weight to sets of loci
#' with a mean closer to the overall mean.
#'
#' Additionally, if the regression weights are available, each accepted point
#' will be weighted by its regression weight and by distance of its associated
#' target. The combination of these weights will be used to merge the multiple
#' posteriors. The weighted mean, median, mode and quantiles will be computed
#' for each of these different posterior merging methods by using the
#' [weighted_stats()] and [mode_locfit()] functions. Note that this function
#' requires the package \pkg{locfit}.
#'
#' @param target a matrix or a list with target mean sumstat, where each entry
#'   corresponds to a vector of size n (n = number of summary statistics) with
#'   the summary statistics of each subset of loci.
#' @param global numeric vector of size n with mean summary statistics across
#'   all loci.
#' @param post list with sample of posterior obtained for each subset of loci.
#'   Each entry of the list is a matrix where each line corresponds to an
#'   accepted simulations (size S) and each column corresponds to a parameter.
#' @param a numeric value with the alpha parameter of the locfit function.
#' @param wtreg (optional) list with the weights of regression method. Each
#'   entry of the list is a numeric vector with weights for each accepted
#'   simulation (size S).
#'
#' @return list of locfit objects with the density of the posterior for each
#'   parameter and of mean, mode and quantiles obtained using weighted
#'   quantiles. The list has the following elements:
#'
#'   \item{merge}{obtained by simply merging all the posteriors into a single
#'   one and fitting a local regression without any prior weighting.}
#'
#'   \item{merged_stat}{posterior point estimates for the corresponding merging
#'   method, `merge`. This includes the median, mean, mode and various quantiles
#'   of the posterior.}
#'
#'   \item{weighted}{each target was weighted by its distance to the `global`
#'   summary statistics mean, giving more weight to the target subset of loci
#'   with mean summary statistics closer to the mean across the genome.}
#'
#'   \item{weighted_stat}{posterior point estimates for the corresponding
#'   merging method, `weighted`. This includes the median, mean, mode and
#'   various quantiles of the posterior.}
#'
#'   \item{merge_reg}{each accepted point was weighted by its regression
#'   weight.}
#'
#'   \item{merge_reg_stat}{posterior point estimates for the corresponding
#'   merging method, `merge_reg`. This includes the median, mean, mode and
#'   various quantiles of the posterior.}
#'
#'   \item{weighted_reg}{each target was weighted according to its distance to
#'   the overall mean and each point was weighted by its regression weight.}
#'
#'   \item{weighted_reg_stat}{posterior point estimates for the corresponding
#'   merging method, `weighted_reg`. This includes the median, mean, mode and
#'   various quantiles of the posterior.}
#'
#'   Details about the output can be found at:
#'   \url{https://aakinshin.net/posts/weighted-quantiles/} and
#'   \url{https://www.rdocumentation.org/packages/reldist/versions/1.6-6/topics/wtd.quantile}
#'
#' @examples
#' # load the matrix with parameter values
#' data(params)
#' # load the matrix with simulated parameter values
#' data(sumstats)
#' # load the matrix with the prior limits
#' data(limits)
#'
#' # select some random simulations to act as target just to test the function
#' targets <- sumstats[c(11:20) ,]
#' # we should remove those random simulation from the sumstats and params matrices
#' sumstats <- sumstats[-c(11:20), ]; params <- params[-c(11:20), ]
#'
#' # parameter estimation for multiple targets
#' myabc <- multipleABC(targets = targets, params = params, sumstats = sumstats, limits = limits,
#' tol = 0.01, method = "regression")
#'
#' # select a random simulation to act as the global value of the summary statistics
#' # ideally this should be computed from the entirety of the observed data
#' global <- sumstats[50, ]
#'
#' # merge the posterior distributions obtained in the previous step
#' mergepost(target = targets, global = global, post = myabc$adjusted, wtreg = myabc$weights)
#'
#' @export
mergepost <- function(target, global, post, a = 0.5, wtreg = NULL) {

  # if needed, transform target into a matrix, where each column is a sumstat and each row a different target subset
  if(is.list(target))
    target <- do.call(rbind, target)

  # check if any sumstat has no variance
  if(any(apply(target, MARGIN = 2, stats::var) == 0))
     stop("one of the summary statistics in the target has zero variance. Please check")

  # check that all input arguments have the correct size
  if(nrow(target) != length(post))
    stop("number of target subsets do not match in target and post!")
  # check size of the global input
  if(ncol(target) != length(global))
    stop("number of sumstats do not match in target and global!")
  # check if they have the same names
  if(any(colnames(target) != names(global)))
    stop("sumstat names are not the same for target and global!")

  # if wtreg is not null
  if(!is.null(wtreg)) {
    # check if wtreg has the correct size
    if(length(post) != length(wtreg))
      stop("number of target subsets do not match in wtreg and post!")
    # check if the number of accepted sims is correct
    if(sum(unlist(lapply(post, nrow)) != unlist(lapply(wtreg, length))) > 0)
      stop("number of accepted sims in post differ from wtreg")
  }

  # 1. compute the distance between each set of loci and the overall global mean
  # compute mean and sdsd of subset of loci to standardize sumstats before computing Euclidean distance
  meanloci <- apply(target, 2, mean)
  sdloci <- apply(target, 2, stats::sd)
  # check if the mean and sd have the correct size
  if(length(meanloci) != length(sdloci) | length(meanloci) != length(global))
    stop("mean and sd have wrong sizes")

  # standardize target and global
  target_st <- vapply(1:ncol(target), function(i) {(target[,i]-meanloci[i])/sdloci[i]}, FUN.VALUE = numeric(nrow(target)))
  global_st <- (global-meanloci)/sdloci

  # euclidean distance between each target and global
  target_dist <- apply(target_st, 1, function(x) {sqrt(sum((x-global_st)^2))})
  # check if the size of the target is correct
  if(length(target_dist) != nrow(target))
    stop("after computing target distance, sizes are incorrect")

  # 2. weight for each target using Epanechnicov kernel
  # dividing the distance by the maximum target distance
  target_wt <- 1-(target_dist/max(target_dist))^2

  # 3. merge all the posteriors from all target subset of loci
  postall <- do.call(rbind, post)
  # check if the dimensions are correct
  if(ncol(postall) != ncol(post[[1]]))
    stop("after merging, incorrect number of parameters")

  # 4. Estimate posterior density simply merging posterior of all target subset of loci
  post_add <- apply(postall, 2, function(x) locfit::locfit(~x, alpha=a, xlim=c(min(x),max(x))))
  # check if the dimensions are correct
  if(length(post_add) != ncol(post[[1]]))
    stop("incorrect number of params for post_add. Problem with locfit adding posterior")

  # compute weighted mean, median, mode and quantiles
  stat_add <- apply(postall, 2, function(x) {weighted_stats(x, w=rep.int(1,times=length(x)), prob=c(0.05, 0.25, 0.75, 0.95))})
  # compute mode and add to summary
  stat_add <- rbind(stat_add,vapply(1:length(post_add), function(i)
    mode_locfit(post_add[[i]], xlim=c(min(postall[,i]), max(postall[,i])), precision=1000), FUN.VALUE = numeric(1)))
  # add row names
  rownames(stat_add)[c(1,nrow(stat_add))] <- c("mean","mode")

  # 5. Estimate posterior density weighting the posterior of each target by distance to overall sumstat mean
  # such that target subset of loci with mean sumstat closer to the sumstat mean across the genome are given more weight
  # weights of target dist
  wtt <- rep(target_wt, each=nrow(post[[1]]))
  post_wtt <- apply(postall, 2, function(x) locfit::locfit(~x, alpha = a, xlim = c(min(x),max(x)), weights = wtt))
  # check if the dimensions are correct
  if(length(post_wtt) != ncol(post[[1]]))
    stop("incorrect number of params for post_wtt. Problem with locfit adding posterior")

  # compute weighted mean, median, mode and quantiles
  stat_wtt <- apply(postall, 2, function(x) {weighted_stats(x, w=wtt, prob=c(0.05, 0.25, 0.75, 0.95))})
  # compute mode and add to summary
  stat_wtt <- rbind(stat_wtt,vapply(1:length(post_wtt), function(i)
    mode_locfit(post_wtt[[i]], xlim = c(min(postall[,i]), max(postall[,i])), precision = 1000), FUN.VALUE = numeric(1)))
  # add row names
  rownames(stat_wtt)[c(1,nrow(stat_wtt))] <- c("mean","mode")

  # 6. If regression weights are available, estimates the posterior
  # adding all target subsets, but weighting each accepted point by its regression weight
  if(!is.null(wtreg)) {
    # merge all the weights
    wtall <- do.call(c, wtreg)

    # posterior considering weights of regression
    post_add_rw <- apply(postall, 2, function(x) locfit::locfit(~x, alpha = a, xlim = c(min(x),max(x)), weights = wtall))
    # check if the dimensions are correct
    if(length(post_add_rw) != ncol(post[[1]]))
      stop("incorrect number of params for post_add_rw. Problem with locfit adding posterior")

    # compute weighted mean, median, mode and quantiles
    stat_wtall <- apply(postall, 2, function(x) {weighted_stats(x, w=wtall, prob=c(0.05, 0.25, 0.75, 0.95))})
    # compute mode and add to summary
    stat_wtall <- rbind(stat_wtall,vapply(1:length(post_add_rw), function(i)
      mode_locfit(post_add_rw[[i]], xlim=c(min(postall[,i]), max(postall[,i])), precision = 1000), FUN.VALUE = numeric(1)))
    # add row names
    rownames(stat_wtall)[c(1,nrow(stat_wtall))] <- c("mean","mode")

    # posterior considering weights of regression and distance of target subset of loci
    # merging all target subsets, weighting by distance of each target and weighting each accepted point by its regression weight
    wtmerge <- (wtt*wtall)
    # posterior considering weights of regression
    post_wtt_rw <- apply(postall, 2, function(x) locfit::locfit(~x, alpha = a, xlim = c(min(x),max(x)), weights = wtmerge))

    # check if the dimensions are correct
    if(length(post_wtt_rw) != ncol(post[[1]]))
      stop("incorrect number of params for post_wtt_rw. Problem with locfit adding posterior")

    # compute weighted mean, median, mode and quantiles
    stat_wtt_rw <- apply(postall, 2, function(x) {weighted_stats(x, w=wtmerge, prob=c(0.05, 0.25, 0.75, 0.95))})
    # compute mode and add to summary
    stat_wtt_rw <- rbind(stat_wtt_rw,vapply(1:length(post_wtt_rw), function(i)
      mode_locfit(post_wtt_rw[[i]], xlim=c(min(postall[,i]), max(postall[,i])), precision = 1000), FUN.VALUE = numeric(1)))
    # add row names
    rownames(stat_wtt_rw)[c(1,nrow(stat_wtt_rw))] <- c("mean","mode")

    # output the merge locfit and the weighted locfit posterior, and weighted with regression
    res <- list(merge=post_add, merged_stat=stat_add, weighted=post_wtt, weighted_stat=stat_wtt, merge_reg=post_add_rw,
                merge_reg_stat=stat_wtall, weighted_reg=post_wtt_rw, weighted_reg_stat=stat_wtt_rw)

  } else {

    # output the merge locfit and the weighted locfit posterior
    res <- list(merge = post_add, merged_stat = stat_add, weighted = post_wtt, weighted_stat = stat_wtt)
  }

  return(res)
}


#' Plot the density estimation of a given parameter
#'
#' Plots a locfit object obtained after parameter estimation with Approximate
#' Bayesian Computation using the [multipleABC()] function and merging the
#' multiple posteriors with the [mergepost()] function.
#'
#' The [mergepost()] function includes different posterior merging methods and
#' produces locfit objects for each parameter and method. It is possible to
#' select which parameter to plot, with the `index` input, and whether to plot
#' the density estimation after each accepted point was weighted by its
#' regression weight and by distance of its associated target to the overall
#' mean of the data. If `regWeights` is set to FALSE, the density estimation
#' obtained without considering the regression weights will be plotted. If
#' `weighted` is set to FALSE, the density estimation obtained without
#' considering the distance between the mean summary statistics of the target
#' and the mean across all loci.
#'
#' @param prior is a vector or matrix of simulated parameter values i.e. numbers
#'   from the simulations. Each row or vector entry should be a different
#'   simulation and each column of a matrix should be a different parameter.
#'   This corresponds to the prior distribution and it should contain all the
#'   simulated parameter values.
#' @param merged_posterior is a list obtained by the [mergepost()] function. The
#'   output of that function produces a list with the locfit of the various
#'   parameters. This function plots those locfits.
#' @param index is an non-negative integer indicating which parameter to plot.
#'   It corresponds to the desired entry of the `merged_posterior` list. So, to
#'   plot the first parameter, corresponding to the first entry in the
#'   `merged_posterior` input select 1. To plot the second parameter, select 2
#'   and so on.
#' @param limits is a matrix with two columns and as many rows as there are
#'   parameters. Each row should contain the minimum value of the prior for a
#'   given parameter in the first column and the maximum value in the second
#'   column.
#' @param regWeights logical, indicating whether to plot the posterior density
#'   obtained from merging the multiple posteriors with or without the weights
#'   of the regression step. The default is TRUE.
#' @param weighted logical, indicating whether to plot the posterior density
#'   obtained from merging the multiple posteriors with or without weighting by
#'   the overall distance to the global mean. The default is TRUE.
#'
#' @return a plot of the density estimation of a given parameter. This plot will
#'   include a title with the name of the parameter. It will also include the
#'   density of the prior distribution for that parameter. The density
#'   estimation shown here is obtained after merging multiple posteriors for
#'   that parameter.
#'
#' @examples
#' # load the matrix with parameter values
#' data(params)
#' # load the matrix with simulated parameter values
#' data(sumstats)
#' # load the matrix with the prior limits
#' data(limits)
#'
#' # select some random simulations to act as target just to test the function
#' targets <- sumstats[c(11:20) ,]
#' # we should remove those random simulation from the sumstats and params matrices
#' sumstats <- sumstats[-c(11:20), ]; params <- params[-c(11:20), ]
#'
#' # parameter estimation for multiple targets
#' myabc <- multipleABC(targets = targets, params = params, sumstats = sumstats, limits = limits,
#' tol = 0.01, method = "regression")
#'
#' # select a random simulation to act as the global value of the summary statistics
#' # ideally this should be computed from the entirety of the observed data
#' global <- sumstats[50, ]
#'
#' # merge the posterior distributions obtained in the previous step
#' mymerge <- mergepost(target = targets, global = global, post = myabc$adjusted,
#' wtreg = myabc$weights)
#'
#' # plot the merged posterior distribution
#' plot_weighted(prior = params, merged_posterior = mymerge, index = 7, limits = limits)
#'
#' # note that this is just an example!
#' # we don't have enough simulations to obtain credible results
#'
#' @export
plot_weighted <- function(prior, merged_posterior, index, limits, regWeights = TRUE, weighted = TRUE) {

  # check if the 'merged_posterior' list is in the correct format
  if(!("merged_stat" %in% names(merged_posterior)))
    stop("wrong 'merged_posterior' input! Check if you are using the correct list")

  # check if the names of the parameters in the posteriors match the names in the priors
  if(any(names(merged_posterior$weighted_reg_stat) != colnames(prior))) {
    stop("parameter names are not the same for the posterior and the prior!")
  }

  # check if the limits input is in the correct format
  if(length(dim(limits)) < 2)
    stop("the 'limits' input should be a matrix with each parameter in a different row!")

  # get the name of the parameter of interest
  param.name <- names(merged_posterior$weighted_reg)[index]

  # check if the parameter of interest matches the correct row of the limits input
  if(param.name != rownames(limits)[index])
    stop("the name of the parameter of interest does not match the correct row of the 'limits' matrix!")

  # get the posterior obtained by using the regression weights and weighting by overall distance to the global mean
  fitPosterior <- merged_posterior$weighted_reg[[index]]

  # in this instance, get the posterior with only the regression weights
  if(weighted == FALSE)
    fitPosterior <- merged_posterior$merge_reg[[index]]

  # here, get the posterior by weighting by overall distance to the global mean
  if(regWeights == FALSE)
    fitPosterior <- merged_posterior$weighted[[index]]

  # and the posterior without regression weights and without considering overall distance to the global mean
  if(weighted == FALSE && regWeights == FALSE)
    fitPosterior <- merged_posterior$merge[[index]]

  # get the values for the parameter of interest
  prior <- prior[, index]

  # get the minimum and the maximum value of the prior for that particular parameter
  minPrior <- limits[index, 1]; maxPrior <- limits[index, 2]

  # obtain the fit for the prior distribution
  fitPrior <- locfit::locfit(~prior, xlim = c(minPrior, maxPrior), alpha = 0.5)

  # create a vector of colours for the plot
  mycols <- RColorBrewer::brewer.pal(n = 6, name = "Paired")

  # initialize the plot but don't actually plot anything
  graphics::plot(fitPosterior, xlim = c(minPrior, maxPrior), type = "n", xlab = "Values of the parameter")

  # plot the density estimation of the posterior distribution on the created plot
  graphics::lines(fitPosterior, lwd = 2, col = mycols[6])

  # plot the density estimation of the prior distribution on the created plot
  graphics::lines(fitPrior, lty = 5, lwd = 2, col = mycols[2])

  # add a title to the plot
  graphics::title(main = paste("Posterior distribution for the", paste(param.name)))

  # add a legend relating the colors with the different distributions
  graphics::legend("topright", legend = c("Posterior", "Prior"), lwd = c(2, 2), lty = c(1, 5),
                   col = c(mycols[6], mycols[2]), box.lty = 0, inset = c(0.01, 0.01))
}

#' Calculate point estimates from the posterior distribution
#'
#' Given a set of samples from the posterior distribution, computes the mean,
#' median and mode of the posterior.
#'
#' If `method` is "regression", the regression weights must also be made
#' available. These will be used to compute the weighted mean, weighted median
#' and weighted mode of the posterior.
#'
#' @param posterior is a matrix or a vector with samples from the posterior
#'   distribution obtained from ABC parameter estimation. If this input is a
#'   matrix, then each row should correspond to an accepted simulation (size S)
#'   and each column to a different parameter.
#' @param limits is a vector if there is only one parameter or a matrix if there
#'   are multiple parameters. In this latter instance, each row should
#'   correspond to a different parameter. In either instance, and considering
#'   matrix rows as vectors, then the first entry of the vector should be the
#'   minimum value of the prior and the second entry should be the maximum value
#'   (for any given parameter).
#' @param method either "rejection" or "regression" indicating whether a
#'   rejection sampling algorithm or a local linear regression algorithm were
#'   used during ABC parameter estimation.
#' @param wtreg is a required numeric vector if the method is "regression". It
#'   should contain the weights for each accepted simulation (size S).
#'
#' @return a matrix with the mode, median and mean of the posterior distribution
#'   for each parameter. Each point estimate is a different row and each
#'   parameter a different column.
#'
#' @examples
#' # load the matrix with parameter values
#' data(params)
#' # load the matrix with simulated parameter values
#' data(sumstats)
#' # load the matrix with the prior limits
#' data(limits)
#'
#' # select a random simulation to act as target just to test the function
#' target <- sumstats[10 ,]
#' # we should remove the random simulation from the sumstats and params matrices
#' sumstats <- sumstats[-10, ]; params <- params[-10, ]
#'
#' # parameter estimation for a single target
#' myabc <- singleABC(target = target, params = params, sumstats = sumstats,
#' limits = limits, tol = 0.01, method = "regression")
#'
#' # compute point estimates from the posterior distribution
#' poststat(posterior = myabc$adjusted, limits = limits, method = "regression", wtreg = myabc$wt)
#'
#' @export
poststat <- function(posterior, limits, method, wtreg = NULL) {

  # get the number of parameters
  nparams <- ncol(posterior)

  # check if the regression weights are supplied when the method is set as regression
  if(method == "regression" & is.null(wtreg))
    stop("when method is regression you should also supply the weights of the regression method")

  # check if the limits input is a matrix
  if(!is.matrix(limits)) # if not, convert it into a matrix
    limits <- matrix(data = limits, nrow = nparams)

  # check if the names of the parameters in the posterior matrix match the names in the xlim input
  if(any((colnames(posterior) == row.names(limits)) == FALSE))
    stop("The names of parameters in the posterior matrix do not match the names in the xlim input")

  # when method is set as regression, compute mean, median and mode accordingly
  if(method == "regression") {

    # get the median of the posterior
    post.median <- apply(posterior, 2, function(p) MetricsWeighted::weighted_quantile(x = p, w = wtreg, probs = 0.5))
    # get the mean of the posterior
    post.mean <- sapply(1:nparams, function(p) stats::weighted.mean(x = posterior[, p], w = wtreg))
    # get the mode of the posterior
    post.mode <- sapply(1:nparams, function(p) getmode(x = posterior[, p], xlim = limits[p, ], weights = wtreg))

  } else { # when method is not regression

    # get the median of the posterior
    post.median <- apply(posterior, 2, stats::median)
    # get the mean of the posterior
    post.mean <- apply(posterior, 2, mean)
    # get the mode of the posterior
    post.mode <- sapply(1:nparams, function(p) getmode(x = posterior[, p], xlim = limits[p, ]))
  }

  # combine the different statistics into a matrix
  stats <- rbind("mode" = post.mode, "median" = post.median, "mean" = post.mean)
  # output that the matrix as the result of this function
  stats
}


#' Organize point estimates from multiple posterior distributions
#'
#' Combines the point estimates of multiple posterior distributions into more
#' easily read matrices.
#'
#' This is a purely organizational function that combines the mode, median and
#' mean point estimates of multiple posterior distributions into its own matrix.
#' Thus, each point estimate will have its own unique matrix. For instance, this
#' will create a "median" matrix with the median of each posterior.
#'
#' @param posterior_stats is a list where each entry corresponds to a different
#'   trial i.e. each entry contains the information obtained for a different
#'   target for ABC parameter estimation. Each entry should be a matrix
#'   containing, in each row, the "mode", "median" and "mean" of the posterior
#'   distribution for each of the parameters.
#'
#' @return a list with the following elements:
#'
#'   \item{mode}{mode of the posterior for each target and parameter.}
#'
#'   \item{median}{median of the posterior for each target and parameter.}
#'
#'   \item{mean}{mean of the posterior for each target and parameter.}
#'
#' @keywords internal
#'
#' @export
organize.poststat <- function(posterior_stats) {

  # obtain the mode of the posterior for each parameter and for each trial
  post.mode <- t(sapply(posterior_stats, function(trial) trial["mode", ]))
  # obtain the median of the posterior for each parameter and for each trial
  post.median <- t(sapply(posterior_stats, function(trial) trial["median", ]))
  # obtain the mean of the posterior for each parameter and for each trial
  post.mean <- t(sapply(posterior_stats, function(trial) trial["mean",]))

  # combine each statistics in an appropriate list entry
  posterior_stats <- list("mode" = post.mode, "median" = post.median, "mean" = post.mean)

  # output a list where each entry contains a different statistic of the posterior distribution
  posterior_stats
}


#' Calculate cross-validation prediction error of parameter estimation
#'
#' This function calculates the prediction error between estimates obtained
#' after a leave-one-out cross validation for ABC and the true parameter values.
#'
#' The prediction error is calculated as \code{sum((E-T)^2) / (nval * var(T))},
#' where T is the true parameter value, E is the estimated parameter value, and
#' nval is the number of points where the true and predicted values were
#' compared.
#'
#' @param true is a matrix where each row corresponds to the true parameter
#'   values of a given simulation and each column to a different parameter.
#'   These parameters where used as the pseudo-observed targets in the
#'   simulation study.
#' @param estimated is a matrix with the estimated parameter values. Each row
#'   corresponds to the estimate of the true parameter values present in the
#'   corresponding row of the `true` matrix. And each column should correspond
#'   to a different parameter.
#'
#' @return a numeric vector with the prediction error for each parameter. If
#'   column names are present in the input matrices, then this vector will also
#'   be named with the parameter names.
#'
#' @keywords internal
#'
#' @export
errorABC <- function(true, estimated) {

  # check that true and estimated have the same size
  if(ncol(true) != ncol(estimated) | nrow(true) != nrow(estimated)) {
    print(estimated)
    print(true)
    stop(paste("error", estimated, " different from ", true))
  }

  # get the number of evaluations performed
  nval <- nrow(true)

  # compute the square difference between the true values and the estimates
  sqdiff <- apply((estimated - true)^2, 2, sum)
  # and the variance of the true values
  truevar <- apply(true, 2, stats::var)*nval
  # compute the prediction error
  prederr <- sqdiff/truevar

  # output the prediction error
  prederr
}


#' Perform an Approximate Bayesian Computation simulation study
#'
#' Perform a leave-one-out cross validation for ABC via subsequent calls to the
#' [singleABC()] function.
#'
#' This function allows users to evaluate the impact of different tolerance rate
#' on the quality of the estimation with ABC and whether a local linear
#' regression algorithm improves the estimates. In subsequent steps, different
#' point estimates of the posterior estimates can be compared with the true
#' values, allowing the users to select the point estimate that leads to lower
#' errors. Thus, performing a leave-one-out cross validation aids in selecting
#' which point estimate is best - the mean, median or mode.
#'
#' @param params is a vector or matrix of simulated parameter values i.e.
#'   numbers from the simulations. Each row or vector entry should be a
#'   different simulation and each column of a matrix should be a different
#'   parameter. This is the dependent variable for the regression, if a
#'   regression step is performed.
#' @param sumstats is a vector or matrix of simulated summary statistics. Each
#'   row or vector entry should be a different simulation and each column of a
#'   matrix should be a different statistic. These act as the independent
#'   variables if a regression step is performed.
#' @param limits is a matrix with two columns and as many rows as there are
#'   parameters. Each row should contain the minimum value of the prior for a
#'   given parameter in the first column and the maximum value in the second
#'   column.
#' @param nval size of the cross-validation sample i.e. how many different
#'   evaluations should be performed. Each evaluation corresponds to a different
#'   target for the parameter estimation.
#' @param tol is the tolerance rate, indicating the required proportion of
#'   points accepted nearest the target values.
#' @param method either "rejection" or "regression" indicating whether a
#'   regression step should be performed during ABC parameter estimation.
#' @param parallel logical, indicating whether this function should be run using
#'   parallel execution. The default setting is FALSE, meaning that this
#'   function will utilize a single core.
#' @param ncores a non-negative integer that is required when `parallel` is
#'   TRUE. It specifies the number of cores to use for parallel execution.
#'
#' @return a list with the following elements:
#'
#'   \item{true}{The parameter values of the simulations that served as
#'   validation.}
#'
#'   \item{rej}{a list with the estimated parameter values under the rejection
#'   algorithm and using three different point estimates: mode, median and mean.
#'   The final entry of the list is the prediction error for each parameter,
#'   considering each of those point estimates as the estimated value.}
#'
#'   \item{reg}{if method is "regression" then this is a list with the estimated
#'   parameter values under the regression algorithm and using three different
#'   point estimates: mode, median and mean. The final entry of the list is the
#'   prediction error for each parameter, considering each of those point
#'   estimates as the estimated value.}
#'
#' @examples
#' # load the matrix with parameter values
#' data(params)
#' # load the matrix with simulated parameter values
#' data(sumstats)
#' # load the matrix with the prior limits
#' data(limits)
#'
#' # perform a leave-one-out cross validation for ABC
#' simulationABC(params = params, sumstats = sumstats, limits, nval = 10,
#' tol = 0.01, method = "regression")
#'
#' @export
simulationABC <- function(params, sumstats, limits, nval, tol, method, parallel = FALSE, ncores = NA) {

  # check if the nval argument is present
  if(missing(nval))
    stop("'nval' must be supplied")

  # check if the method is correctly defined
  if(method != "rejection" & method != "regression")
    stop("method must be either rejection or regression")

  # when dealing with a single summary statistic - in vector format - convert that vector into a matrix with one column
  if(is.vector(sumstats)) sumstats <- matrix(sumstats, ncol = 1)

  # get the number of parameters
  nparams <- ncol(params)
  # get the names of the parameters
  param.names <- colnames(params)

  # select random rows of the matrix with the simulated summary statistics to act as the target
  # first, create a random index of the rows that should be set as the target
  index <- sample(1:nrow(sumstats), nval)
  # then use that index to create a target matrix - each row is a different target
  target <- sumstats[index, ]

  # get the true value of the parameters
  true <- params[index, ]

  # if parallel is TRUE, this functions performs parallel execution of the simulation study
  if(parallel == TRUE) {

    # so %dopar% doesn't need to be attached
    `%dopar%` <- foreach::`%dopar%`

    # check if the number of cores are specified
    if(is.na(ncores))
      stop("Please specify the number of cores to use for parallel execution")

    # set binding for global variable
    i <- NULL

    # set the number of cores to utilize
    doParallel::registerDoParallel(cores = ncores)

    # run ABC using parallel
    myabc <- foreach::foreach(i = 1:nval) %dopar% {
      singleABC(target = target[i, ], params = params[-index[i], ], sumstats = sumstats[-index[i], ],
                limits = limits, tol = tol, method = method) }

  } else {

    # run ABC without parallel execution
    myabc <- lapply(1:nval, FUN = function(i)
      singleABC(target = target[i, ], params = params[-index[i], ], sumstats = sumstats[-index[i], ],
                limits = limits, tol = tol, method = method))
  }

  # if method is set to regression
  if(method == "regression") {

    # get the adjusted parameter inferences
    loc <- lapply(myabc, function(i) i[["loc.prediction"]])

    # organize the previous list
    loc <- organize.poststat(posterior_stats = loc)

    # compute the prediction error
    loc.prederr <- t(sapply(loc, function(stat) errorABC(true = true, estimated = stat)))

  } else {

    # set the output of the function - if no regression step is performed
    loc <- NA
  }

  # get the estimates obtained with the rejection method
  rej <- lapply(myabc , function(i) i[["rej.prediction"]])

  # organize the previous list
  rej <- organize.poststat(posterior_stats = rej)

  # compute the prediction error
  prederr <- t(sapply(rej, function(stat) errorABC(true = true, estimated = stat)))

  # if method is set to regression - add the prediction error to the loc list
  # add a entry to the list, containing the prediction error obtained when using a regression based ABC algorithm
  if(method == "regression")
    loc$error <- loc.prederr

  # add the prediction error obtained when using a rejection based ABC algorithm to the rej list
  rej$error <- prederr

  # create the final output of this function
  out <- list("true" = true, "rej" = rej, "reg" = loc)

  # output the prediction error
  out
}


#' Prediction error plots for ABC
#'
#' Plots the prediction error computed from a leave-one-out cross validation for
#' ABC parameter inference.
#'
#' These plots help in visualizing the quality of the estimation and the effect
#' of the chosen tolerance level or point estimate statistic.
#'
#' @param true is a numeric vector containing the true parameter values.
#' @param estimated a numeric vector containing the estimated parameter values.
#' @param transformation default is none. It can also be 'log' if you wish to
#'   transform both the true and estimated values using a log10 scale.
#' @param param.name is an optional character input defining the name of the
#'   parameter you are looking at.
#' @param main is a optional character argument that will be used as the title
#'   for the plot. If this input argument is not included, this function will do
#'   its best to create an appropriate title.
#'
#' @return a plot of the estimated value of the parameter (in the y-axis) versus
#'   the true parameter value (in the x-axis). A line marking the correspondence
#'   between the true and estimated values is also plotted. Thus, the closer the
#'   points are to that line, the lower the prediction error is.
#'
#' @keywords internal
#'
#' @export
plot_error <- function(true, estimated, transformation = "none", param.name = NULL, main = NULL) {

  # First, start by choosing some colors - you can use RColorBrewer to get some nice colours
  mycols <- RColorBrewer::brewer.pal(8, "Greys")

  # if transformation is set to log, transform the data accordingly
  if(transformation == "log") {
    true <- log10(true)
    estimated <- log10(estimated)
  }

  # check if the name of the parameter was supplied as input and if not, create a generic parameter name
  if(is.null(param.name))
    param.name <- "p1"

  # if no plot title is available
  if(is.null(main))
    main <- paste("Prediction error for", paste(param.name))

  # combine the true value and the estimation into a dataframe
  mydf <- data.frame(true, estimated)

  # set the necessary information - data for the axis, etc
  ggplot2::ggplot(data = mydf, ggplot2::aes(x = true, y = estimated)) +
    # set the color and the transparency of the data points
    ggplot2::geom_point(alpha = 0.4, colour = mycols[8]) +
    # create a trend line and set the transparency
    ggplot2::geom_abline(alpha = 0.8) +
    # set the title of the plot
    ggplot2::ggtitle(main) +
    # set the legend for the y and x axis
    ggplot2::ylab("estimated value of the parameter") + ggplot2::xlab("true value of the parameter") +
    # define a series of visual settings - including size of legend text and ticks on the axis
    ggplot2::theme_light(base_size = 15, base_family = "", base_line_size = 0.2, base_rect_size = 1) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 20), axis.text.y = ggplot2::element_text(size = 20),
                   axis.title.x = ggplot2::element_text(size = 16, margin = ggplot2::margin(t = 8)),
                   axis.title.y = ggplot2::element_text(size = 16, margin = ggplot2::margin(r = 8)),
                   plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")) +
    # remove some of the grid lines
    ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank(), panel.grid.minor.y = ggplot2::element_blank())
}


#' Prediction error plots for ABC using a list
#'
#' Plots the prediction error computed from a leave-one-out cross validation for
#' ABC parameter inference. This function takes as input a list created when
#' performing cross validation and allows the user to select which ABC algorithm
#' and point estimate statistic to plot.
#'
#' These plots help in visualizing the quality of the estimation and the effect
#' of the chosen tolerance level or point estimate statistic.
#'
#' @param x is a list produced by a leave-one-out cross validation of ABC. This
#'   list should contain the prediction errors computed using the rejection
#'   and/or regression algorithm. For each of those methods, the prediction
#'   error obtained using three different point estimates of the posterior
#'   should be included in this list.
#' @param method a character that can be either 'rej' or 'reg' indicating
#'   whether you wish to plot the prediction error computed with a rejection or
#'   regression based ABC algorithm.
#' @param statistic a character that can be 'mode', 'median' or 'mean'
#'   indicating if you wish to plot the prediction error obtained using the
#'   mode, median or mean as the point estimate of the posterior.
#' @param index an integer indicating which parameter to look at. It corresponds
#'   to a column on a matrix. So, to plot the first parameter, corresponding to
#'   the first column, select 1. To plot the second parameter, select 2 and so
#'   on.
#' @param transformation default is none. It can also be 'log' if you wish to
#'   transform both the true and estimated values using a log10 scale.
#' @param main is an optional character input. It will be used as the title of
#'   the plot. If NULL (default), then a generic title will be used instead.
#'
#' @return a plot of the estimated value of the parameter (in the y-axis) versus
#'   the true parameter value (in the x-axis). A line marking the perfect
#'   correspondence between the true and estimated values is also plotted. Thus,
#'   the closer the points are to that line, the lower the prediction error is.
#'
#' @examples
#' # load the matrix with parameter values
#' data(params)
#' # load the matrix with simulated parameter values
#' data(sumstats)
#' # load the matrix with the prior limits
#' data(limits)
#'
#' # perform a leave-one-out cross validation for ABC
#' mysim <- simulationABC(params = params, sumstats = sumstats, limits, nval = 10,
#' tol = 0.1, method = "regression")
#'
#' # plot the prediction error for a given parameter
#' plot_errorABC(x = mysim, method = "reg", statistic = "median", index = 1)
#'
#' @export
plot_errorABC <- function(x, method, statistic, index, transformation = "none", main = NULL) {

  # check if the method input is correct
  if(method != "rej" && method != "reg")
    stop("method should be either 'rej' or 'reg'")

  # check if the statistic input is correct
  if((statistic %in% c("mode", "median", "mean")) == FALSE)
    stop("statistic should be set as 'mode', 'median' or 'mean'")

  # get the name of the parameter to be plotted
  param.name <- colnames(x[["true"]])[index]

  # get the true parameter value - for the parameter of interest
  true <- x[["true"]][, index]

  # get the estimated value of the parameter of interest - using the chosen method and statistic
  estimated <- x[[method]][[statistic]][, index]

  # plot the prediction error, obtained using the chosen method and point estimate, of the parameter of interest
  plot_error(true = true, estimated = estimated, transformation = transformation, param.name = param.name, main = main)
}


#' Perform model selection with Approximate Bayesian Computation
#'
#' Estimates posterior model probabilities using Approximate Bayesian
#' Computation (ABC).
#'
#' Prior to using this function, simulations must have been performed under, at
#' least, two different models. When `method` is "rejection", the posterior
#' posterior probability of a given model is approximated by the proportion of
#' accepted simulations of that particular model. Note that this approximation
#' is only valid if all models where, a priori, equally likely and if the number
#' of simulations performed is the same for all models. When the `method` is set
#' to "regression", a multinomial logistic regression is used to estimate the
#' posterior model probabilities. This multinomial regression is implemented in
#' the \link[nnet]{multinom} function.
#'
#' @param target is a vector with the target summary statistics. These are
#'   usually computed from observed data.
#' @param index is a vector of model indices. This can be a a character vector
#'   of model names, repeated as many times as there are simulations for each
#'   model. This vector will be coerced to factor and it must have the same
#'   length as \code{nrow(sumstats)} to indicate which row of the `sumstats`
#'   matrix belongs to which model.
#' @param sumstats is a vector or matrix containing the simulated summary
#'   statistics for all the models. Each row or vector entry should be a
#'   different simulation and each column of a matrix should be a different
#'   statistic. The order must be the same as the order of the models in the
#'   `index` vector.
#' @param tol is a numerical value, indicating the required proportion of points
#'   nearest the target values (tolerance).
#' @param method a character string, either "rejection" or "regression",
#'   indicating which algorithm should be used for model selection.
#' @param warning logical, if TRUE (default) warnings produced while running
#'   this function, mainly related with accepting simulations for just one of
#'   the models, will be displayed.
#'
#' @return a list with the following elements:
#'
#'   \item{method}{the method used for model selection.}
#'
#'   \item{indices}{a vector of model indices in the accepted region. In other
#'   words, this vector contains the name of the accepted model for each
#'   accepted point.}
#'
#'   \item{pred}{a vector of model probabilities.}
#'
#'   \item{ss}{the summary statistics in the accepted region.}
#'
#'   \item{weights}{vector of regression weights when method is regression.}
#'
#'   \item{nmodels}{the number of a priori simulations performed for each
#'   model.}
#'
#' @examples
#' # load the matrix with simulated parameter values
#' data(sumstats)
#'
#' # select a random simulation to act as target just to test the function
#' target <- sumstats[10 ,]
#'
#' # create a "fake" vector of model indices
#' # this assumes that half the simulations were from one model and the other half from other model
#' # this is not true but serves as an example of how to use this function
#' index <- c(rep("model1", nrow(sumstats)/2), rep("model2", nrow(sumstats)/2))
#'
#' # perform model selection with ABC
#' modelSelect(target = target, index = index, sumstats = sumstats, tol = 0.01, method = "regression")
#'
#' @export
modelSelect <- function(target, index, sumstats, tol, method, warning = TRUE) {

  # check if the method is correctly defined
  if(!any(method == c("rejection", "regression")))
    stop("method must be 'rejection' or 'regression")

  # check if the 'index' input is in the correct format
  if(!is.vector(index))
    stop("the 'index' input has to be a vector. Please check")

  # check if there are at least two different models
  if(length(unique(index)) == 1)
    stop("At least two different models must be given")

  # when dealing with a single sumstat - in the form of a vector - convert that vector into a matrix with one column
  if(is.vector(sumstats)) sumstats <- matrix(sumstats, ncol = 1)

  # check if the number of sumstats in the target matches the number in the 'sumstats' matrix
  if(length(target) != ncol(sumstats))
    stop("Number of summary statistics in 'target' has to be the same as in 'sumstats'")

  # check if the number of model indices matches the number of different simulations
  if(length(index) != nrow(sumstats))
    stop("'index' must be the same length as the number of rows in 'sumstats'")

  # get the number of summary statistics
  nss <- length(sumstats[1, ])

  # get the names of the summary statistics
  statnames <- colnames(sumstats)

  # transform the vector of model indices into a factor
  index <- factor(index)
  # get the name of the models
  mymodels <- levels(index)

  # scale everything
  # scale the matrix containing the summary statistics
  scaled.sumstat <- apply(X = sumstats, MARGIN = 2, FUN = function(col) normalise(x = col, y = col))
  # and the vector with the target
  scaled.target <- vapply(1:nss, FUN = function(i) normalise(target[i], sumstats[, i]), FUN.VALUE = numeric(1))

  # calculate euclidean distance
  dst <- apply(X = scaled.sumstat, MARGIN = 1, FUN = function(row) euclidean(a = row, b = scaled.target))

  # wt1 defines the region we're interested in
  # first define the threshold value
  abstol <- stats::quantile(x = dst, probs = tol)
  # create a vector of TRUE/FALSE indicating which values are below (or equal to) the threshold
  wt1 <- dst <= abstol

  # select summary statistics in region
  ss <- scaled.sumstat[wt1, ]
  # get the index of the accepted models
  ok.models <- index[wt1]
  # how many different models were accepted?
  nmodels <- length(unique(ok.models))

  # if the method selected is the 'rejection' method
  if(method == "rejection") {

    # set the weights in this instance - no further regression step is performed
    weights <- NULL
    # model probabilities using a simple rejection
    pred <- c(table(ok.models)/length(ok.models))
    # get the sumstats in the accepted region
    ss <- sumstats[wt1, ]
    # get the number of simulations per model
    nmodels <- c(table(index))

    # define the output of the function under these conditions
    out <- list(method = method, indices = ok.models, pred = pred, ss = ss, weights = weights, nmodels = nmodels)

    # stop the function and output the results under a rejection algorithm
    stop(return(out))
  }

  # create the formula for the multinomial regression
  fml <- stats::as.formula(paste("ok.models ~ ", paste(statnames, collapse = "+")))

  # calculate the weights for the regression - using the epanechnikov kernel
  weights <- 1 - (dst[wt1]/abstol)^2
  # remove any unnecessary names of the weights
  weights <- unname(weights)

  # check if only a single model was accepted
  if(nmodels < length(mymodels)) {
    # set the warning to output
    warning(paste("There are", length(mymodels),"models but only", nmodels, "for which simulations have been accepted.
                  \nNo regression is performed, method is set to rejection.
                  \nConsider increasing the tolerance rate.\n"), sep = "", immediate. = warning, call. = FALSE)

    # set the weights in this instance - no further regression step is performed
    weights <- NULL
    # model probabilities using a simple rejection
    pred <- c(table(ok.models)/length(ok.models))
    # get the sumstats in the accepted region
    ss <- sumstats[wt1, ]
    # define the output of the function under these conditions
    out <- list(indices = ok.models, pred = pred, ss = ss, weights = weights, nmodels = c(table(index)))

    # stop the function and output the results under a rejection algorithm
    stop(return(out))
  }

  # calculate the number of weights for multinom
  mymnw <- (nss+2) * length(mymodels)

  # convert the accepted scaled sumstats into a data frame
  ss <- data.frame(ss)
  # add the column names
  colnames(ss) <- statnames

  # fit a multinomial log-linear model
  fit1 <- nnet::multinom(fml, data = ss, weigths = weights, trace = FALSE, MaxNWts = mymnw + 1)

  # transform the scaled target into a data frame
  scaled.target <- as.data.frame(matrix(scaled.target, nrow = 1))
  # add the name of the sumstats to each entry
  names(scaled.target) <- statnames
  # get predictions from the results of multinomial log-linear model
  pred <- stats::predict(fit1, scaled.target, type = "probs")

  # when pred only has one entry, create the entry for the other model
  if(length(pred) == 1) {
    pred <- c(1 - pred, pred)
    names(pred) <- levels(ok.models)
  }

  # correction for potentially different numbers of simulations per models
  ratio <- (pred*length(index)*tol) / table(index)
  pred <- ratio/sum(ratio)
  attributes(dimnames(pred)) <- NULL

  # get the number of simulations per model
  nmodels <- c(table(index))

  # create the output of the function after the regression step
  out <- list(method = method, indices = ok.models, pred = pred, ss = sumstats[wt1, ], weights = weights, nmodels = nmodels)
  # output the result of the function
  out
}


#' Posterior model probabilities
#'
#' Extract the posterior model probabilities and obtain a summary of model
#' selection performed with Approximate Bayesian Computation.
#'
#' This function produces an easy-to-read output of the model selection step. It
#' also computes the Bayes factors.
#'
#' @param object a list created by the [modelSelect()] function, containing
#'   results of model selection with Approximate Bayesian Computation.
#' @param print logical, if TRUE (default), then this function prints the mean
#'   models probabilities.
#'
#' @return a list with two main elements if model selection used the regression
#'   algorithm or a single element if only the rejection step was used:
#'
#'   \item{rejection}{results of model selection based on the rejection method.
#'   This element contains two entries, the first is an object of class numeric
#'   with the posterior model probabilities and the second are the Bayes factors
#'   between pairs of models.}
#'
#'   \item{mnlogistic}{results of model selection based on the regression
#'   method. This element contains two entries, the first is an object of class
#'   numeric with the posterior model probabilities and the second are the Bayes
#'   factors between pairs of models.}
#'
#' @examples
#' # load the matrix with simulated parameter values
#' data(sumstats)
#'
#' # select a random simulation to act as target just to test the function
#' target <- sumstats[10 ,]
#'
#' # create a "fake" vector of model indices
#' # this assumes that half the simulations were from one model and the other half from other model
#' # this is not true but serves as an example of how to use this function
#' index <- c(rep("model1", nrow(sumstats)/2), rep("model2", nrow(sumstats)/2))
#'
#' # perform model selection with ABC
#' mysel <- modelSelect(target = target, index = index, sumstats = sumstats,
#' tol = 0.01, method = "regression")
#'
#' # compute posterior model probabilities
#' summary_modelSelect(object = mysel)
#'
#' @export
summary_modelSelect <- function(object, print = TRUE) {

  # get the method used in the model selection
  method <- object$method

  # get the number of posterior samples
  npost <- length(object$indices)
  # obtain the vector of model probabilities
  pred <- object$pred
  # get the vector of model indices in the accepted region using the rejection method
  allvals <- object$indices
  # which models are present in the posterior samples
  postmod <- levels(object$indices)
  # how many models are present in the posterior samples
  nmod <- length(postmod)
  # how many models were considered a priori
  nmodels <- object$nmodels

  # proportion of accepted models using a simple rejection
  pr.rej <- table(allvals)/length(allvals)
  # correction for potentially different numbers of simulations per models
  ratio <- (pr.rej*npost) / nmodels; pr.rej <- ratio/sum(ratio)
  # get the the names of the accepted models using a simple rejection
  prnames <- dimnames(pr.rej)$allvals
  # add those names to the matrix
  pr.rej <- c(pr.rej); names(pr.rej) <- prnames

  if(print) {
    cat(paste("Data:\n results based on ", npost," posterior samples\n\n", sep = ""))
    cat(paste("Models a priori:\n "))
    cat(names(object$nmodels), sep = ", ")
    cat("\n")
    cat(paste("\nModels a posteriori:\n "))
    cat(postmod, sep = ", ")
    if(length(unique(nmodels)) > 1) {
      cat("\n")
      warning("Posterior model probabilities are corrected for unequal number of simulations per models.",
              immediate. = T, call. = F)
    }
    cat("\n\n")
    cat("Proportion of accepted simulations (rejection):\n")
    print(round(pr.rej, digits = 3))

    if(method != "rejection") { # when a regression was performed
      cat("\n")
      cat(paste("Posterior model probabilities (mnlogistic):\n", sep = ""))
      print(round(pred, digits = 3))
    }
  }

  if(nmod > 1) {

    # calculate Bayes factors for the rejection method
    bf.rej <- t(matrix(pr.rej, nmod, nmod, byrow = T)/matrix(pr.rej, nmod, nmod, byrow = F))
    # add the names of the models to the rows and columns
    colnames(bf.rej) <- postmod; rownames(bf.rej) <- postmod

    if(method != "rejection") { # when a regression was performed

      # calculate Bayes factors for the regression method
      bf.reg <- t(matrix(pred[pred != 0], nmod, nmod, byrow = T)/matrix(pred[pred != 0], nmod, nmod, byrow = F))
      # add the names of the models to the rows and columns
      colnames(bf.reg) <- postmod; rownames(bf.reg) <- postmod

    }

  } else {

    # define the output when only a single model was accepted
    bf.rej <- NA
    # when a regression was performed
    if(method != "rejection")
      bf.reg <- NA
  }

  # create the output of the function - when a regression was performed
  if(method != "rejection")
    out <- list(rejection = list(Prob = pr.rej, BayesF = bf.rej), mnlogistic = list(Prob = pred, BayesF = bf.reg))
  else
    out <- list(rejection = list(Prob = pr.rej, BayesF = bf.rej)) # when only a rejection step was performed

  # output the result of the function
  invisible(out)
}


#' Leave-one-out cross validation of model selection
#'
#' This function performs a simulation study to assess the quality of model
#' selection with ABC. This is done by performing a leave-one-out cross
#' validation via subsequent calls to the function [modelSelect()].
#'
#' One simulation is randomly selected from each model to be a validation
#' simulation, while all the other simulations are used as training simulations.
#' This random simulation is used as the target of the [modelSelect()] function
#' and posterior model probabilities are estimated.
#'
#' Please note that the actual size of the cross-validation sample is
#' \code{nval*the number of models}. This is because `nval` cross-validation
#' estimation steps are performed for each model.
#'
#' @param index is a vector of model indices. This can be a a character vector
#'   of model names, repeated as many times as there are simulations for each
#'   model. This vector will be coerced to factor and it must have the same
#'   length as \code{nrow(sumstats)} to indicate which row of the `sumstats`
#'   matrix belongs to which model.
#' @param sumstats is a vector or matrix containing the simulated summary
#'   statistics for all the models. Each row or vector entry should be a
#'   different simulation and each column of a matrix should be a different
#'   statistic. The order must be the same as the order of the models in the
#'   `index` vector.
#' @param nval a numerical value defining the the size of the cross-validation
#'   sample for each model.
#' @param tol is a numerical value, indicating the required proportion of points
#'   nearest the target values (tolerance).
#' @param warning logical, if FALSE (default) warnings produced while running
#'   this function, mainly related with accepting simulations for just one of
#'   the models, will not be displayed.
#'
#' @return a list with the following elements:
#'
#'   \item{cvsamples}{is a vector of length \code{nval*the number of models}
#'   indicating which rows of the `sumstat` input were used as validation values
#'   for each model.}
#'
#'   \item{true}{a character vector of the true models.}
#'
#'   \item{estimated}{a character vector of the estimated models.}
#'
#'   \item{model.probs}{a matrix with the estimated model probabilities. Each
#'   row of the matrix represents a different cross-validation trial.}
#'
#'   \item{models}{a character vector with the designation of the models.}
#'
#' @examples
#' # load the matrix with simulated parameter values
#' data(sumstats)
#'
#' # select a random simulation to act as target just to test the function
#' target <- sumstats[10 ,]
#'
#' # create a "fake" vector of model indices
#' # this assumes that half the simulations were from one model and the other half from other model
#' # this is not true but serves as an example of how to use this function
#' index <- c(rep("model1", nrow(sumstats)/2), rep("model2", nrow(sumstats)/2))
#'
#' # perform a leave-one-out cross validation of model selection
#' sim_modelSel(index = index, sumstats = sumstats, nval = 10, tol = 0.1)
#'
#' @export
sim_modelSel <- function(index, sumstats, nval, tol, warning = FALSE) {

  # check if the nval argument is present
  if(missing(nval))
    stop("'nval' must be supplied")

  # when dealing with a single summary statistic - in vector format - convert that vector into a matrix with one column
  if(is.vector(sumstats)) sumstats <- matrix(sumstats, ncol = 1)

  # get the name of the models
  mymodels <- levels(factor(index))

  # select random entries of the vector of model indices to act as the target
  cvsamp <- unlist(tapply(c(1:length(index)), index, sample, nval))

  # perform model selection over all the nvals
  suppressWarnings(model.sel <- lapply(1:length(cvsamp), FUN = function(i)
    modelSelect(target = sumstats[cvsamp[i], ], index = index[-cvsamp[i]], sumstats = sumstats[-cvsamp[i], ],
                tol = tol, method = "regression", warning = warning)))

  # get the prediction from each of the evaluations performed
  preds <- t(sapply(model.sel, FUN = function(i) i[["pred"]]))
  # add the appropriate names to the columns and rows of the preds matrix
  colnames(preds) <- mymodels; rownames(preds) <- index[cvsamp]

  # get the name of the most likely model for each evaluation
  likely <- unname(apply(preds, MARGIN = 1, function(xx) mymodels[which(xx == max(xx))]))

  # create the result of the function
  out <- list(cvsamples = cvsamp, true = index[cvsamp], estimated = likely, model.probs = preds, models = mymodels)
  # output the result of the function
  out
}


#' Compute error in model selection with Approximate Bayesian Computation
#'
#' This function calculates the confusion matrix and the mean misclassification
#' probabilities of models from the output of the [sim_modelSel()] function.
#'
#' It is also possible to define a `threshold` for the posterior model
#' probabilities. This threshold sets the minimum posterior probability of
#' assignment. Thus, a simulation where the posterior probability of any model
#' is below the threshold will not be assigned to a model and will instead be
#' classified as "unclear".
#'
#' @param object a list created by the [sim_modelSel()] function, containing
#'   results of a simulation study to evaluate the quality of model selection
#'   with Approximate Bayesian Computation.
#' @param threshold numeric value between 0 and 1 representing the minimum
#'   posterior probability of assignment.
#' @param print logical, if TRUE (default), then this function prints the mean
#'   models probabilities.
#'
#' @return apart from directly displaying the results if print is TRUE, the
#'   output object of this function is a list with the following elements:
#'
#'   \item{confusion.matrix}{the confusion matrix.}
#'
#'   \item{probs}{the mean model misclassification probabilities.}
#'
#'   \item{postmeans}{the mean model misclassification probabilities when each
#'   model is correctly or incorrectly estimated.}
#'
#' @examples
#' # load the matrix with simulated parameter values
#' data(sumstats)
#'
#' # select a random simulation to act as target just to test the function
#' target <- sumstats[10 ,]
#'
#' # create a "fake" vector of model indices
#' # this assumes that half the simulations were from one model and the other half from other model
#' # this is not true but serves as an example of how to use this function
#' index <- c(rep("model1", nrow(sumstats)/2), rep("model2", nrow(sumstats)/2))
#'
#' # perform a leave-one-out cross validation of model selection
#' mysim <- sim_modelSel(index = index, sumstats = sumstats, nval = 10, tol = 0.1)
#'
#' # compute the confusion matrix and the mean misclassification probabilities
#' error_modelSel(object = mysim)
#'
#' @export
error_modelSel <- function(object, threshold = NA, print = TRUE) {

  # get the true models
  true <- object$true
  # get the name of the estimated models
  estim <- object$estimated
  # obtain the estimated model probabilities
  model.probs <- object$model.probs
  # how many models were tested
  nmodels <- length(object$models)
  # how many evaluations were performed
  nval <- length(true)/nmodels
  # get the name of the models
  models <- object$models

  # if a threshold is set in the input - this is the minimum posterior probability for a model to be assigned as correct
  if(!is.na(threshold)) {

    # get the row index, from the matrix containing the model probabilities
    # where the posterior probability is not above the threshold - it must be below that value for both models
    index <- unname(rowSums(model.probs >= threshold) == 0)
    # at those sims, where the posterior probability is below the threshold for both models
    # replace the model estimated with an "unclear"
    estim[index] <- "unclear"
  }

  # compute the confusion matrix
  cm <- table(true, estim)
  # remove unnecessary names from the confusion matrix
  attributes(dimnames(cm))$names <- NULL

  # calculate the mean model misclassification probabilities
  probs <- apply(model.probs, MARGIN = 2, tapply, true, mean)

  # calculate the mean posterior probability of the models correctly assigned to the first model
  temp <- row.names(model.probs) == models[1] & estim == models[1]
  correct1 <- colMeans(model.probs[temp, , drop = FALSE])
  # correct the names of the vector - the second entry is not the other model but just sims not assigned to the first model
  names(correct1) <- c(models[1], "incorrect")

  # calculate the mean posterior probability of the models correctly assigned to the second model
  temp <- row.names(model.probs) == models[2] & estim == models[2]
  correct2 <- colMeans(model.probs[temp, , drop = FALSE])
  # correct the names of the vector - the first entry is not the other model but just sims not assigned to the second model
  names(correct2) <- c("incorrect", models[2])

  # calculate the mean posterior probability of the first models that were incorrectly assigned to the second model
  temp <- row.names(model.probs) == models[1] & estim == models[2]
  incorrect1 <- colMeans(model.probs[temp, , drop = FALSE])

  # calculate the mean posterior probability of the second models that were incorrectly assigned to the first model
  temp <- row.names(model.probs) == models[2] & estim == models[1]
  incorrect2 <- colMeans(model.probs[temp, , drop = FALSE])

  # combine all these different posterior probabilities into a single matrix
  postmeans <- rbind(correct1, correct2, incorrect1, incorrect2)

  # create names for the rows of that matrix
  names <- c(paste("true", models[1], sep = " "), paste("true", models[2], sep = " "), paste("wrong", models[1], sep = " "),
             paste("wrong", models[2], sep = " "))

  # add those names to the matrix
  rownames(postmeans) <- names

  if(print) {
    cat("Confusion matrix based on ", nval, " samples for each model\n\n", sep = "")
    print(cm); cat("\n")
    cat(paste("Mean model posterior probabilities (mnlogistic)\n\n", sep = ""))
    print(round(probs, digits = 3)); cat("\n")
    cat(paste("Posterior probabilities of correctly assigned ", models[1], " models\n\n", sep = ""))
    print(round(correct1, digits = 3)); cat("\n")
    cat(paste("Posterior probabilities of correctly assigned ", models[2], " models\n\n", sep = ""))
    print(round(correct2, digits = 3)); cat("\n")
    cat(paste("Posterior probabilities when ", models[1], " is estimated as ", models[2], "\n\n", sep = ""))
    print(round(incorrect1, digits = 3)); cat("\n")
    cat(paste("Posterior probabilities when ", models[2], " is estimated as ", models[1], "\n\n", sep = ""))
    print(round(incorrect2, digits = 3)); cat("\n")
  }

  # create the output of the function
  out <- list(confusion.matrix = cm, probs = probs, postmeans = postmeans)
  # output the final result of this function
  invisible(out)
}


#' Plot model misclassification
#'
#' Displays a barplot of the confusion matrix obtained with a leave-one-out
#' cross validation for model selection.
#'
#' The barplot shows the proportion of validation simulations classified to each
#' of the models. This function can produce either a colour or a grey scale
#' barplot. If the classification of models is perfect, meaning that the model
#' probability of each model is one for the correct model, then each bar will
#' have a single colour representing its corresponding model.
#'
#' @param object a list created by the [error_modelSel()] function, containing
#'   the results of a leave-one-out cross validation for model selection.
#' @param color logical, if TRUE (default) then a colour version of the barplot
#'   will be produced, if FALSE then a grey scale version will be produced.
#'
#' @return a barplot of the proportion of simulations classified to any of the
#'   models. In other words, a barplot of the confusion matrix.
#'
#' @importFrom rlang .data
#'
#' @examples
#' # load the matrix with simulated parameter values
#' data(sumstats)
#'
#' # select a random simulation to act as target just to test the function
#' target <- sumstats[10 ,]
#'
#' # create a "fake" vector of model indices
#' # this assumes that half the simulations were from one model and the other half from other model
#' # this is not true but serves as an example of how to use this function
#' index <- c(rep("model1", nrow(sumstats)/2), rep("model2", nrow(sumstats)/2))
#'
#' # perform a leave-one-out cross validation of model selection
#' mysim <- sim_modelSel(index = index, sumstats = sumstats, nval = 10, tol = 0.1)
#'
#' # compute the confusion matrix and the mean misclassification probabilities
#' myerror <- error_modelSel(object = mysim, print = FALSE)
#'
#' # barplot of model misclassification
#' plot_msel(object = myerror)
#'
#' @export
plot_msel <- function(object, color = TRUE) {

  # get the confusion matrix from the input
  cm <- object[["confusion.matrix"]]
  # check how many different models were evaluated
  nmodels <- nrow(cm)
  # create a temporary list
  temp <- list()

  # do a loop over all the models evaluated
  for(i in 1:nmodels) {

    # create a list where each entry is a data frame with the information for each model
    # this data frame is appropriate for ggplot
    temp[[i]] <- data.frame(model = rownames(cm)[i], estimated = colnames(cm), value = cm[i, ], row.names = NULL)
  }

  # combine all the list entries into a final data frame
  final <- do.call(rbind, temp)

  if(color == TRUE)
    # create a colour-blind-friendly palette
    palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  else
    # create a grey-scale palette
    palette <- c("#999999", "#666666", "#000000")

  # plot the model selection values for each model
  ggplot2::ggplot(final, ggplot2::aes(fill = .data$estimated, y = .data$value, x = .data$model)) +
    ggplot2::geom_bar(position = "stack", stat = "identity") +
    # change the colour of the bars
    ggplot2::scale_fill_manual(values = palette) +
    # modify the visual aspect of the plot
    ggplot2::theme_light(base_size = 15, base_family = "", base_line_size = 0.2, base_rect_size = 1) +
    # define a series of visual settings - including size of legend text and ticks on the axis
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 20),axis.text.y = ggplot2::element_text(size = 20),
                   axis.title.x = ggplot2::element_text(size = 15, margin = ggplot2::margin(t = 8)),
                   axis.title.y = ggplot2::element_text(size = 15, margin = ggplot2::margin(r = 8)),
                   plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14)) +
    # remove some of the grid lines and remove the title from the legends
    ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank(), panel.grid.minor.y = ggplot2::element_blank(),
                   legend.title = ggplot2::element_blank())
}
