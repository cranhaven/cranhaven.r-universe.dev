# sNorm
#
# Generates normal data for source nodes (nodes with no parents).
#
# @param N The number of observations to simulate.
#
# @param b0 The mean of the normal distribution.
#
# @param s The standard deviation of the normal distribution.
#
# @return A vector containing the simulated data.
#
#' @importFrom stats rnorm
#'
sNorm <- function (N,
                   b0,
                   s) {

  return (rnorm(n = N,
                mean = b0,
                sd = s))

}

# sBinom
#
# Generates binomial data for source nodes (nodes with no parents).
#
# @param N The number of observations to simulate.
#
# @param p The probability of success. For rbinom the value 1 represents a
# success.
#
# @return A vector containing the simulated data.
#
#' @importFrom stats rbinom
#'
sBinom <- function (N,
                    p) {

  return (rbinom(n = N,
                 size = 1,
                 prob = p))

}

# sMulti
#
# Generates multinomial data for source nodes (nodes with no parents).
#
# @param N The number of observations to simulate.
#
# @param q The frequency of the reference allele.
#
# @return A vector containing the simulated data.
#
#' @importFrom stats rnorm
#'
sMulti <- function (N,
                    q) {

  return (sample(x = 0:2,
                 size = N,
                 replace = TRUE,
                 prob = c((1 - q)^2,
                          2 * q * (1 - q),
                          q^2)))

}

# cNorm
#
# Generates normal data for child nodes (nodes one or more parents).
#
# @param N The number of observations to simulate.
#
# @param parentData A list containing the data of the parents nodes.
#
# @param b0 The slope of the linear model
# b0 + b1[[1]] * parentData[[1]] + b1[[2]] * parentData[[2]] + ...
#
# @param b1 A vector containing the regression coefficients of the parent nodes.
#
# @return A vector containing the simulated data.
#
#' @importFrom stats as.formula rnorm
#'
cNorm <- function (N,
                   parentData,
                   b0,
                   b1,
                   s) {

  # Check if the parent vector and the signal strength vector have the same
  # number of elements.
  if (length(parentData) != length(b1)) {

    stop ('parentData and b1 must have the same number of elements')

  }

  # Determine the number of parents.
  mParents <- length(parentData)

  # Create a vector that will hold a linear model as a character string.
  lmFormula <- vector(length = mParents + 1)

  # The first element is the slope.
  lmFormula[[1]] <- '~ b0'

  # Loop through the parents for the current node.
  for (e in 1:mParents) {

    # Fill in the remaining elements with the regression coefficient and the
    # data for the corresponding parent node.
    lmFormula[[e + 1]] <- paste0('b1[[',
                                 e,
                                 ']] * parentData[[',
                                 e,
                                 ']]')

  }

  # Create a formula object and evaluate it. The subset, [[2]], extracts just
  # the right hand side of the formula.
  means <- eval(as.formula(paste(lmFormula,
                                 collapse = ' + '))[[2]])

  # Simulate data from a normal distribution and return it.
  return (rnorm(n = N,
                mean = means,
                sd = s))

}

# cBinom
#
# Generates binomial data for child nodes (nodes one or more parents).
#
# @param N The number of observations to simulate.
#
# @param parentData A list containing the data of the parents nodes.
#
# @param b0 The slope of the linear model
# b0 + b1[[1]] * parentData[[1]] + b1[[2]] * parentData[[2]] + ...
#
# @param b1 A vector containing the regression coefficients of the parent nodes.
#
# @param p The probability of success (a number between 0 and 1).
#
# @return A vector containing the simulated data.
#
#' @importFrom stats as.formula rbinom
#'
cBinom <- function (N,
                    parentData,
                    b0,
                    b1) {

  # Check if the parent vector and the signal strength vector have the same
  # number of elements.
  if (length(parentData) != length(b1)) {

    stop ('parentData and b1 must have the same number of elements')

  }

  # Determine the number of parents.
  mParents <- length(parentData)

  # Create a vector that will hold a linear model as a character string.
  lmFormula <- vector(length = mParents + 1)

  # The first element is the slope.
  lmFormula[[1]] <- '~ b0'

  # Loop through the parents for the current node.
  for (e in 1:mParents) {

    # Fill in the remaining elements with the regression coefficient and the
    # data for the corresponding parent node.
    lmFormula[[e + 1]] <- paste0('b1[[',
                                 e,
                                 ']] * parentData[[',
                                 e,
                                 ']]')

  }

  # Create a formula object and evaluate it. The subset, [[2]], extracts just
  # the right hand side of the formula.
  linearModel <- eval(as.formula(paste(lmFormula,
                                       collapse = ' + '))[[2]])

  # Use the inverse logit to calculate the probability of success for each
  # observation.
  probs <- 1 / (1 + exp(-linearModel))

  # Simulate data from a binomial distribution and return it.
  return (rbinom(n = N,
                 size = 1,
                 prob = probs))

}

# cMulti
#
# Generates multinomial data for child nodes (nodes one or more parents).
#
# @param N The number of observations to simulate.
#
# @param q The frequency of the reference allele.
#
# @param parentData A list containing the data of the parents nodes.
#
# @param b0 The slope of the linear model
# b0 + b1[[1]] * parentData[[1]] + b1[[2]] * parentData[[2]] + ...
#
# @param b1 A vector containing the regression coefficients of the parent nodes.
#
# @param q The frequency of the reference allele.
#
# @return A vector containing the simulated data.
#
#' @importFrom stats as.formula qnorm
#'
cMulti <- function (N,
                    parentData,
                    b0,
                    b1,
                    q) {

  # Check if the parent vector and the signal strength vector have the same
  # number of elements.
  if (length(parentData) != length(b1)) {

    stop ('parentData and b1 must have the same number of elements')

  }

  # Determine the number of parents.
  mParents <- length(parentData)

  # Create a vector that will hold a linear model as a character string.
  lmFormula <- vector(length = mParents + 1)

  # The first element is the slope.
  lmFormula[[1]] <- '~ b0'

  # Loop through the parents for the current node.
  for (e in 1:mParents) {

    # Fill in the remaining elements with the regression coefficient and the
    # data for the corresponding parent node.
    lmFormula[[e + 1]] <- paste0('b1[[',
                                 e,
                                 ']] * parentData[[',
                                 e,
                                 ']]')

  }

  # Create a formula object and evaluate it. The subset, [[2]], extracts just
  # the right hand side of the formula.
  means <- eval(as.formula(paste(lmFormula,
                                 collapse = ' + '))[[2]])

  # Simulate data from a normal distribution. This will be divided later to
  # produce a vector of 0s, 1s, and 2s that follow a multinomial distribution
  # with new frequencies from having one or more parents.
  normalRV <- rnorm(n = N,
                    mean = means,
                    sd = 1)

  # Create the lower cutoff for q^2 in the standard normal distribution.
  lowerCutoff <- qnorm(q^2, 0, 1)

  # Create the upper cutoff for (1 - q)^2 in the standard normal distribution.
  upperCutoff <- qnorm((1 - q)^2, 0, 1, lower.tail = FALSE)

  # Initialize the U vector to full length.
  U <- rep(NA, length = N)

  # Change the normal values to a zero if the fall below the lower cutoff.
  U[normalRV < lowerCutoff] <- 0

  # Change the normal values to a two if they are above the upper cutoff.
  U[normalRV > upperCutoff] <- 2

  # Change the remaining normal values to a one.
  U[is.na(U)] <- 1

  # Return the multinomial rv.
  return (U)

}
