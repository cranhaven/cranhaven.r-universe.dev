#' Random number generation for bidimensional parsimonious mixtures of MSEN or MTIN distributions
#'
#' @param n An integer specifying the number of data points to be simulated.
#' @param k An integer indicating the number of groups in the data.
#' @param Pi A vector of length \code{k} representing the probability of belonging to the \code{k} groups for each data point.
#' @param mu A matrix of means with 2 rows and \code{k} columns.
#' @param cov.model A character indicating the parsimonious structure of the scale matrices. Possible values are:
#'     "EII", "VII", "EEI", "VEI", "EVI", "VVI", "EEE", "VEE", "EVE", "EEV", "VVE", "VEV", "EVV" or "VVV".
#' @param lambda A numeric vector of length \code{k}, related to the scale matrices (see Punzo et al., 2016),
#'      which determines the volumes of the mixture components. Each element must be greater than 0.
#'      Required for all the parsimonious structures.
#' @param delta A numeric vector of length \code{k}, related to the scale matrices (see Punzo et al., 2016),
#'      which determines the shapes of the mixture components. Each element must be between 0 and 1.
#'      Required for all the parsimonious structures, with the exclusion of "EII" and "VII".
#' @param gamma A numeric vector of length \code{k}, related to the scale matrices (see Punzo et al., 2016),
#'      which determines the orientation of the mixture components. Each element represents an angle expressed in radian unit.
#'      Required for the "EEE", "VEE", "EVE", "EEV", "VVE", "VEV", "EVV" or "VVV" parsimonious structures.
#' @param theta A vector of length \code{k} representing the tailedness parameters.
#' @param density A character indicating the density of the mixture components. Possible values are: "MSEN" or "MTIN".
#'
#' @return A list with the following elements:
#' \item{X}{A data matrix with \code{n} rows and 2 columns.}
#' \item{Sigma}{An array of dimension 2 x 2 x \code{k} for the generated scale matrices.}
#' \item{Size}{The size of each generated group.}
#' @export
#'
#' @references
#' Punzo A., Browne R. and McNicholas P.D. (2016). Hypothesis Testing for Mixture Model Selection.
#' *Journal of Statistical Computation and Simulation*, **86**(14), 2797-2818.
#'
#' @examples
#' n <- 50
#' k <- 2
#' Pi <- c(0.5, 0.5)
#' mu <- matrix(c(0, 0, 4, 5), 2, 2)
#' cov.model <- "EEE"
#' lambda <- c(0.5, 0.5)
#' delta <- c(0.7, 0.7)
#' gamma <- c(2.62, 2.62)
#' theta <- c(0.1, 0.1)
#' density <- "MSEN"
#' data <- rMixt(n, k, Pi, mu, cov.model, lambda, delta, gamma, theta, density)
rMixt <- function(n, k, Pi, mu, cov.model, lambda, delta, gamma, theta, density) {
  d <- 2
  Sigma <- array(0, dim = c(d, d, k))
  dat <- list()

  if (cov.model == "EII") {
    for (j in 1:k) {
      Sigma[, , j] <- lambda[1] * diag(d)
    }
  }

  if (cov.model == "VII") {
    for (j in 1:k) {
      Sigma[, , j] <- lambda[j] * diag(d)
    }
  }

  if (cov.model == "EEI") {
    for (j in 1:k) {
      Sigma[, , j] <- lambda[1] * diag(c(1 / delta[1], delta[1]))
    }
  }

  if (cov.model == "VEI") {
    for (j in 1:k) {
      Sigma[, , j] <- lambda[j] * diag(c(1 / delta[1], delta[1]))
    }
  }

  if (cov.model == "EVI") {
    for (j in 1:k) {
      Sigma[, , j] <- lambda[1] * diag(c(1 / delta[j], delta[j]))
    }
  }

  if (cov.model == "VVI") {
    for (j in 1:k) {
      Sigma[, , j] <- lambda[j] * diag(c(1 / delta[j], delta[j]))
    }
  }

  if (cov.model == "EEE") {
    for (j in 1:k) {
      Sigma[, , j] <- lambda[1] * rotmat(gamma[1]) %*% diag(c(1 / delta[1], delta[1])) %*% t(rotmat(gamma[1]))
    }
  }

  if (cov.model == "VEE") {
    for (j in 1:k) {
      Sigma[, , j] <- lambda[j] * rotmat(gamma[1]) %*% diag(c(1 / delta[1], delta[1])) %*% t(rotmat(gamma[1]))
    }
  }

  if (cov.model == "EVE") {
    for (j in 1:k) {
      Sigma[, , j] <- lambda[1] * rotmat(gamma[1]) %*% diag(c(1 / delta[j], delta[j])) %*% t(rotmat(gamma[1]))
    }
  }

  if (cov.model == "EEV") {
    for (j in 1:k) {
      Sigma[, , j] <- lambda[1] * rotmat(gamma[j]) %*% diag(c(1 / delta[1], delta[1])) %*% t(rotmat(gamma[j]))
    }
  }

  if (cov.model == "VVE") {
    for (j in 1:k) {
      Sigma[, , j] <- lambda[j] * rotmat(gamma[1]) %*% diag(c(1 / delta[j], delta[j])) %*% t(rotmat(gamma[1]))
    }
  }

  if (cov.model == "VEV") {
    for (j in 1:k) {
      Sigma[, , j] <- lambda[j] * rotmat(gamma[j]) %*% diag(c(1 / delta[1], delta[1])) %*% t(rotmat(gamma[j]))
    }
  }

  if (cov.model == "EVV") {
    for (j in 1:k) {
      Sigma[, , j] <- lambda[1] * rotmat(gamma[j]) %*% diag(c(1 / delta[j], delta[j])) %*% t(rotmat(gamma[j]))
    }
  }

  if (cov.model == "VVV") {
    for (j in 1:k) {
      Sigma[, , j] <- lambda[j] * rotmat(gamma[j]) %*% diag(c(1 / delta[j], delta[j])) %*% t(rotmat(gamma[j]))
    }
  }

  temp <- rowSums(stats::rmultinom(n, 1, prob = Pi))

  if (density == "MSEN") {
    for (j in 1:k) {
      dat[[j]] <- rmsen(temp[j], mu[, j], Sigma[, , j], theta[j])$X
    }
  }

  if (density == "MTIN") {
    for (j in 1:k) {
      dat[[j]] <- rmtin(temp[j], mu[, j], Sigma[, , j], theta[j])$X
    }
  }

  X <- matrix(rlist::list.rbind(dat), n, d)

  return(list(X = X, Sigma = Sigma, Size = temp))
}
