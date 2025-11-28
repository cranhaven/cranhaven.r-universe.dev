# Deviance functions for Tweedie family.

#' Deviance function for the Tweedie family.
#'
#' Compute the deviance for the Tweedie family case.
#'
#' @param y a vector containing the observed values.
#' @param mu a vector containing the fitted values.
#' @param w an optional vector of weights.
#' @param tweedieVal a numeric representing the Tweedie Power. It has to be a positive number outside of the interval ]0,1[.
#'
#' @return A vector of individual deviance contribution.
#'
#' @details
#' This function computes the Tweedie related deviance. The latter is defined as:
#'
#' \deqn{d(y, mu, w) = w (y-mu)^2, if tweedieVal = 0;}
#' \deqn{d(y, mu, w) = 2 w (y log(y/mu) + mu - y), if tweedieVal = 1;}
#' \deqn{d(y, mu, w) = 2 w (log(mu/y) + y/mu - 1), if tweedieVal = 2;}
#' \deqn{d(y, mu, w) = 2 w (max(y,0)^(2-p)/((1-p)(2-p)) - y mu^(1-p)/(1-p) + mu^(2-p)/(2-p)), else.}
#'
#'
#' @author Gireg Willame \email{gireg.willame@@gmail.com}
#'
#' \emph{This package is inspired by the \code{gbm3} package. For more details, see \url{https://github.com/gbm-developers/gbm3/}}.
#'
#' @seealso \code{\link{BT}}, \code{\link{BT_call}}.
#'
#' @references M. Denuit, D. Hainaut and J. Trufin (2019). \strong{Effective Statistical Learning Methods for Actuaries |: GLMs and Extensions}, \emph{Springer Actuarial}.
#'
#' M. Denuit, D. Hainaut and J. Trufin (2019). \strong{Effective Statistical Learning Methods for Actuaries ||: Tree-Based Methods and Extensions}, \emph{Springer Actuarial}.
#'
#' M. Denuit, D. Hainaut and J. Trufin (2019). \strong{Effective Statistical Learning Methods for Actuaries |||: Neural Networks and Extensions}, \emph{Springer Actuarial}.
#'
#' M. Denuit, D. Hainaut and J. Trufin (2022). \strong{Response versus gradient boosting trees, GLMs and neural networks under Tweedie loss and log-link}.
#' Accepted for publication in \emph{Scandinavian Actuarial Journal}.
#'
#' M. Denuit, J. Huyghe and J. Trufin (2022). \strong{Boosting cost-complexity pruned trees on Tweedie responses: The ABT machine for insurance ratemaking}.
#' Paper submitted for publication.
#'
#' M. Denuit, J. Trufin and T. Verdebout (2022). \strong{Boosting on the responses with Tweedie loss functions}. Paper submitted for publication.
#'
#' @export
#'
BT_devTweedie <- function(y, mu, tweedieVal, w = NULL) {
  .check_tweedie_power(tweedieVal)
  if (any(is.logical(y) |
          is.character(y) | (y != as.double(y)) | is.na(y))) {
    stop("Responses must be doubles")
  }
  if (any(is.logical(mu) |
          is.character(mu) | (mu != as.double(mu)) | is.na(mu))) {
    stop("Predictions must be doubles")
  }
  if (is.null(w)) {
    w <- rep(1, length(y))
  }

  if (any(is.logical(w) |
          is.character(w) | (w != as.double(w)) | is.na(w) | (w < 0))) {
    stop("Weights must be positive doubles")
  }
  if (any(length(y) != length(mu) | length(y) != length(w))) {
    stop("Responses, predictions and weights should have the same length")
  }

  if (tweedieVal == 0) {
    # Gaussian case.
    dev <- w * (y - mu) ** 2
  } else if (tweedieVal == 1) {
    # Poisson case.
    r <- mu
    p <- which(y > 0)
    r[p] <- (y * log(y / mu) - (y - mu))[p]
    dev <- 2 * r * w
  } else if (tweedieVal == 2) {
    # Gamma case.
    dev <-
      2 * w * (-log(ifelse(y == 0, 1, y / mu)) + (y / mu) - 1) # Support Gamma : ]0; +Inf[
  } else{
    dev <-
      2 * w * (((max(y, 0) ^ (2 - tweedieVal)) / ((1 - tweedieVal) * (2 - tweedieVal))) - (y *
                                                                                             (mu ^ (1 - tweedieVal)) / (1 - tweedieVal)) + ((mu ^ (2 - tweedieVal)) /
                                                                                                                                              (2 - tweedieVal)))
  }
  return(dev)
}
