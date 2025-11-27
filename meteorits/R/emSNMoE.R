#' emSNMoE implements the ECM algorithm to fit a Skew-Normal Mixture of Experts
#' (SNMoE).
#'
#' emSNMoE implements the maximum-likelihood parameter estimation of a
#' Skew-Normal Mixture of Experts (SNMoE) model by the Expectation Conditional
#' Maximization (ECM) algorithm.
#'
#' @details emSNMoE function implements the ECM algorithm for the SNMoE model.
#'   This function starts with an initialization of the parameters done by the
#'   method `initParam` of the class [ParamSNMoE][ParamSNMoE], then it
#'   alternates between the E-Step (method of the class [StatSNMoE][StatSNMoE])
#'   and the M-Step (method of the class [ParamSNMoE][ParamSNMoE]) until
#'   convergence (until the relative variation of log-likelihood between two
#'   steps of the ECM algorithm is less than the `threshold` parameter).
#'
#' @param X Numeric vector of length \emph{n} representing the covariates/inputs
#'   \eqn{x_{1},\dots,x_{n}}.
#' @param Y Numeric vector of length \emph{n} representing the observed
#'   response/output \eqn{y_{1},\dots,y_{n}}.
#' @param K The number of experts.
#' @param p Optional. The order of the polynomial regression for the experts.
#' @param q Optional. The order of the logistic regression for the gating
#'   network.
#' @param n_tries Optional. Number of runs of the ECM algorithm. The solution
#'   providing the highest log-likelihood will be returned.
#' @param max_iter Optional. The maximum number of iterations for the ECM
#'   algorithm.
#' @param threshold Optional. A numeric value specifying the threshold for the
#'   relative difference of log-likelihood between two steps of the ECM as
#'   stopping criteria.
#' @param verbose Optional. A logical value indicating whether or not values of
#'   the log-likelihood should be printed during ECM iterations.
#' @param verbose_IRLS Optional. A logical value indicating whether or not
#'   values of the criterion optimized by IRLS should be printed at each step of
#'   the ECM algorithm.
#' @return ECM returns an object of class [ModelSNMoE][ModelSNMoE].
#' @seealso [ModelSNMoE], [ParamSNMoE], [StatSNMoE]
#' @export
#'
#' @examples
#' data(tempanomalies)
#' x <- tempanomalies$Year
#' y <- tempanomalies$AnnualAnomaly
#'
#' snmoe <- emSNMoE(X = x, Y = y, K = 2, p = 1, verbose = TRUE)
#'
#' snmoe$summary()
#'
#' snmoe$plot()
emSNMoE <- function(X, Y, K, p = 3, q = 1, n_tries = 1, max_iter = 1500, threshold = 1e-6, verbose = FALSE, verbose_IRLS = FALSE) {

  top <- 0
  try_EM <- 0
  best_loglik <- -Inf

  while (try_EM < n_tries) {

    try_EM <- try_EM + 1

    if (n_tries > 1 && verbose) {
      message("EM try number: ", try_EM, "\n")
    }

    # Initialization
    param <- ParamSNMoE(X = X, Y = Y, K = K, p = p, q = q)
    param$initParam(segmental = TRUE)

    iter <- 0
    converge <- FALSE
    prev_loglik <- -Inf

    stat <- StatSNMoE(paramSNMoE = param)

    while (!converge && (iter <= max_iter)) {
      stat$EStep(param)

      reg_irls <- param$MStep(stat, verbose_IRLS)

      stat$computeLikelihood(reg_irls)

      iter <- iter + 1
      if (verbose) {
        message("EM - SNMoE: Iteration: ", iter, " | log-likelihood: "  , stat$loglik)
      }

      if (prev_loglik - stat$loglik > 1e-5) {
        if (verbose) {
          warning("EM log-likelihood is decreasing from ", prev_loglik, "to ", stat$loglik, "!")
        }
        top <- top + 1
        if (top > 20)
          break
      }

      # Test of convergence
      converge <- abs((stat$loglik - prev_loglik) / prev_loglik) <= threshold

      if (is.na(converge)) {
        converge <- FALSE
      } # Basically for the first iteration when prev_loglik is Inf

      prev_loglik <- stat$loglik
      stat$stored_loglik <- c(stat$stored_loglik, stat$loglik)
    }# End of an EM loop

    if (stat$loglik > best_loglik) {
      statSolution <- stat$copy()
      paramSolution <- param$copy()

      best_loglik <- stat$loglik
    }

    if (n_tries > 1 && verbose) {
      message("Max value of the log-likelihood: ", stat$loglik, "\n\n")
    }
  }

  # Computation of z_ik the hard partition of the data and the class labels klas
  statSolution$MAP()

  if (n_tries > 1 && verbose) {
    message("Max value of the log-likelihood: ", statSolution$loglik, "\n")
  }

  statSolution$computeStats(paramSolution)

  return(ModelSNMoE(param = paramSolution, stat = statSolution))
}
