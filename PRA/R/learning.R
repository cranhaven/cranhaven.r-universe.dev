#' Posterior Risk Probability.
#'
#' This function calculates the posterior probability of a risk event 'R' occurring
#' based on observations of multiple root causes and their associated conditional probabilities.
#'
#' @srrstats {G1.0} *Software lists primary reference from published academic literature.*
#' @srrstats {G1.1} *Software is the first implementation within **R** of the algorithm which has previously been implemented in other languages or contexts.*
#' @srrstats {G1.4} *Software uses [`roxygen2`](https://roxygen2.r-lib.org/) to document all functions.*
#' @srrstats {G2.0} *Implements assertions on lengths of inputs - all vectors must have same length.*
#' @srrstats {G2.0a} *Parameter documentation explicitly states expected input structure.*
#' @srrstats {G2.1} *Implements assertions on types of inputs - probability values validated between 0 and 1.*
#' @srrstats {G2.1a} *Parameter documentation explicitly states data types expected.*
#' @srrstats {G2.13} *Implements checks for NA values in observed_causes - NA is permitted to indicate unobserved causes.*
#' @srrstats {G2.14b} *Ignores NA values in observed_causes with appropriate handling for unobserved causes.*
#' @srrstats {G2.15} *Implements checks for NaN values via is.nan() prior to processing.*
#' @srrstats {G2.16} *Implements checks for Inf/-Inf values via is.infinite() prior to processing.*
#' @srrstats {G5.2a} *Each error message produced by stop() is unique.*
#'
#' @param cause_probs A vector of prior probabilities for each root cause 'C_i'.
#' @param risks_given_causes A vector of conditional probabilities of the risk event 'R' given each cause 'C_i'.
#' @param risks_given_not_causes A vector of conditional probabilities of the risk event 'R' given not each cause 'C_i'.
#' @param observed_causes A vector of observed values for each cause 'C_i' (1 if observed, 0 if not observed, NA if unobserved).
#' @return A numeric value for the posterior probability of the risk event given the observed causes.
#' @references
#' Damnjanovic, Ivan, and Kenneth Reinschmidt. Data analytics for engineering and
#' construction project risk management. No. 172534. Cham, Switzerland: Springer, 2020.
#' @examples
#' cause_probs <- c(0.3, 0.2)
#' risks_given_causes <- c(0.8, 0.6)
#' risks_given_not_causes <- c(0.2, 0.4)
#' observed_causes <- c(1, NA)
#' risk_post_prob <- risk_post_prob(
#'   cause_probs, risks_given_causes,
#'   risks_given_not_causes, observed_causes
#' )
#' print(risk_post_prob)
#' @export
risk_post_prob <- function(cause_probs, risks_given_causes, risks_given_not_causes, observed_causes) {
  # Validate inputs
  if (any(is.nan(cause_probs)) || any(is.nan(risks_given_causes)) || any(is.nan(risks_given_not_causes))) {
    stop("cause_probs, risks_given_causes, and risks_given_not_causes must not contain NaN values.")
  }
  if (anyNA(cause_probs) || anyNA(risks_given_causes) || anyNA(risks_given_not_causes)) {
    stop("cause_probs, risks_given_causes, and risks_given_not_causes must not contain NA values.")
  }
  if (any(is.infinite(cause_probs)) || any(is.infinite(risks_given_causes)) || any(is.infinite(risks_given_not_causes))) {
    stop("cause_probs, risks_given_causes, and risks_given_not_causes must not contain infinite values.")
  }
  if (any(is.infinite(observed_causes[!is.na(observed_causes)]))) {
    stop("observed_causes must not contain infinite values.")
  }
  if (length(cause_probs) != length(risks_given_causes) ||
    length(cause_probs) != length(risks_given_not_causes) ||
    length(cause_probs) != length(observed_causes)) {
    stop("All input vectors must have the same length.")
  }
  if (any(cause_probs < 0 | cause_probs > 1)) {
    stop("All values in cause_probs must be between 0 and 1.")
  }
  if (any(risks_given_causes < 0 | risks_given_causes > 1)) {
    stop("All values in risks_given_causes must be between 0 and 1.")
  }
  if (any(risks_given_not_causes < 0 | risks_given_not_causes > 1)) {
    stop("All values in risks_given_not_causes must be between 0 and 1.")
  }
  if (!all(is.na(observed_causes) | observed_causes %in% c(0, 1))) {
    stop("All values in observed_causes must be 0, 1, or NA.")
  }

  # Initialize posterior probability of the risk event
  numerator <- 1
  denominator <- 1

  for (i in seq_along(cause_probs)) {
    if (!is.na(observed_causes[i])) {
      if (observed_causes[i] == 1) {
        numerator <- numerator * risks_given_causes[i] * cause_probs[i]
        denominator <- denominator * (risks_given_causes[i] * cause_probs[i] +
          risks_given_not_causes[i] * (1 - cause_probs[i]))
      } else {
        numerator <- numerator * risks_given_not_causes[i] * (1 - cause_probs[i])
        denominator <- denominator * (risks_given_causes[i] * cause_probs[i] +
          risks_given_not_causes[i] * (1 - cause_probs[i]))
      }
    }
  }

  # Return the normalized posterior probability
  return(numerator / denominator)
}

#' Posterior Cost Probability Density.
#'
#' This function generates random samples from the posterior distribution of the cost 'A'
#' given observations of multiple risk events 'R_i'. Each risk event has its own mean
#' and standard deviation for the cost distribution. The function also accounts for a baseline cost
#' when no risk event occurs.
#'
#' @srrstats {G1.0} *Software lists primary reference from published academic literature.*
#' @srrstats {G1.1} *Software is the first implementation within **R** of the algorithm which has previously been implemented in other languages or contexts.*
#' @srrstats {G1.4} *Software uses [`roxygen2`](https://roxygen2.r-lib.org/) to document all functions.*
#' @srrstats {G2.0} *Implements assertions on lengths of inputs - all parameter vectors must have same length.*
#' @srrstats {G2.0a} *Parameter documentation explicitly states expected input structure.*
#' @srrstats {G2.1} *Implements assertions on types of inputs via is.numeric() checks.*
#' @srrstats {G2.1a} *Parameter documentation explicitly states data types expected.*
#' @srrstats {G2.2} *Prohibits multivariate input for num_sims which must be a positive integer.*
#' @srrstats {G2.13} *Implements checks for NA values in observed_risks - NA permitted to indicate unobserved risks.*
#' @srrstats {G2.14b} *Ignores NA values in observed_risks with appropriate handling for unobserved risks.*
#' @srrstats {G2.15} *Implements checks for NaN values via is.nan() prior to processing.*
#' @srrstats {G2.16} *Implements checks for Inf/-Inf values via is.infinite() prior to processing.*
#' @srrstats {G5.2a} *Each error message produced by stop() is unique.*
#'
#' @param num_sims Number of random samples to draw from the posterior distribution.
#' @param observed_risks A vector of observed values for each risk event 'R_i' (1 if observed, 0 if not observed, NA if unobserved).
#' @param means_given_risks A vector of means of the normal distribution for cost 'A' given each risk event 'R_i'.
#' @param sds_given_risks A vector of standard deviations of the normal distribution for cost 'A' given each risk event 'R_i'.
#' @param base_cost The baseline cost given no risk event occurs.
#' @return A numeric vector of random samples from the posterior distribution of costs.
#' @references
#' Damnjanovic, Ivan, and Kenneth Reinschmidt. Data analytics for engineering and
#' construction project risk management. No. 172534. Cham, Switzerland: Springer, 2020.
#' @examples
#' # Example with three risk events
#' num_sims <- 1000
#' observed_risks <- c(1, NA, 1)
#' means_given_risks <- c(10000, 15000, 5000)
#' sds_given_risks <- c(2000, 1000, 1000)
#' base_cost <- 2000
#' posterior_samples <- cost_post_pdf(
#'   num_sims = num_sims,
#'   observed_risks = observed_risks,
#'   means_given_risks = means_given_risks,
#'   sds_given_risks = sds_given_risks,
#'   base_cost = base_cost
#' )
#' hist(posterior_samples, breaks = 30, col = "skyblue", main = "Posterior Cost PDF", xlab = "Cost")
#' @importFrom stats rnorm
#' @export
cost_post_pdf <- function(num_sims, observed_risks, means_given_risks, sds_given_risks, base_cost = 0) {
  # Validate inputs
  if (num_sims <= 0 || !is.numeric(num_sims)) stop("num_sims must be a positive integer.")
  if (any(is.nan(means_given_risks)) || any(is.nan(sds_given_risks))) {
    stop("means_given_risks and sds_given_risks must not contain NaN values.")
  }
  if (anyNA(means_given_risks) || anyNA(sds_given_risks)) {
    stop("means_given_risks and sds_given_risks must not contain NA values.")
  }
  if (any(is.infinite(means_given_risks)) || any(is.infinite(sds_given_risks))) {
    stop("means_given_risks and sds_given_risks must not contain infinite values.")
  }
  if (any(is.infinite(observed_risks[!is.na(observed_risks)]))) {
    stop("observed_risks must not contain infinite values.")
  }
  if (is.nan(base_cost)) stop("base_cost must not be NaN.")
  if (is.na(base_cost)) stop("base_cost must not be NA.")
  if (is.infinite(base_cost)) stop("base_cost must not be infinite.")
  if (!all(is.na(observed_risks) | observed_risks %in% c(0, 1))) stop("All values in observed_risks must be 0, 1, or NA.")
  if (length(observed_risks) != length(means_given_risks) || length(observed_risks) != length(sds_given_risks)) {
    stop("observed_risks, means_given_risks, and sds_given_risks must have the same length.")
  }
  if (any(sds_given_risks < 0)) stop("Standard deviations must be non-negative.")

  # Number of risk events
  num_risks <- length(observed_risks)

  # Initialize cost samples with base cost
  samples <- rep(base_cost, num_sims)

  # Iterate over each risk event
  for (i in seq_len(num_risks)) {
    if (!is.na(observed_risks[i]) && observed_risks[i] == 1) {
      # Add cost samples for the observed risk event
      samples <- samples + rnorm(num_sims, mean = means_given_risks[i], sd = sds_given_risks[i])
    }
  }

  return(samples)
}
