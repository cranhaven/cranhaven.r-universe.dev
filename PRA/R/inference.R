#' Bayesian Inference for Risk Probability.
#'
#' This function calculates the overall probability of a risk event 'R' occurring
#' based on the probabilities of multiple root causes and their associated conditional probabilities.
#'
#' @srrstats {G1.0} *Software lists primary reference from published academic literature.*
#' @srrstats {G1.1} *Software is the first implementation within **R** of the algorithm which has previously been implemented in other languages or contexts.*
#' @srrstats {G1.4} *Software uses [`roxygen2`](https://roxygen2.r-lib.org/) to document all functions.*
#' @srrstats {G2.0} *Implements assertions on lengths of inputs - all vectors must have same length.*
#' @srrstats {G2.0a} *Parameter documentation explicitly states expected input structure.*
#' @srrstats {G2.1} *Implements assertions on types of inputs - probability values must be numeric between 0 and 1.*
#' @srrstats {G2.1a} *Parameter documentation explicitly states data types expected (vectors of probabilities).*
#' @srrstats {G2.15} *Implements checks for NaN values via is.nan() prior to processing.*
#' @srrstats {G2.16} *Implements checks for Inf/-Inf values via is.infinite() prior to processing.*
#' @srrstats {G5.2a} *Each error message produced by stop() is unique.*
#'
#' @param cause_probs A vector of probabilities for each root cause 'C_i'.
#' @param risks_given_causes A vector of conditional probabilities of the risk event 'R' given each cause 'C_i'.
#' @param risks_given_not_causes A vector of conditional probabilities of the risk event 'R' given not each cause 'C_i'.
#' @return The function returns a numeric value for the probability of risk event 'R'.
#' @references
#' Damnjanovic, Ivan, and Kenneth Reinschmidt. Data analytics for engineering and
#' construction project risk management. No. 172534. Cham, Switzerland: Springer, 2020.
#' @examples
#' cause_probs <- c(0.3, 0.2)
#' risks_given_causes <- c(0.8, 0.6)
#' risks_given_not_causes <- c(0.2, 0.4)
#' risk_prob_value <- risk_prob(cause_probs, risks_given_causes, risks_given_not_causes)
#' print(risk_prob_value)
#'
#' @export
risk_prob <- function(cause_probs, risks_given_causes, risks_given_not_causes) {
  # Validate inputs
  if (any(is.nan(cause_probs)) || any(is.nan(risks_given_causes)) || any(is.nan(risks_given_not_causes))) {
    stop("Input vectors must not contain NaN values.")
  }
  if (anyNA(cause_probs) || anyNA(risks_given_causes) || anyNA(risks_given_not_causes)) {
    stop("Input vectors must not contain NA values.")
  }
  if (any(is.infinite(cause_probs)) || any(is.infinite(risks_given_causes)) || any(is.infinite(risks_given_not_causes))) {
    stop("Input vectors must not contain infinite values.")
  }
  if (length(cause_probs) != length(risks_given_causes) || length(cause_probs) != length(risks_given_not_causes)) {
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

  # Calculate P(R) using the law of total probability
  total_risk_prob <- 0
  for (i in seq_along(cause_probs)) {
    not_cause_prob <- 1 - cause_probs[i]
    total_risk_prob <- total_risk_prob +
      (risks_given_causes[i] * cause_probs[i]) +
      (risks_given_not_causes[i] * not_cause_prob)
  }

  return(total_risk_prob)
}

#' Cost Probability Density.
#'
#' This function generates random samples from a mixture model representing the cost 'A'
#' associated with multiple risk events 'R_i'. Each risk event has its own probability,
#' mean, and standard deviation for the cost distribution. The function also accounts for a baseline cost
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
#' @srrstats {G2.15} *Implements checks for NaN values via is.nan() prior to processing.*
#' @srrstats {G2.16} *Implements checks for Inf/-Inf values via is.infinite() prior to processing.*
#' @srrstats {G5.2a} *Each error message produced by stop() is unique.*
#'
#' @param num_sims Number of random samples to draw from the mixture model.
#' @param risk_probs A vector of probabilities for each risk event 'R_i'.
#' @param means_given_risks A vector of means of the normal distribution for cost 'A' given each risk event 'R_i'.
#' @param sds_given_risks A vector of standard deviations of the normal distribution for cost 'A' given each risk event 'R_i'.
#' @param base_cost The baseline cost given no risk event occurs.
#' @return A numeric vector of random samples from the mixture model.
#' @references
#' Damnjanovic, Ivan, and Kenneth Reinschmidt. Data analytics for engineering and
#' construction project risk management. No. 172534. Cham, Switzerland: Springer, 2020.
#' @examples
#' # Example with three risk events
#' num_sims <- 1000
#' risk_probs <- c(0.3, 0.5, 0.2)
#' means_given_risks <- c(10000, 15000, 5000)
#' sds_given_risks <- c(2000, 1000, 1000)
#' base_cost <- 2000
#' samples <- cost_pdf(
#'   num_sims = num_sims,
#'   risk_probs = risk_probs,
#'   means_given_risks = means_given_risks,
#'   sds_given_risks = sds_given_risks,
#'   base_cost = base_cost
#' )
#' hist(samples, breaks = 30, col = "skyblue", main = "Histogram of Cost", xlab = "Cost")
#' @importFrom stats rbinom rnorm
#' @export
cost_pdf <- function(num_sims, risk_probs, means_given_risks, sds_given_risks, base_cost = 0) {
  # Validate inputs
  if (num_sims <= 0 || !is.numeric(num_sims)) stop("num_sims must be a positive integer.")
  if (any(is.nan(risk_probs)) || any(is.nan(means_given_risks)) || any(is.nan(sds_given_risks))) {
    stop("risk_probs, means_given_risks, and sds_given_risks must not contain NaN values.")
  }
  if (anyNA(risk_probs) || anyNA(means_given_risks) || anyNA(sds_given_risks)) {
    stop("risk_probs, means_given_risks, and sds_given_risks must not contain NA values.")
  }
  if (any(is.infinite(risk_probs)) || any(is.infinite(means_given_risks)) || any(is.infinite(sds_given_risks))) {
    stop("risk_probs, means_given_risks, and sds_given_risks must not contain infinite values.")
  }
  if (is.nan(base_cost)) stop("base_cost must not be NaN.")
  if (is.na(base_cost)) stop("base_cost must not be NA.")
  if (is.infinite(base_cost)) stop("base_cost must not be infinite.")
  if (any(risk_probs < 0 | risk_probs > 1)) stop("All risk_probs must be between 0 and 1.")
  if (sum(risk_probs) > 1) stop("Sum of risk_probs must not exceed 1.")
  if (length(risk_probs) != length(means_given_risks) || length(risk_probs) != length(sds_given_risks)) {
    stop("risk_probs, means_given_risks, and sds_given_risks must have the same length.")
  }
  if (any(sds_given_risks < 0)) stop("Standard deviations must be non-negative.")

  # Number of risk events
  num_risks <- length(risk_probs)

  # Initialize cost samples with base cost
  samples <- rep(base_cost, num_sims)

  # Iterate over each risk event
  for (i in seq_len(num_risks)) {
    # Simulate risk outcomes for the current risk event
    risk_event <- rbinom(num_sims, size = 1, prob = risk_probs[i])

    # Add cost samples for the current risk event
    samples <- samples + ifelse(
      risk_event == 1,
      rnorm(num_sims, mean = means_given_risks[i], sd = sds_given_risks[i]),
      0
    )
  }

  return(samples)
}
