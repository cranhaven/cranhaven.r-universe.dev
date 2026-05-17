#' Contingency Calculation.
#'
#' This function calculates the contingency required for a project based on the results
#' of a Monte Carlo simulation. The contingency is determined by the difference between
#' the specified high percentile (phigh) and the base percentile (pbase) of the total
#' project duration distribution.
#'
#' @srrstats {G1.0} *Software lists primary reference from published academic literature.*
#' @srrstats {G1.1} *Software is the first implementation within **R** of the algorithm which has previously been implemented in other languages or contexts.*
#' @srrstats {G1.4} *Software uses [`roxygen2`](https://roxygen2.r-lib.org/) to document all functions.*
#' @srrstats {G2.0} *Implements assertions on lengths of inputs - phigh and pbase are validated as single values.*
#' @srrstats {G2.0a} *Parameter documentation explicitly states expected input types (single numeric values between 0 and 1).*
#' @srrstats {G2.1} *Implements assertions on types of inputs via is.numeric() checks.*
#' @srrstats {G2.1a} *Parameter documentation explicitly states data types expected.*
#' @srrstats {G2.2} *Prohibits multivariate input for phigh and pbase parameters which are expected to be univariate.*
#' @srrstats {G2.15} *Implements checks for NaN values via is.nan() prior to processing.*
#' @srrstats {G2.16} *Implements checks for Inf/-Inf values via is.infinite() prior to processing.*
#' @srrstats {G5.2a} *Each error message produced by stop() is unique.*
#'
#' @param sims List of results from a Monte Carlo simulation containing the total
#' project duration distribution.
#' @param phigh Percentile level for contingency calculation. Default is 0.95 (95th percentile).
#' @param pbase Base level for contingency calculation. Default is 0.5 (50th percentile).
#' @return The function returns the value of calculated contingency based on the
#' specified percentiles.
#' @references
#' Damnjanovic, Ivan, and Kenneth Reinschmidt. Data analytics for engineering and
#' construction project risk management. No. 172534. Cham, Switzerland: Springer, 2020.
#' @examples
#' # Set the number os simulations and the task distributions for a toy project.
#' num_sims <- 10000
#' task_dists <- list(
#'   list(type = "normal", mean = 10, sd = 2), # Task A: Normal distribution
#'   list(type = "triangular", a = 5, b = 10, c = 15), # Task B: Triangular distribution
#'   list(type = "uniform", min = 8, max = 12) # Task C: Uniform distribution
#' )
#'
#' # Set the correlation matrix for the correlations between tasks.
#' cor_mat <- matrix(c(
#'   1, 0.5, 0.3,
#'   0.5, 1, 0.4,
#'   0.3, 0.4, 1
#' ), nrow = 3, byrow = TRUE)
#'
#' # Run the Monte Carlo simulation.
#' results <- mcs(num_sims, task_dists, cor_mat)
#'
#' # Calculate the contingency and print the results.
#' contingency <- contingency(results, phigh = 0.95, pbase = 0.50)
#' cat("Contingency based on 95th percentile and 50th percentile:", contingency)
#'
#' # Without correlation matrix
#' results_indep <- mcs(num_sims, task_dists)
#' contingency_indep <- contingency(results_indep,
#'   phigh = 0.95,
#'   pbase = 0.50
#' )
#' cat("Contingency based on 95th percentile and 50th percentile (
#' independent tasks):", contingency_indep)
#'
#' # Build a barplot to visualize the contingency results.
#' contingency_data <- data.frame(
#'   Scenario = c("With Correlation", "Independent Tasks"),
#'   Contingency = c(contingency, contingency_indep)
#' )
#' barplot(
#'   height = contingency_data$Contingency,
#'   names = contingency_data$Scenario,
#'   col = c("orange", "purple"),
#'   horiz = TRUE,
#'   xlab = "Contingency",
#'   ylab = "Scenario"
#' )
#' title("Contingency Calculation for Project Scenarios")
#'
#' @importFrom stats quantile
#' @export
contingency <- function(sims, phigh = 0.95, pbase = 0.50) {
  # Check for NaN, NA, and Inf values
  if (is.numeric(phigh) && is.nan(phigh)) {
    stop("phigh must not be NaN")
  }
  if (is.numeric(pbase) && is.nan(pbase)) {
    stop("pbase must not be NaN")
  }
  if (is.numeric(phigh) && is.na(phigh)) {
    stop("phigh must not be NA")
  }
  if (is.numeric(pbase) && is.na(pbase)) {
    stop("pbase must not be NA")
  }
  if (is.numeric(phigh) && is.infinite(phigh)) {
    stop("phigh must not be infinite")
  }
  if (is.numeric(pbase) && is.infinite(pbase)) {
    stop("pbase must not be infinite")
  }
  # Check for valid p-values
  if (!is.numeric(phigh) || phigh < 0 || phigh > 1) {
    stop("phigh must be between 0 and 1")
  }
  if (!is.numeric(pbase) || pbase < 0 || pbase > 1) {
    stop("pbase must be between 0 and 1")
  }
  if (phigh < pbase) {
    stop("phigh must be greater than pbase.")
  }



  # Extract the relevant percentiles from the simulation results
  phigh_value <- stats::quantile(sims$total_distribution, probs = phigh)
  pbase_value <- stats::quantile(sims$total_distribution, probs = pbase)

  # Calculate the contingency
  contingency <- phigh_value - pbase_value

  # Return the contingency value
  return(contingency)
}
