#' Comprehensive Goodness-of-Fit Analysis for GKw Family Distributions
#'
#' @description
#' Computes and displays a comprehensive set of goodness-of-fit statistics for
#' distributions from the Generalized Kumaraswamy (GKw) family fitted using \code{gkwfit}.
#' This function provides various measures including distance-based tests,
#' information criteria, moment comparisons, probability plot metrics, and additional
#' visualization tools for model adequacy assessment.
#'
#' @param object An object of class "gkwfit", typically the result of a call to \code{gkwfit}.
#' @param simulate_p_values Logical; if TRUE, uses parametric bootstrap to compute
#'        approximate p-values for distance-based tests. Default is FALSE.
#' @param n_bootstrap Number of bootstrap replicates for p-value simulation.
#'        Only used if \code{simulate_p_values = TRUE}. Default is 1000.
#' @param plot Logical; if TRUE, generates additional diagnostic plots beyond
#'        those already in the gkwfit object. Default is TRUE.
#' @param print_summary Logical; if TRUE, prints a formatted summary of the
#'        goodness-of-fit statistics. Default is TRUE.
#' @param verbose Logical; if TRUE, provides additional details and explanations
#'        about the test statistics. Default is FALSE.
#' @param title Plot title.
#' @param ncols Number of columns to draw plots in graphics window.
#' @param theme A ggplot theme for all plots. default ggplot2::theme_bw()
#' @param ... Additional arguments to be passed to plotting functions.
#'
#' @details
#' This function calculates the following goodness-of-fit statistics:
#'
#' \strong{Distance-based tests:}
#' \itemize{
#'   \item Kolmogorov-Smirnov (KS) statistic: Measures the maximum absolute difference
#'         between the empirical and theoretical CDFs.
#'   \item Cramer-von Mises (CvM) statistic: Measures the integrated squared difference
#'         between the empirical and theoretical CDFs.
#'   \item Anderson-Darling (AD) statistic: Similar to CvM but places more weight on
#'         the tails of the distribution.
#'   \item Watson (\eqn{W^2}) statistic: A modification of CvM that is location-invariant on the circle.
#' }
#'
#' \strong{Information criteria:}
#' \itemize{
#'   \item Akaike Information Criterion (AIC): \eqn{-2\log(L) + 2k}
#'   \item Bayesian Information Criterion (BIC): \eqn{-2\log(L) + k\log(n)}
#'   \item Corrected AIC (AICc): \eqn{AIC + \frac{2k(k+1)}{n-k-1}}
#'   \item Consistent AIC (CAIC): \eqn{-2\log(L) + k(\log(n) + 1)}
#'   \item Hannan-Quinn IC (HQIC): \eqn{-2\log(L) + 2k\log(\log(n))}
#' }
#'
#' \strong{Moment-based comparisons:}
#' \itemize{
#'   \item Theoretical vs. sample mean, variance, skewness, and kurtosis
#'   \item Standardized moment differences (relative to sample standard deviation)
#'   \item Root mean squared error of moments (RMSE)
#' }
#'
#' \strong{Probability plot metrics:}
#' \itemize{
#'   \item Correlation coefficient from P-P plot (closer to 1 indicates better fit)
#'   \item Area between P-P curve and diagonal line
#'   \item Mean absolute error in Q-Q plot
#' }
#'
#' \strong{Likelihood statistics:}
#' \itemize{
#'   \item Log-likelihood
#'   \item Log-likelihood per observation
#'   \item Pseudo-\eqn{R^2} measure for bounded distributions
#' }
#'
#' \strong{Prediction accuracy metrics:}
#' \itemize{
#'   \item Mean Absolute Error (MAE) between empirical and theoretical CDF
#'   \item Root Mean Squared Error (RMSE) between empirical and theoretical CDF
#'   \item Continuous Ranked Probability Score (CRPS)
#' }
#'
#' For model selection, lower values of information criteria (AIC, BIC, etc.) indicate
#' better fit, while higher values of correlation coefficients and pseudo-\eqn{R^2} indicate
#' better fit. The distance-based tests are primarily used for hypothesis testing rather
#' than model selection.
#'
#' When \code{simulate_p_values = TRUE}, the function performs parametric bootstrap to
#' compute approximate p-values for the distance-based tests, which accounts for parameter
#' estimation uncertainty.
#'
#' @return
#' An object of class "gkwgof" (inheriting from "list") containing the following components:
#' \itemize{
#'   \item \code{family}: The fitted distribution family
#'   \item \code{coefficients}: The estimated model parameters
#'   \item \code{sample_size}: The number of observations used in fitting
#'   \item \code{distance_tests}: Results from KS, CvM, AD, and Watson tests
#'   \item \code{information_criteria}: AIC, BIC, AICc, CAIC, and HQIC values
#'   \item \code{moments}: Theoretical moments, sample moments, and their differences
#'   \item \code{probability_plots}: Metrics from P-P and Q-Q plots
#'   \item \code{likelihood}: Log-likelihood statistics and pseudo-\eqn{R^2} measure
#'   \item \code{prediction}: Prediction accuracy metrics
#'   \item \code{plots}: Additional diagnostic plots (if \code{plot = TRUE})
#'   \item \code{p_values}: Simulated p-values (if \code{simulate_p_values = TRUE})
#'   \item \code{bootstrap_stats}: Bootstrap distribution of test statistics (if
#'         \code{simulate_p_values = TRUE})
#'   \item \code{call}: The matched call
#'   \item \code{gkwfit_object}: The original gkwfit object used for analysis
#' }
#'
#' @examples
#' \donttest{
#' # Example 1: Simulate and analyze data from a Kumaraswamy (Kw) distribution
#' set.seed(2203) # Set seed for reproducibility
#' # Simulate 1000 observations from Kumaraswamy distribution with parameters alpha=2.5, beta=1.8
#' data_kw <- rkw(n = 1000, alpha = 2.5, beta = 1.8)
#' # Fit the Kumaraswamy distribution to the data
#' fit_kw <- gkwfit(data_kw, family = "kw")
#' # Basic goodness-of-fit analysis
#' gof_kw <- gkwgof(fit_kw)
#' # Analysis with bootstrap simulated p-values
#' gof_kw_bootstrap <- gkwgof(fit_kw, simulate_p_values = TRUE, n_bootstrap = 500)
#' # Detailed summary with additional explanations
#' gof_kw_verbose <- gkwgof(fit_kw, verbose = TRUE)
#'
#' # Example 2: Comparing different distributions from the GKw family
#' # Simulate data from the Generalized Kumaraswamy (GKw) distribution
#' data_gkw <- rgkw(n = 1000, alpha = 2.0, beta = 1.5, gamma = 3.0, delta = 2.0, lambda = 1.2)
#' # Fit different models to the same data
#' fit_kw <- gkwfit(data_gkw, family = "kw") # Kumaraswamy model (simplified)
#' fit_bkw <- gkwfit(data_gkw, family = "bkw") # Beta-Kumaraswamy model
#' fit_ekw <- gkwfit(data_gkw, family = "ekw") # Exponentiated Kumaraswamy model
#' fit_gkw <- gkwfit(data_gkw, family = "gkw") # Full Generalized Kumaraswamy model
#' fit_beta <- gkwfit(data_gkw, family = "beta") # Standard Beta model
#' # Goodness-of-fit analysis for each model (without printing summaries)
#' gof_kw <- gkwgof(fit_kw, print_summary = FALSE)
#' gof_bkw <- gkwgof(fit_bkw, print_summary = FALSE)
#' gof_ekw <- gkwgof(fit_ekw, print_summary = FALSE)
#' gof_gkw <- gkwgof(fit_gkw, print_summary = FALSE)
#' gof_beta <- gkwgof(fit_beta, print_summary = FALSE)
#' # Information criteria comparison for model selection
#' ic_comparison <- data.frame(
#'   family = c("kw", "bkw", "ekw", "gkw", "beta"),
#'   n_params = c(
#'     length(gof_kw$coefficients),
#'     length(gof_bkw$coefficients),
#'     length(gof_ekw$coefficients),
#'     length(gof_gkw$coefficients),
#'     length(gof_beta$coefficients)
#'   ),
#'   logLik = c(
#'     gof_kw$likelihood$loglik,
#'     gof_bkw$likelihood$loglik,
#'     gof_ekw$likelihood$loglik,
#'     gof_gkw$likelihood$loglik,
#'     gof_beta$likelihood$loglik
#'   ),
#'   AIC = c(
#'     gof_kw$information_criteria$AIC,
#'     gof_bkw$information_criteria$AIC,
#'     gof_ekw$information_criteria$AIC,
#'     gof_gkw$information_criteria$AIC,
#'     gof_beta$information_criteria$AIC
#'   ),
#'   BIC = c(
#'     gof_kw$information_criteria$BIC,
#'     gof_bkw$information_criteria$BIC,
#'     gof_ekw$information_criteria$BIC,
#'     gof_gkw$information_criteria$BIC,
#'     gof_beta$information_criteria$BIC
#'   ),
#'   AICc = c(
#'     gof_kw$information_criteria$AICc,
#'     gof_bkw$information_criteria$AICc,
#'     gof_ekw$information_criteria$AICc,
#'     gof_gkw$information_criteria$AICc,
#'     gof_beta$information_criteria$AICc
#'   )
#' )
#' # Sort by AIC (lower is better)
#' ic_comparison <- ic_comparison[order(ic_comparison$AIC), ]
#' print(ic_comparison)
#'
#' # Example 3: Comparative visualization
#' # Generate data from Beta distribution to demonstrate another case
#' set.seed(2203)
#' data_beta <- rbeta_(n = 1000, gamma = 2.5, delta = 1.5)
#' # Fit different distributions
#' fit_beta_true <- gkwfit(data_beta, family = "beta")
#' fit_kw_misspec <- gkwfit(data_beta, family = "kw")
#' fit_gkw_complex <- gkwfit(data_beta, family = "gkw")
#' # Goodness-of-fit analysis
#' gof_beta_true <- gkwgof(fit_beta_true, print_summary = FALSE, plot = FALSE)
#' gof_kw_misspec <- gkwgof(fit_kw_misspec, print_summary = FALSE, plot = FALSE)
#' gof_gkw_complex <- gkwgof(fit_gkw_complex, print_summary = FALSE, plot = FALSE)
#' # Comparative goodness-of-fit plot
#'
#' plotcompare(
#'   list(
#'     "Beta (correct)" = gof_beta_true,
#'     "Kw (underspecified)" = gof_kw_misspec,
#'     "GKw (overspecified)" = gof_gkw_complex
#'   ),
#'   title = "Comparison of fits for Beta data"
#' )
#'
#' # Example 5: Likelihood ratio tests for nested models
#' # Testing if the GKw distribution is significantly better than BKw (lambda = 1)
#' # Null hypothesis: lambda = 1 (BKw is adequate)
#' # Alternative hypothesis: lambda != 1 (GKw is necessary)
#' # Fitting nested models to data
#' nested_data <- rgkw(n = 1000, alpha = 2.0, beta = 1.5, gamma = 3.0, delta = 2.0, lambda = 1.0)
#' nested_fit_bkw <- gkwfit(nested_data, family = "bkw") # Restricted model (lambda = 1)
#' nested_fit_gkw <- gkwfit(nested_data, family = "gkw") # Unrestricted model
#'
#' # Extracting log-likelihoods
#' ll_bkw <- nested_fit_bkw$loglik
#' ll_gkw <- nested_fit_gkw$loglik
#' # Calculating likelihood ratio test statistic
#' lr_stat <- 2 * (ll_gkw - ll_bkw)
#' # Calculating p-value (chi-square with 1 degree of freedom)
#' lr_pvalue <- 1 - pchisq(lr_stat, df = 1)
#' # Displaying test results
#' cat("Likelihood ratio test:\n")
#' cat("Test statistic:", round(lr_stat, 4), "\n")
#' cat("P-value:", format.pval(lr_pvalue), "\n")
#' cat("Conclusion:", ifelse(lr_pvalue < 0.05,
#'   "Reject H0 - GKw is necessary",
#'   "Fail to reject H0 - BKw is adequate"
#' ), "\n")
#'
#' # Example 6: Power simulation
#' # Checking the power of goodness-of-fit tests in detecting misspecifications
#' # Function to simulate power
#' simulate_power <- function(n_sim = 1000, n_obs = 1000, alpha_level = 0.05) {
#'   # Counters for rejections
#'   ks_rejections <- 0
#'   ad_rejections <- 0
#'
#'   for (i in 1:n_sim) {
#'     # Simulating data from GKw, but fitting a Kw model (incorrect)
#'     sim_data <- rgkw(
#'       n = n_obs, alpha = 2.0, beta = 1.5,
#'       gamma = 3.0, delta = 2.0, lambda = 1.5
#'     )
#'
#'     # Fitting incorrect model
#'     sim_fit_kw <- gkwfit(sim_data, family = "kw")
#'
#'     # Calculating test statistics
#'     sim_gof <- gkwgof(sim_fit_kw,
#'       simulate_p_values = TRUE,
#'       n_bootstrap = 200, print_summary = FALSE, plot = FALSE
#'     )
#'
#'     # Checking rejections in tests
#'     if (sim_gof$p_values$ks < alpha_level) ks_rejections <- ks_rejections + 1
#'     if (sim_gof$p_values$ad < alpha_level) ad_rejections <- ad_rejections + 1
#'   }
#'
#'   # Calculating power
#'   power_ks <- ks_rejections / n_sim
#'   power_ad <- ad_rejections / n_sim
#'
#'   return(list(
#'     power_ks = power_ks,
#'     power_ad = power_ad
#'   ))
#' }
#'
#' # Run power simulation with reduced number of repetitions for example
#' power_results <- simulate_power(n_sim = 100)
#' cat("Estimated power (KS):", round(power_results$power_ks, 2), "\n")
#' cat("Estimated power (AD):", round(power_results$power_ad, 2), "\n")
#' }
#'
#' @seealso \code{\link{gkwfit}}, \code{\link{summary.gkwfit}}, \code{\link{print.gkwgof}},
#'          \code{\link{plot.gkwgof}}, \code{\link{plotcompare}}
#'
#' @references
#' Anderson, T. W., & Darling, D. A. (1952). Asymptotic theory of certain "goodness of fit" criteria
#' based on stochastic processes. Annals of Mathematical Statistics, 23(2), 193-212.
#'
#' Burnham, K. P., & Anderson, D. R. (2002). Model Selection and Multimodel Inference: A Practical
#' Information-Theoretic Approach (2nd ed.). Springer.
#'
#' D'Agostino, R. B., & Stephens, M. A. (1986). Goodness-of-fit techniques. Marcel Dekker, Inc.
#'
#' Kumaraswamy, P. (1980). A generalized probability density function for double-bounded random processes.
#' Journal of Hydrology, 46(1-2), 79-88.
#'
#' @importFrom stats cor var
#' @export
gkwgof <- function(object, simulate_p_values = FALSE, n_bootstrap = 1000,
                   plot = TRUE, print_summary = TRUE, verbose = FALSE,
                   theme = ggplot2::theme_bw(), ncols = 4, title = NULL, ...) {
  # Validate inputs
  if (!inherits(object, "gkwfit")) {
    stop("Input 'object' must be of class 'gkwfit'.")
  }

  # Match and store the function call
  call <- match.call()

  # Extract relevant information from the gkwfit object
  family <- object$family
  coefficients <- object$coefficients
  data <- object$data
  loglik <- object$loglik
  n <- length(data)
  k <- length(coefficients) # number of parameters

  # Sort data for empirical CDF calculations
  sorted_data <- sort(data)

  # Basic checks on the data
  if (n < 5) {
    warning("Sample size is very small (n < 5). Results may not be reliable.")
  }

  if (any(is.na(data))) {
    stop("Data contains missing values.")
  }

  # Create a container for results
  results <- list(
    family = family,
    coefficients = coefficients,
    sample_size = n,
    call = call,
    gkwfit_object = object
  )

  # Calculate theoretical CDF for the sorted data
  p_theoretical <- .calculate_theoretical_cdf(sorted_data, family, coefficients)

  # Calculate empirical CDF (using Blom's plotting position formula)
  p_empirical <- (1:n - 0.375) / (n + 0.25)

  # Distance-based test statistics
  results$distance_tests <- .calculate_distance_tests(sorted_data, p_empirical, p_theoretical)

  # Information criteria (extracted from the gkwfit object and expanded)
  results$information_criteria <- .calculate_information_criteria(loglik, k, n)

  # Moment-based comparisons
  results$moments <- .calculate_moment_comparisons(data, family, coefficients)

  # Probability plot metrics
  results$probability_plots <- .calculate_probability_plot_metrics(
    p_empirical, p_theoretical,
    sorted_data, family, coefficients
  )

  # Likelihood statistics
  results$likelihood <- .calculate_likelihood_statistics(loglik, n, data, family, coefficients)

  # Prediction accuracy metrics
  results$prediction <- .calculate_prediction_metrics(p_empirical, p_theoretical)

  # Simulate p-values using parametric bootstrap if requested
  if (simulate_p_values) {
    bootstrap_results <- .simulate_p_values_bootstrap(
      object, n_bootstrap, results$distance_tests
    )
    results$p_values <- bootstrap_results$p_values
    results$bootstrap_stats <- bootstrap_results$bootstrap_stats
  }

  # Generate additional diagnostic plots if requested
  if (plot) {
    results$plots <- .generate_additional_plots(object, data, p_empirical, p_theoretical, theme, ...)

    # Check if 'patchwork' and 'ggplot2' packages are available
    if (requireNamespace("patchwork", quietly = TRUE) &&
      requireNamespace("ggplot2", quietly = TRUE)) {
      # Retrieve the list of plots
      plot_list <- results$plots

      # Only proceed if we have at least one plot to combine
      if (length(plot_list) > 0) {
        # Combine the plots using patchwork::wrap_plots with a layout of 4 columns (by row)
        combined_plot <- patchwork::wrap_plots(
          plot_list,
          byrow = TRUE,
          ncol = ncols
        ) +
          patchwork::plot_annotation(
            title = ifelse(is.null(title), paste("Goodness-of-Fit Diagnostics for", toupper(family), "Distribution"), title),
            subtitle = NULL
          )

        # Display the combined plot
        print(combined_plot)
      }
    }
  }


  # Print a formatted summary if requested
  if (print_summary) {
    .print_gof_summary(results, verbose)
  }

  # Set the class for the returned object
  class(results) <- "gkwgof"

  return(results)
}

#' Calculate Theoretical CDF Values for GKw Family Distributions
#'
#' @param x Numeric vector of data points
#' @param family Character string specifying the distribution family
#' @param params Named numeric vector of distribution parameters
#' @return Numeric vector of CDF values
#' @keywords internal
.calculate_theoretical_cdf <- function(x, family, params) {
  # Apply the appropriate CDF function based on the family
  switch(family,
    "gkw" = pgkw(
      x, params["alpha"], params["beta"], params["gamma"],
      params["delta"], params["lambda"]
    ),
    "bkw" = pbkw(
      x, params["alpha"], params["beta"], params["gamma"],
      params["delta"]
    ),
    "kkw" = pkkw(
      x, params["alpha"], params["beta"], params["delta"],
      params["lambda"]
    ),
    "ekw" = pekw(x, params["alpha"], params["beta"], params["lambda"]),
    "mc" = pmc(x, params["gamma"], params["delta"], params["lambda"]),
    "kw" = pkw(x, params["alpha"], params["beta"]),
    "beta" = pbeta_(x, params["gamma"], params["delta"]),
    stop("Unknown family specified")
  )
}

#' Calculate Theoretical PDF Values for GKw Family Distributions
#'
#' @param x Numeric vector of data points
#' @param family Character string specifying the distribution family
#' @param params Named numeric vector of distribution parameters
#' @return Numeric vector of PDF values
#' @keywords internal
.calculate_theoretical_pdf <- function(x, family, params) {
  # Apply the appropriate PDF function based on the family
  switch(family,
    "gkw" = dgkw(
      x, params["alpha"], params["beta"], params["gamma"],
      params["delta"], params["lambda"]
    ),
    "bkw" = dbkw(
      x, params["alpha"], params["beta"], params["gamma"],
      params["delta"]
    ),
    "kkw" = dkkw(
      x, params["alpha"], params["beta"], params["delta"],
      params["lambda"]
    ),
    "ekw" = dekw(x, params["alpha"], params["beta"], params["lambda"]),
    "mc" = dmc(x, params["gamma"], params["delta"], params["lambda"]),
    "kw" = dkw(x, params["alpha"], params["beta"]),
    "beta" = dbeta_(x, params["gamma"], params["delta"]),
    stop("Unknown family specified")
  )
}

#' Calculate Theoretical Quantiles for GKw Family Distributions
#'
#' @param p Numeric vector of probabilities
#' @param family Character string specifying the distribution family
#' @param params Named numeric vector of distribution parameters
#' @return Numeric vector of quantiles
#' @keywords internal
.calculate_theoretical_quantiles <- function(p, family, params) {
  # Apply the appropriate quantile function based on the family
  switch(family,
    "gkw" = qgkw(
      p, params["alpha"], params["beta"], params["gamma"],
      params["delta"], params["lambda"]
    ),
    "bkw" = qbkw(
      p, params["alpha"], params["beta"], params["gamma"],
      params["delta"]
    ),
    "kkw" = qkkw(
      p, params["alpha"], params["beta"], params["delta"],
      params["lambda"]
    ),
    "ekw" = qekw(p, params["alpha"], params["beta"], params["lambda"]),
    "mc" = qmc(p, params["gamma"], params["delta"], params["lambda"]),
    "kw" = qkw(p, params["alpha"], params["beta"]),
    "beta" = qbeta_(p, params["gamma"], params["delta"]),
    stop("Unknown family specified")
  )
}

#' Generate Random Samples from GKw Family Distributions
#'
#' @param n Integer number of samples to generate
#' @param family Character string specifying the distribution family
#' @param params Named numeric vector of distribution parameters
#' @return Numeric vector of random samples
#' @keywords internal
.generate_random_samples <- function(n, family, params) {
  # Apply the appropriate random number generation function based on the family
  switch(family,
    "gkw" = rgkw(
      n, params["alpha"], params["beta"], params["gamma"],
      params["delta"], params["lambda"]
    ),
    "bkw" = rbkw(
      n, params["alpha"], params["beta"], params["gamma"],
      params["delta"]
    ),
    "kkw" = rkkw(
      n, params["alpha"], params["beta"], params["delta"],
      params["lambda"]
    ),
    "ekw" = rekw(n, params["alpha"], params["beta"], params["lambda"]),
    "mc" = rmc(n, params["gamma"], params["delta"], params["lambda"]),
    "kw" = rkw(n, params["alpha"], params["beta"]),
    "beta" = rbeta_(n, params["gamma"], params["delta"]),
    stop("Unknown family specified")
  )
}

#' Calculate Distance-Based Test Statistics
#'
#' @param sorted_data Sorted numeric vector of the data
#' @param p_empirical Numeric vector of empirical CDF values
#' @param p_theoretical Numeric vector of theoretical CDF values
#' @return A list containing the calculated test statistics
#' @keywords internal
.calculate_distance_tests <- function(sorted_data, p_empirical, p_theoretical) {
  n <- length(sorted_data)

  # Kolmogorov-Smirnov statistic
  ks_stat <- max(abs(p_empirical - p_theoretical))

  # Cramer-von Mises statistic
  cvm_stat <- (1 / (12 * n)) + sum((p_theoretical - p_empirical)^2)

  # Anderson-Darling statistic
  # Protect against 0 and 1 in calculations
  p_adj <- pmin(pmax(p_theoretical, 1e-10), 1 - 1e-10)
  ad_summand <- (2 * (1:n) - 1) * (log(p_adj) + log(1 - rev(p_adj)))
  ad_stat <- -n - (1 / n) * sum(ad_summand)

  # Watson statistic
  mean_diff <- mean(p_theoretical - p_empirical)
  w2_stat <- cvm_stat - n * mean_diff^2

  # Return all statistics in a list
  list(
    ks = ks_stat,
    cvm = cvm_stat,
    ad = ad_stat,
    watson = w2_stat
  )
}

#' Calculate Information Criteria
#'
#' @param loglik Log-likelihood value
#' @param k Number of parameters
#' @param n Sample size
#' @return A list containing various information criteria
#' @keywords internal
.calculate_information_criteria <- function(loglik, k, n) {
  # AIC: Akaike Information Criterion
  aic <- -2 * loglik + 2 * k

  # BIC: Bayesian Information Criterion
  bic <- -2 * loglik + k * log(n)

  # AICc: Corrected AIC
  aicc <- aic + (2 * k * (k + 1)) / (n - k - 1)

  # CAIC: Consistent AIC
  caic <- -2 * loglik + k * (log(n) + 1)

  # HQIC: Hannan-Quinn Information Criterion
  hqic <- -2 * loglik + 2 * k * log(log(n))

  # Return all information criteria in a list
  list(
    AIC = aic,
    BIC = bic,
    AICc = aicc,
    CAIC = caic,
    HQIC = hqic
  )
}




#' Calculate Theoretical Moments for GKw Family Distributions
#'
#' @param family Character string specifying the distribution family
#' @param params Named numeric vector of distribution parameters
#' @param num_points Number of points for numerical integration
#' @return A vector containing the first four theoretical moments
#' @keywords internal
.calculate_theoretical_moments <- function(family, params, num_points = 1000) {
  # Create a grid for numerical integration
  grid <- seq(0.001, 0.999, length.out = num_points)
  step <- grid[2] - grid[1]

  # Calculate PDF at the grid points
  pdf_values <- .calculate_theoretical_pdf(grid, family, params)

  # Initialize moments
  moments <- numeric(4)

  # Calculate first 4 raw moments using numerical integration (trapezoidal rule)
  for (m in 1:4) {
    integrand <- grid^m * pdf_values
    moments[m] <- 0.5 * step * sum(integrand[-1] + integrand[-num_points])
  }

  # Calculate central moments (for variance, skewness, kurtosis)
  mu <- moments[1]
  var <- moments[2] - mu^2

  # Calculate skewness
  mu3 <- moments[3] - 3 * mu * moments[2] + 2 * mu^3
  skew <- mu3 / var^1.5

  # Calculate kurtosis
  mu4 <- moments[4] - 4 * mu * moments[3] + 6 * mu^2 * moments[2] - 3 * mu^4
  kurt <- mu4 / var^2

  # Return mean, variance, skewness, kurtosis
  c(mean = mu, var = var, skewness = skew, kurtosis = kurt)
}

#' Calculate Sample Moments
#'
#' @param data Numeric vector of data
#' @return A vector containing the first four sample moments
#' @keywords internal
.calculate_sample_moments <- function(data) {
  n <- length(data)

  # Calculate mean and variance
  mean_val <- mean(data)
  var_val <- var(data) * (n - 1) / n # To match definition used in theoretical calculations

  # Calculate skewness
  m3 <- mean((data - mean_val)^3)
  skew_val <- m3 / var_val^1.5

  # Calculate kurtosis
  m4 <- mean((data - mean_val)^4)
  kurt_val <- m4 / var_val^2

  # Return mean, variance, skewness, kurtosis
  c(mean = mean_val, var = var_val, skewness = skew_val, kurtosis = kurt_val)
}

#' Calculate Moment Comparisons
#'
#' @param data Numeric vector of data
#' @param family Character string specifying the distribution family
#' @param params Named numeric vector of distribution parameters
#' @return A list containing theoretical moments, sample moments, and comparison metrics
#' @keywords internal
.calculate_moment_comparisons <- function(data, family, params) {
  # Calculate theoretical moments
  theor_moments <- .calculate_theoretical_moments(family, params)

  # Calculate sample moments
  sample_moments <- .calculate_sample_moments(data)

  # Calculate absolute differences
  abs_diffs <- abs(theor_moments - sample_moments)

  # Calculate standardized differences (relative to the standard deviation)
  std_diffs <- abs_diffs / sqrt(sample_moments[2])

  # Calculate root mean squared error of moment estimation
  rmse_moments <- sqrt(mean(abs_diffs^2))

  # Return all moment-related metrics in a list
  list(
    theoretical = theor_moments,
    sample = sample_moments,
    absolute_differences = abs_diffs,
    standardized_differences = std_diffs,
    rmse = rmse_moments
  )
}

#' Calculate Probability Plot Metrics
#'
#' @param p_empirical Numeric vector of empirical CDF values
#' @param p_theoretical Numeric vector of theoretical CDF values
#' @param sorted_data Sorted numeric vector of the data
#' @param family Character string specifying the distribution family
#' @param params Named numeric vector of distribution parameters
#' @return A list containing metrics from P-P and Q-Q plots
#' @keywords internal
.calculate_probability_plot_metrics <- function(p_empirical, p_theoretical,
                                                sorted_data, family, params) {
  # P-P plot correlation coefficient
  pp_corr <- cor(p_empirical, p_theoretical)

  # Area between P-P curve and diagonal line
  pp_area <- mean(abs(p_theoretical - p_empirical))

  # Q-Q plot metrics
  # Calculate theoretical quantiles for the same probabilities as in p_empirical
  theor_quantiles <- .calculate_theoretical_quantiles(p_empirical, family, params)

  # Mean absolute error in Q-Q plot
  qq_mae <- mean(abs(sorted_data - theor_quantiles))

  # Correlation in Q-Q plot
  qq_corr <- cor(sorted_data, theor_quantiles)

  # Return all probability plot metrics in a list
  list(
    pp_correlation = pp_corr,
    pp_area = pp_area,
    qq_mae = qq_mae,
    qq_correlation = qq_corr
  )
}

#' Calculate Likelihood Statistics
#'
#' @param loglik Log-likelihood value
#' @param n Sample size
#' @param data Numeric vector of data
#' @param family Character string specifying the distribution family
#' @param params Named numeric vector of distribution parameters
#' @return A list containing likelihood-based statistics
#' @keywords internal
.calculate_likelihood_statistics <- function(loglik, n, data, family, params) {
  # Log-likelihood per observation
  loglik_per_obs <- loglik / n

  # Calculate log-likelihood of a null model (using uniform distribution)
  # For data in (0,1), the uniform PDF equals 1, so log-likelihood = 0
  null_loglik <- 0

  # McFadden's pseudo-\eqn{R^2} (if applicable)
  # For bounded data, the uniform null model often makes more sense
  if (null_loglik != 0) {
    mcfadden_r2 <- 1 - loglik / null_loglik
  } else {
    # Alternative measure for when null_loglik = 0
    mcfadden_r2 <- 1 - exp(-2 * loglik / n)
  }

  # Calculate likelihood ratio index against uniform model
  # Transform to put on 0-1 scale
  lr_index <- 1 - exp(loglik / n)

  # Return all likelihood statistics in a list
  list(
    loglik = loglik,
    loglik_per_obs = loglik_per_obs,
    pseudo_r_squared = mcfadden_r2,
    likelihood_ratio_index = lr_index
  )
}

#' Calculate Prediction Accuracy Metrics
#'
#' @param p_empirical Numeric vector of empirical CDF values
#' @param p_theoretical Numeric vector of theoretical CDF values
#' @return A list containing prediction accuracy metrics
#' @keywords internal
.calculate_prediction_metrics <- function(p_empirical, p_theoretical) {
  # Mean Absolute Error (MAE)
  mae <- mean(abs(p_empirical - p_theoretical))

  # Root Mean Squared Error (RMSE)
  rmse <- sqrt(mean((p_empirical - p_theoretical)^2))

  # Continuous Ranked Probability Score (CRPS) - simplified approximation
  # For a more accurate CRPS, integration over all thresholds would be required
  crps <- mean((p_empirical - p_theoretical)^2)

  # Return all prediction metrics in a list
  list(
    mae = mae,
    rmse = rmse,
    crps = crps
  )
}



#' Simulate P-Values Using Parametric Bootstrap
#'
#' @param object Object of class "gkwfit"
#' @param n_bootstrap Number of bootstrap replicates
#' @param observed_tests List of observed test statistics
#' @return A list containing simulated p-values and bootstrap distributions
#' @keywords internal
.simulate_p_values_bootstrap <- function(object, n_bootstrap, observed_tests) {
  # Extract information from gkwfit object
  family <- object$family
  params <- object$coefficients
  n <- length(object$data)

  # Initialize matrices to store bootstrap test statistics
  bootstrap_ks <- numeric(n_bootstrap)
  bootstrap_cvm <- numeric(n_bootstrap)
  bootstrap_ad <- numeric(n_bootstrap)
  bootstrap_watson <- numeric(n_bootstrap)

  # Perform parametric bootstrap
  for (i in 1:n_bootstrap) {
    # Generate bootstrap sample
    bootstrap_sample <- .generate_random_samples(n, family, params)

    # Sort bootstrap sample
    sorted_bootstrap <- sort(bootstrap_sample)

    # Calculate empirical CDF for bootstrap sample
    p_empirical_bootstrap <- (1:n - 0.375) / (n + 0.25)

    # Calculate theoretical CDF for bootstrap sample
    p_theoretical_bootstrap <- .calculate_theoretical_cdf(sorted_bootstrap, family, params)

    # Calculate test statistics for bootstrap sample
    bootstrap_tests <- .calculate_distance_tests(
      sorted_bootstrap, p_empirical_bootstrap, p_theoretical_bootstrap
    )

    # Store test statistics
    bootstrap_ks[i] <- bootstrap_tests$ks
    bootstrap_cvm[i] <- bootstrap_tests$cvm
    bootstrap_ad[i] <- bootstrap_tests$ad
    bootstrap_watson[i] <- bootstrap_tests$watson
  }

  # Calculate p-values as proportion of bootstrap statistics >= observed
  p_value_ks <- mean(bootstrap_ks >= observed_tests$ks)
  p_value_cvm <- mean(bootstrap_cvm >= observed_tests$cvm)
  p_value_ad <- mean(bootstrap_ad >= observed_tests$ad)
  p_value_watson <- mean(bootstrap_watson >= observed_tests$watson)

  # Return p-values and bootstrap distributions
  list(
    p_values = list(
      ks = p_value_ks,
      cvm = p_value_cvm,
      ad = p_value_ad,
      watson = p_value_watson
    ),
    bootstrap_stats = list(
      ks = bootstrap_ks,
      cvm = bootstrap_cvm,
      ad = bootstrap_ad,
      watson = bootstrap_watson
    )
  )
}


#' Generate Additional Diagnostic Plots Beyond Those in gkwfit
#'
#' @param object Object of class "gkwfit"
#' @param data Numeric vector of data
#' @param p_empirical Numeric vector of empirical CDF values
#' @param p_theoretical Numeric vector of theoretical CDF values
#' @param ... Additional arguments to be passed to plotting functions
#' @param theme A ggplot theme, defaults to theme_classic()
#' @return A list of ggplot2 objects for additional diagnostic plots
#' @keywords internal
.generate_additional_plots <- function(object, data, p_empirical, p_theoretical, theme = ggplot2::theme_classic(), ...) {
  # Extract parameters
  family <- object$family
  params <- object$coefficients
  n <- length(data)
  sorted_data <- sort(data)

  # List to store plots
  plots <- list()

  # Define a base theme modification for all plots that centers titles
  title_theme <- ggplot2::theme(
    plot.title = ggplot2::element_text(size = 11, face = "bold", hjust = 0.5),
    axis.title = ggplot2::element_text(size = 9)
  )

  # Plot counter for alphabetical labeling
  plot_count <- 0

  # Function to generate the next plot label
  next_label <- function() {
    plot_count <<- plot_count + 1
    return(LETTERS[plot_count])
  }

  # Create enhanced P-P plot
  label <- next_label()
  pp_plot <- ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = p_theoretical, y = p_empirical), size = 2, alpha = 0.7) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", linewidth = 1) +
    ggplot2::labs(
      title = paste0("(", label, ") P-P Plot"),
      x = "Theoretical CDF",
      y = "Empirical CDF"
    ) +
    theme +
    title_theme +
    ggplot2::coord_fixed(ratio = 1, xlim = c(0, 1), ylim = c(0, 1))

  plots$pp_plot_extended <- pp_plot

  # Create enhanced Q-Q plot
  label <- next_label()
  theoretical_quantiles <- .calculate_theoretical_quantiles(p_empirical, family, params)

  qq_plot <- ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = theoretical_quantiles, y = sorted_data), size = 2, alpha = 0.7) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", linewidth = 1) +
    ggplot2::labs(
      title = paste0("(", label, ") Q-Q Plot"),
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    theme +
    title_theme

  plots$qq_plot_extended <- qq_plot

  # Create PDF comparison plot
  label <- next_label()
  grid_points <- seq(0.001, 0.999, length.out = 100)
  theoretical_pdf <- .calculate_theoretical_pdf(grid_points, family, params)

  # Prepare histogram with density
  hist_data <- graphics::hist(data, breaks = "FD", plot = FALSE)
  bins <- length(hist_data$breaks) - 1
  bin_width <- hist_data$breaks[2] - hist_data$breaks[1]

  # Calculate density values for histogram scaling
  density_scaling <- length(data) * bin_width

  # Create density histogram data frame
  hist_df <- data.frame(
    x = hist_data$mids,
    y = hist_data$counts / density_scaling,
    Type = rep("Empirical", length(hist_data$mids))
  )

  # Create theoretical PDF data frame
  pdf_df <- data.frame(
    x = grid_points,
    y = theoretical_pdf,
    Type = rep("Theoretical", length(grid_points))
  )

  # Create PDF comparison plot
  pdf_plot <- ggplot2::ggplot() +
    ggplot2::geom_histogram(
      data = data.frame(x = data),
      ggplot2::aes(x = x, y = ggplot2::after_stat(density)),
      bins = bins,
      fill = "lightblue",
      alpha = 0.5,
      color = "darkblue"
    ) +
    ggplot2::geom_line(
      data = pdf_df,
      ggplot2::aes(x = x, y = y),
      color = "red",
      linewidth = 1
    ) +
    ggplot2::labs(
      title = paste0("(", label, ") PDF Empirical vs Theoretical"),
      x = "x",
      y = "Density"
    ) +
    theme +
    title_theme

  plots$pdf_comparison <- pdf_plot

  # Create CDF comparison plot
  label <- next_label()
  theoretical_cdf <- .calculate_theoretical_cdf(grid_points, family, params)

  # Create empirical CDF function using the ecdf function
  emp_cdf_fn <- stats::ecdf(data)
  empirical_cdf <- emp_cdf_fn(grid_points)

  # Prepare data for CDF plot
  cdf_data_theoretical <- data.frame(
    x = grid_points,
    y = theoretical_cdf,
    Type = rep("Theoretical", length(grid_points))
  )

  cdf_data_empirical <- data.frame(
    x = grid_points,
    y = empirical_cdf,
    Type = rep("Empirical", length(grid_points))
  )

  cdf_df <- rbind(cdf_data_theoretical, cdf_data_empirical)

  cdf_plot <- ggplot2::ggplot(cdf_df, ggplot2::aes(x = x, y = y, color = Type)) +
    ggplot2::geom_line(linewidth = 1) + # Using linewidth instead of size
    ggplot2::scale_color_manual(values = c("Theoretical" = "red", "Empirical" = "blue")) +
    ggplot2::labs(
      title = paste0("(", label, ") CDF Empirical vs Theoretical"),
      x = "x",
      y = "Cumulative Probability"
    ) +
    theme +
    title_theme +
    ggplot2::theme(legend.position = "none") # Remove legend as requested

  plots$cdf_comparison <- cdf_plot

  # Add bootstrap plots if bootstrap statistics are available
  if (!is.null(object$bootstrap_stats)) {
    label <- next_label()
    # Create data frame for bootstrap statistics
    bs_ks <- data.frame(
      statistic = object$bootstrap_stats$ks,
      type = "Kolmogorov-Smirnov"
    )
    bs_cvm <- data.frame(
      statistic = object$bootstrap_stats$cvm,
      type = "Cramer-von Mises"
    )
    bs_ad <- data.frame(
      statistic = object$bootstrap_stats$ad,
      type = "Anderson-Darling"
    )

    bs_data <- rbind(bs_ks, bs_cvm, bs_ad)

    # Create bootstrap plot
    bootstrap_plot <- ggplot2::ggplot(bs_data, ggplot2::aes(x = statistic)) +
      ggplot2::geom_density(ggplot2::aes(fill = type), alpha = 0.5) +
      ggplot2::geom_vline(data = data.frame(
        type = c("Kolmogorov-Smirnov", "Cramer-von Mises", "Anderson-Darling"),
        observed = c(object$distance_tests$ks, object$distance_tests$cvm, object$distance_tests$ad)
      ), ggplot2::aes(xintercept = observed), linetype = "dashed") +
      ggplot2::facet_wrap(~type, scales = "free") +
      ggplot2::labs(
        title = paste0("(", label, ") Bootstrap Distribution"),
        x = "Statistic Value",
        y = "Density"
      ) +
      theme +
      title_theme +
      ggplot2::theme(legend.position = "none")

    plots$bootstrap_plot <- bootstrap_plot
  }

  # Add profile likelihood plots if available in the original gkwfit object
  if (!is.null(object$profile) &&
    is.list(object$profile) &&
    length(object$profile) > 0) {
    for (param in names(object$profile)) {
      label <- next_label()
      prof_data <- object$profile[[param]]

      # Calculate reference line at max - qchisq(0.95, 1)/2 for 95% confidence
      ref_level <- max(prof_data$loglik, na.rm = TRUE) - stats::qchisq(0.95, 1) / 2

      # Define title and x-axis label using Greek letters when appropriate
      if (param %in% c("alpha", "beta", "gamma", "delta", "lambda")) {
        # Construct expression for title and x label
        plot_title <- bquote("(" * .(label) * ") Profile " * .(as.name(param)))
        x_label <- bquote(.(as.name(param)))
      } else {
        plot_title <- paste0("(", label, ") Profile ", param)
        x_label <- param
      }

      # Create profile likelihood plot
      profile_plot <- ggplot2::ggplot(prof_data, ggplot2::aes(x = value, y = loglik)) +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::geom_vline(
          xintercept = object$coefficients[param],
          linetype = "dashed", color = "red"
        ) +
        ggplot2::geom_hline(
          yintercept = ref_level,
          linetype = "dotted", color = "blue"
        ) +
        ggplot2::labs(
          title = plot_title,
          x = x_label, y = "Log-likelihood"
        ) +
        theme +
        title_theme

      plots[[paste0("profile_", param)]] <- profile_plot
    }
  }

  # Return all plots
  return(plots)
}


# .generate_additional_plots <- function(object, data, p_empirical, p_theoretical, theme = ggplot2::theme_classic(), ...) {
#   # Extract parameters
#   family <- object$family
#   params <- object$coefficients
#   n <- length(data)
#   sorted_data <- sort(data)
#
#   # List to store plots
#   plots <- list()
#
#   # Check if basic plots already exist in the gkwfit object
#   has_basic_plots <- !is.null(object$plots) &&
#     (is.list(object$plots) || inherits(object$plots, "ggplot"))
#
#   # Always generate P-P and Q-Q plots (regardless of whether basic ones exist)
#   # Create enhanced P-P plot
#   pp_plot <- ggplot2::ggplot() +
#     # ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
#     ggplot2::geom_point(ggplot2::aes(x = p_theoretical, y = p_empirical),size = 2, alpha = 0.7) +
#     ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
#     ggplot2::labs(
#       title = "Probability-Probability (P-P) Plot",
#       x = "Theoretical CDF",
#       y = "Empirical CDF"
#     ) +
#     # ggplot2::theme_minimal() +
#     theme +
#     ggplot2::theme(
#       plot.title = ggplot2::element_text(size = 11, face = "bold"),
#       axis.title = ggplot2::element_text(size = 9)
#     ) +
#     ggplot2::coord_fixed(ratio = 1, xlim = c(0, 1), ylim = c(0, 1))
#
#   plots$pp_plot_extended <- pp_plot
#
#   # Create enhanced Q-Q plot
#   theoretical_quantiles <- .calculate_theoretical_quantiles(p_empirical, family, params)
#
#   qq_plot <- ggplot2::ggplot() +
#     # ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
#     ggplot2::geom_point(ggplot2::aes(x = theoretical_quantiles, y = sorted_data), size = 2, alpha = 0.7) +
#     ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
#     ggplot2::labs(
#       title = "Quantile-Quantile (Q-Q) Plot",
#       x = "Theoretical Quantiles",
#       y = "Sample Quantiles"
#     ) +
#     # ggplot2::theme_minimal() +
#     theme +
#     ggplot2::theme(
#       plot.title = ggplot2::element_text(size = 11, face = "bold"),
#       axis.title = ggplot2::element_text(size = 9)
#     )
#
#   plots$qq_plot_extended <- qq_plot
#
#   # Always create CDF comparison plot (this is an additional plot not in gkwfit)
#   grid_points <- seq(0.001, 0.999, length.out = 100)
#   theoretical_cdf <- .calculate_theoretical_cdf(grid_points, family, params)
#
#   # Create empirical CDF function using the ecdf function
#   emp_cdf_fn <- stats::ecdf(data)
#   empirical_cdf <- emp_cdf_fn(grid_points)
#
#   # Prepare data for CDF plot
#   cdf_data_theoretical <- data.frame(
#     x = grid_points,
#     y = theoretical_cdf,
#     Type = rep("Theoretical", length(grid_points))
#   )
#
#   cdf_data_empirical <- data.frame(
#     x = grid_points,
#     y = empirical_cdf,
#     Type = rep("Empirical", length(grid_points))
#   )
#
#   cdf_df <- rbind(cdf_data_theoretical, cdf_data_empirical)
#
#   cdf_plot <- ggplot2::ggplot(cdf_df, ggplot2::aes(x = x, y = y, color = Type)) +
#     ggplot2::geom_line(linewidth = 1) +  # Using linewidth instead of size
#     ggplot2::scale_color_manual(values = c("Theoretical" = "red", "Empirical" = "blue")) +
#     ggplot2::labs(
#       title = "CDF Comparison",
#       x = "x",
#       y = "Cumulative Probability"
#     ) +
#     # ggplot2::theme_minimal() +
#     theme +
#     ggplot2::theme(
#       plot.title = ggplot2::element_text(size = 11, face = "bold"),
#       axis.title = ggplot2::element_text(size = 9),
#       legend.position = "bottom"
#     )
#
#   plots$cdf_comparison <- cdf_plot
#
#   # Always create CDF deviation plot (unique to this function)
#   deviations_df <- data.frame(
#     x = grid_points,
#     Deviation = empirical_cdf - theoretical_cdf
#   )
#
#   deviation_plot <- ggplot2::ggplot(deviations_df, ggplot2::aes(x = x, y = Deviation)) +
#     ggplot2::geom_line(linewidth = 0.8, color = "blue") +  # Using linewidth instead of size
#     ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
#     ggplot2::labs(
#       title = "CDF Deviation Plot",
#       x = "x",
#       y = "Empirical - Theoretical"
#     ) +
#     # ggplot2::theme_minimal() +
#     theme +
#     ggplot2::theme(
#       plot.title = ggplot2::element_text(size = 11, face = "bold"),
#       axis.title = ggplot2::element_text(size = 9)
#     )
#
#   plots$deviation_plot <- deviation_plot
#
#   # Create PDF comparison plot
#   grid_points <- seq(0.001, 0.999, length.out = 100)
#   theoretical_pdf <- .calculate_theoretical_pdf(grid_points, family, params)
#
#   # Prepare histogram with density
#   hist_data <- graphics::hist(data, breaks = "FD", plot = FALSE)
#   bins <- length(hist_data$breaks) - 1
#   bin_width <- hist_data$breaks[2] - hist_data$breaks[1]
#
#   # Calculate density values for histogram scaling
#   density_scaling <- length(data) * bin_width
#
#   # Create density histogram data frame
#   hist_df <- data.frame(
#     x = hist_data$mids,
#     y = hist_data$counts / density_scaling,
#     Type = rep("Empirical", length(hist_data$mids))
#   )
#
#   # Create theoretical PDF data frame
#   pdf_df <- data.frame(
#     x = grid_points,
#     y = theoretical_pdf,
#     Type = rep("Theoretical", length(grid_points))
#   )
#
#   # Combine data frames
#   combined_df <- rbind(hist_df, pdf_df)
#
#   # Create PDF comparison plot
#   pdf_plot <- ggplot2::ggplot() +
#     ggplot2::geom_histogram(data = data.frame(x = data),
#                             ggplot2::aes(x = x, y = after_stat(density)),
#                             bins = bins,
#                             fill = "lightblue",
#                             alpha = 0.5,
#                             color = "darkblue") +
#     ggplot2::geom_line(data = pdf_df,
#                        ggplot2::aes(x = x, y = y),
#                        color = "red",
#                        linewidth = 1) +
#     ggplot2::labs(
#       title = "PDF Comparison",
#       x = "x",
#       y = "Density"
#     ) +
#     # ggplot2::theme_minimal() +
#     theme +
#     ggplot2::theme(
#       plot.title = ggplot2::element_text(size = 11, face = "bold"),
#       axis.title = ggplot2::element_text(size = 9)
#     )
#
#   plots$pdf_comparison <- pdf_plot
#
#   # Add bootstrap plots if bootstrap statistics are available
#   if (!is.null(object$bootstrap_stats)) {
#     # Create data frame for bootstrap statistics
#     bs_ks <- data.frame(
#       statistic = object$bootstrap_stats$ks,
#       type = "Kolmogorov-Smirnov"
#     )
#     bs_cvm <- data.frame(
#       statistic = object$bootstrap_stats$cvm,
#       type = "Cramer-von Mises"
#     )
#     bs_ad <- data.frame(
#       statistic = object$bootstrap_stats$ad,
#       type = "Anderson-Darling"
#     )
#
#     bs_data <- rbind(bs_ks, bs_cvm, bs_ad)
#
#     # Create bootstrap plot
#     bootstrap_plot <- ggplot2::ggplot(bs_data, ggplot2::aes(x = statistic)) +
#       ggplot2::geom_density(ggplot2::aes(fill = type), alpha = 0.5) +
#       ggplot2::geom_vline(data = data.frame(
#         type = c("Kolmogorov-Smirnov", "Cramer-von Mises", "Anderson-Darling"),
#         observed = c(object$distance_tests$ks, object$distance_tests$cvm, object$distance_tests$ad)
#       ), ggplot2::aes(xintercept = observed), linetype = "dashed") +
#       ggplot2::facet_wrap(~type, scales = "free") +
#       ggplot2::labs(
#         title = "Bootstrap Distribution of Test Statistics",
#         x = "Statistic Value",
#         y = "Density"
#       ) +
#       theme +
#       # ggplot2::theme_minimal() +
#       ggplot2::theme(legend.position = "none")
#
#     plots$bootstrap_plot <- bootstrap_plot
#   }
#
#   # Add profile likelihood plots if available in the original gkwfit object
#   if (!is.null(object$profile) &&
#       is.list(object$profile) &&
#       length(object$profile) > 0) {
#
#     for (param in names(object$profile)) {
#       prof_data <- object$profile[[param]]
#
#       # Calculate reference line at max - qchisq(0.95, 1)/2 for 95% confidence
#       ref_level <- max(prof_data$loglik, na.rm = TRUE) - stats::qchisq(0.95, 1) / 2
#
#       # Create profile likelihood plot
#       profile_plot <- ggplot2::ggplot(prof_data, ggplot2::aes(x = value, y = loglik)) +
#         ggplot2::geom_line(linewidth = 1) +
#         ggplot2::geom_vline(
#           xintercept = object$coefficients[param],
#           linetype = "dashed", color = "red"
#         ) +
#         ggplot2::geom_hline(
#           yintercept = ref_level,
#           linetype = "dotted", color = "blue"
#         ) +
#         ggplot2::labs(
#           title = paste("Profile Likelihood for", param),
#           x = param, y = "Log-likelihood"
#         ) +
#         theme +
#         # ggplot2::theme_minimal() +
#         ggplot2::theme(
#           plot.title = ggplot2::element_text(size = 11, face = "bold"),
#           axis.title = ggplot2::element_text(size = 9)
#         )
#
#       plots[[paste0("profile_", param)]] <- profile_plot
#     }
#   }
#
#   # Return all plots
#   return(plots)
# }




#' Print Formatted Summary of Goodness-of-Fit Statistics
#'
#' @param results List of results from gkwgof function
#' @param verbose Logical; if TRUE, provides additional details
#' @return None (called for its side effect of printing to console)
#' @keywords internal
.print_gof_summary <- function(results, verbose) {
  # Extract key information
  family <- results$family
  n <- results$sample_size
  coefficients <- results$coefficients

  # Print header
  cat("\n")
  cat(paste(rep("=", 80), collapse = ""), "\n")
  cat(paste0("Goodness-of-Fit Analysis for ", toupper(family), " Distribution"), "\n")
  cat(paste(rep("-", 80), collapse = ""), "\n\n")

  # Print key model information
  cat("Sample Size:", n, "\n")
  cat("Parameters:\n")
  for (i in seq_along(coefficients)) {
    cat(sprintf("  %-10s = %10.6f\n", names(coefficients)[i], coefficients[i]))
  }
  cat("\n")

  # Print distance-based tests
  cat("Distance-Based Tests:\n")
  cat(sprintf("  %-20s = %10.6f\n", "Kolmogorov-Smirnov", results$distance_tests$ks))
  cat(sprintf("  %-20s = %10.6f\n", "Cramer-von Mises", results$distance_tests$cvm))
  cat(sprintf("  %-20s = %10.6f\n", "Anderson-Darling", results$distance_tests$ad))
  cat(sprintf("  %-20s = %10.6f\n", "Watson", results$distance_tests$watson))

  # Print p-values if available
  if (!is.null(results$p_values)) {
    cat("\nBootstrap P-Values (based on", length(results$bootstrap_stats$ks), "replicates):\n")
    cat(sprintf("  %-20s = %10.6f\n", "Kolmogorov-Smirnov", results$p_values$ks))
    cat(sprintf("  %-20s = %10.6f\n", "Cramer-von Mises", results$p_values$cvm))
    cat(sprintf("  %-20s = %10.6f\n", "Anderson-Darling", results$p_values$ad))
    cat(sprintf("  %-20s = %10.6f\n", "Watson", results$p_values$watson))
  }
  cat("\n")

  # Print information criteria
  cat("Information Criteria:\n")
  ic <- results$information_criteria
  cat(sprintf("  %-20s = %10.4f\n", "AIC", ic$AIC))
  cat(sprintf("  %-20s = %10.4f\n", "BIC", ic$BIC))
  cat(sprintf("  %-20s = %10.4f\n", "AICc", ic$AICc))
  cat(sprintf("  %-20s = %10.4f\n", "CAIC", ic$CAIC))
  cat(sprintf("  %-20s = %10.4f\n", "HQIC", ic$HQIC))
  cat("\n")

  # Print likelihood-based statistics
  cat("Likelihood Statistics:\n")
  ll <- results$likelihood
  cat(sprintf("  %-20s = %10.4f\n", "Log-likelihood", ll$loglik))
  cat(sprintf("  %-20s = %10.4f\n", "Log-lik. per obs.", ll$loglik_per_obs))
  cat(sprintf("  %-20s = %10.4f\n", "Pseudo-R^2", ll$pseudo_r_squared))
  cat("\n")

  # Print moment comparisons
  cat("Moment Comparisons:\n")
  mom <- results$moments
  cat(sprintf("  %-10s %10s %10s %10s\n", "Moment", "Theoretical", "Sample", "Difference"))
  cat(sprintf("  %-10s %10.6f %10.6f %10.6f\n", "Mean", mom$theoretical[1], mom$sample[1], mom$absolute_differences[1]))
  cat(sprintf("  %-10s %10.6f %10.6f %10.6f\n", "Variance", mom$theoretical[2], mom$sample[2], mom$absolute_differences[2]))
  cat(sprintf("  %-10s %10.6f %10.6f %10.6f\n", "Skewness", mom$theoretical[3], mom$sample[3], mom$absolute_differences[3]))
  cat(sprintf("  %-10s %10.6f %10.6f %10.6f\n", "Kurtosis", mom$theoretical[4], mom$sample[4], mom$absolute_differences[4]))
  cat(sprintf("  %-20s = %10.6f\n", "RMSE Moments", mom$rmse))
  cat("\n")

  # Print probability plot metrics
  cat("Probability Plot Metrics:\n")
  pp <- results$probability_plots
  cat(sprintf("  %-20s = %10.6f\n", "P-P Correlation", pp$pp_correlation))
  cat(sprintf("  %-20s = %10.6f\n", "P-P Mean Deviation", pp$pp_area))
  cat(sprintf("  %-20s = %10.6f\n", "Q-Q Correlation", pp$qq_correlation))
  cat(sprintf("  %-20s = %10.6f\n", "Q-Q Mean Abs. Error", pp$qq_mae))
  cat("\n")

  # Print prediction accuracy metrics
  cat("Prediction Accuracy Metrics:\n")
  pred <- results$prediction
  cat(sprintf("  %-20s = %10.6f\n", "MAE", pred$mae))
  cat(sprintf("  %-20s = %10.6f\n", "RMSE", pred$rmse))
  cat(sprintf("  %-20s = %10.6f\n", "CRPS", pred$crps))
  cat("\n")

  # Print additional information if verbose
  if (verbose) {
    cat("Interpretation Guide:\n")
    cat("  Distance-based Tests: Lower values indicate better fit.\n")
    cat("  Information Criteria: Lower values indicate better fit.\n")
    cat("  Pseudo-R^2: Higher values (closer to 1) indicate better fit.\n")
    cat("  Correlations: Higher values (closer to 1) indicate better fit.\n")
    cat("  P-values: Values > 0.05 suggest no significant deviation from the fitted distribution.\n")
    cat("\n")

    cat("Notes:\n")
    cat("  1. The Anderson-Darling test places more weight on the tails of the distribution.\n")
    cat("  2. For model comparison, AIC and BIC are most commonly used.\n")
    cat("  3. Moment comparisons help assess if the distribution captures key data characteristics.\n")
    cat("  4. P-P and Q-Q plots are visual tools for assessing distributional fit.\n")
    cat("\n")
  }

  cat(paste(rep("=", 80), collapse = ""), "\n")
}




#' Print Method for gkwgof Objects
#'
#' @description
#' Prints a summary of the goodness-of-fit analysis for GKw family distributions.
#'
#' @param x An object of class "gkwgof", typically the result of a call to \code{gkwgof}.
#' @param verbose Logical; if TRUE, provides additional details and explanations.
#'        Default is FALSE.
#' @param ... Additional arguments (currently unused).
#'
#' @return The input object \code{x} is returned invisibly.
#'
#' @export
print.gkwgof <- function(x, verbose = FALSE, ...) {
  # Call the internal function to print the summary
  .print_gof_summary(x, verbose)

  # Return the object invisibly
  invisible(x)
}



#' Plot Method for gkwgof Objects
#'
#' @description
#' Creates a panel of diagnostic plots for assessing the goodness-of-fit of a model
#' from the GKw family of distributions.
#'
#' @param x An object of class "gkwgof", typically the result of a call to \code{gkwgof}.
#' @param title Plot title
#' @param ncols Number of columns to draw plots in graphics window
#' @param which A numeric vector specifying which plots to display.
#'        If NULL (default), all available plots will be displayed.
#' @param ... Additional arguments to be passed to plotting functions.
#'
#' @return The input object \code{x} is returned invisibly.
#'
#' @export
plot.gkwgof <- function(x, title = NULL, ncols = 4, which = NULL, ...) {
  # Check if the object contains any plots
  if (is.null(x$plots)) {
    stop("No plots available in the 'gkwgof' object. Re-run gkwgof() with plot = TRUE.")
  }

  # Ensure that ggplot2 package is available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting. Please install it.")
  }

  # Retrieve all available plot names from the object's plots list
  available_plot_names <- names(x$plots)

  # Define the default order of standard diagnostic plots
  standard_plot_types <- c(
    "pp_plot_extended",
    "qq_plot_extended",
    "pdf_comparison",
    "cdf_comparison",
    "bootstrap_plot"
  )

  # Identify profile likelihood plots (names starting with "profile_")
  profile_plot_names <- grep("^profile_", available_plot_names, value = TRUE)

  # Order the plots: standard plots first, then profile plots
  ordered_plot_names <- c(
    intersect(standard_plot_types, available_plot_names),
    profile_plot_names
  )

  # If 'which' is NULL, use all ordered plots
  if (is.null(which)) {
    which <- seq_along(ordered_plot_names)
  }

  # Validate the 'which' indices to ensure they are within the valid range
  which <- which[which > 0 & which <= length(ordered_plot_names)]

  if (length(which) == 0) {
    message("No plots available to display. Try re-running gkwgof() with plot = TRUE to generate plots.")
    return(invisible(x))
  }

  # Select the plot names based on the 'which' indices
  selected_plot_names <- ordered_plot_names[which]

  # If patchwork is available and more than one plot is selected, combine plots
  if (requireNamespace("patchwork", quietly = TRUE) && length(selected_plot_names) > 1) {
    # Combine selected plots using patchwork::wrap_plots with a specified layout (by row, 4 columns)
    combined_plot <- patchwork::wrap_plots(
      x$plots[selected_plot_names],
      byrow = TRUE,
      ncol = ncols
    ) +
      patchwork::plot_annotation(
        title = ifelse(is.null(title),
          paste("Goodness-of-Fit Diagnostics for", toupper(x$family), "Distribution"), title
        )
      )

    # Print the combined plot
    print(combined_plot)
  } else {
    # If patchwork is not available or only one plot is selected, print each plot individually
    for (plot_name in selected_plot_names) {
      print(x$plots[[plot_name]])
    }
  }

  # Return the input object invisibly
  invisible(x)
}


# plot.gkwgof <- function(x, which = NULL, ...) {
#   # Check if the object contains plots
#   if (is.null(x$plots)) {
#     stop("No plots available in the 'gkwgof' object. Re-run gkwgof() with plot = TRUE.")
#   }
#
#   # Check if ggplot2 is available
#   if (!requireNamespace("ggplot2", quietly = TRUE)) {
#     stop("Package 'ggplot2' is required for plotting. Please install it.")
#   }
#
#   # Get all available plot names from the plots list
#   available_plot_names <- names(x$plots)
#
#   # Define the plot order priority (standard diagnostic plots first, then profiles)
#   standard_plot_types <- c(
#     "pp_plot_extended",
#     "qq_plot_extended",
#     "pdf_comparison",
#     "cdf_comparison",
#     # "deviation_plot",
#     "bootstrap_plot"
#   )
#
#   # Identify profile plots
#   profile_plot_names <- grep("^profile_", available_plot_names, value = TRUE)
#
#   # Combine standard and profile plots in the preferred order
#   ordered_plot_names <- c(
#     intersect(standard_plot_types, available_plot_names),
#     profile_plot_names
#   )
#
#   # Set default which parameter based on available plots
#   if (is.null(which)) {
#     which <- seq_along(ordered_plot_names)
#   }
#
#   # Ensure 'which' is within bounds
#   which <- which[which > 0 & which <= length(ordered_plot_names)]
#
#   # If no valid plot indices, provide a helpful message
#   if (length(which) == 0) {
#     message("No plots available to display. Try re-running gkwgof() with plot = TRUE to generate plots.")
#     return(invisible(x))
#   }
#
#   # Select the plots to display
#   selected_plot_names <- ordered_plot_names[which]
#
#   # Check if we can use patchwork for a better display
#   use_patchwork <- requireNamespace("patchwork", quietly = TRUE) && length(selected_plot_names) > 1
#
#   if (use_patchwork) {
#     # Start with the first plot
#     combined_plot <- x$plots[[selected_plot_names[1]]]
#
#     # Add remaining plots
#     if (length(selected_plot_names) > 1) {
#       for (i in 2:length(selected_plot_names)) {
#         combined_plot <- combined_plot + x$plots[[selected_plot_names[i]]]
#       }
#     }
#
#     # Set the layout - try to make it look nice
#     if (length(selected_plot_names) > 2) {
#       # Calculate a reasonable grid layout
#       n_plots <- length(x$plots)
#       n_cols <- min(3, n_plots)
#       n_rows <- ceiling(n_plots / n_cols)
#
#       # Apply the layout
#       combined_plot <- combined_plot +
#         patchwork::plot_layout(ncol = n_cols, nrow = n_rows) +
#         patchwork::plot_annotation(
#           title = paste("Goodness-of-Fit Diagnostics for", toupper(x$family), "Distribution")
#         )
#     }
#
#     patchwork::wrap_plots(combined_plot, byrow = TRUE, ncol = 4)
#
#     # Display the combined plot
#     print(combined_plot)
#   } else {
#     # Display each plot individually
#     for (plot_name in selected_plot_names) {
#       print(x$plots[[plot_name]])
#     }
#   }
#
#   # Return the object invisibly
#   invisible(x)
# }




#' Compare Goodness-of-Fit Results Across Multiple Models
#'
#' @description
#' Creates a comparison of goodness-of-fit statistics and plots across multiple models
#' from the GKw family of distributions.
#'
#' @param gof_list A named list of gkwgof objects, where names are used as model identifiers.
#' @param criteria Character vector specifying which criteria to compare. Available options are:
#'        "information" (for AIC, BIC, etc.), "distance" (for KS, CvM, AD, etc.),
#'        "prediction" (for MAE, RMSE, etc.), "probability" (for P-P, Q-Q correlations),
#'        or "all" for all criteria. Default is "all".
#' @param plot Logical; if TRUE, creates comparison plots. Default is TRUE.
#' @param plot_type Character string specifying the type of plot to create. Available options are:
#'        "radar" for a radar chart (requires the fmsb package),
#'        "bar" for bar charts, "table" for a formatted table,
#'        or "all" for all plot types. Default is "all".
#' @param ... Additional arguments to be passed to plotting functions.
#'
#' @return A list containing the comparison results and plots.
#'
#' @examples
#' \donttest{
#' # Generate sample data
#' set.seed(123)
#' data <- rkw(n = 200, alpha = 2.5, beta = 1.8)
#'
#' # Fit multiple models
#' fit_kw <- gkwfit(data, family = "kw")
#' fit_beta <- gkwfit(data, family = "beta")
#' fit_gkw <- gkwfit(data, family = "gkw")
#'
#' # Calculate goodness-of-fit statistics for each model
#' gof_kw <- gkwgof(fit_kw, print_summary = FALSE)
#' gof_beta <- gkwgof(fit_beta, print_summary = FALSE)
#' gof_gkw <- gkwgof(fit_gkw, print_summary = FALSE)
#'
#' # Compare the models
#' comparison <- plotcompare(
#'   list(KW = gof_kw, Beta = gof_beta, GKW = gof_gkw),
#'   plot_type = "all"
#' )
#' }
#'
#' @export
plotcompare <- function(gof_list, criteria = "all", plot = TRUE, plot_type = "all", ...) {
  # Check if the input is a list
  if (!is.list(gof_list)) {
    stop("Input 'gof_list' must be a list of gkwgof objects.")
  }

  # Check if all elements in the list are gkwgof objects
  if (!all(sapply(gof_list, inherits, "gkwgof"))) {
    stop("All elements in 'gof_list' must be gkwgof objects.")
  }

  # Check if the list has names
  if (is.null(names(gof_list))) {
    names(gof_list) <- paste0("Model", seq_along(gof_list))
    warning("The gof_list was unnamed. Using default names: ", paste(names(gof_list), collapse = ", "))
  }

  # Define criteria to include
  available_criteria <- c("information", "distance", "prediction", "probability")
  if ("all" %in% criteria) {
    criteria <- available_criteria
  } else {
    criteria <- match.arg(criteria, available_criteria, several.ok = TRUE)
  }

  # Create comparison data frame for information criteria
  if ("information" %in% criteria) {
    ic_data <- data.frame(
      model = names(gof_list),
      family = sapply(gof_list, function(x) x$family),
      AIC = sapply(gof_list, function(x) x$information_criteria$AIC),
      BIC = sapply(gof_list, function(x) x$information_criteria$BIC),
      AICc = sapply(gof_list, function(x) x$information_criteria$AICc),
      CAIC = sapply(gof_list, function(x) x$information_criteria$CAIC),
      HQIC = sapply(gof_list, function(x) x$information_criteria$HQIC)
    )

    # Sort by AIC
    ic_data <- ic_data[order(ic_data$AIC), ]
  } else {
    ic_data <- NULL
  }

  # Create comparison data frame for distance-based tests
  if ("distance" %in% criteria) {
    dist_data <- data.frame(
      model = names(gof_list),
      family = sapply(gof_list, function(x) x$family),
      KS = sapply(gof_list, function(x) x$distance_tests$ks),
      CvM = sapply(gof_list, function(x) x$distance_tests$cvm),
      AD = sapply(gof_list, function(x) x$distance_tests$ad),
      Watson = sapply(gof_list, function(x) x$distance_tests$watson)
    )

    # Sort by AD (often considered most powerful)
    dist_data <- dist_data[order(dist_data$AD), ]
  } else {
    dist_data <- NULL
  }

  # Create comparison data frame for prediction metrics
  if ("prediction" %in% criteria) {
    pred_data <- data.frame(
      model = names(gof_list),
      family = sapply(gof_list, function(x) x$family),
      MAE = sapply(gof_list, function(x) x$prediction$mae),
      RMSE = sapply(gof_list, function(x) x$prediction$rmse),
      CRPS = sapply(gof_list, function(x) x$prediction$crps)
    )

    # Sort by RMSE
    pred_data <- pred_data[order(pred_data$RMSE), ]
  } else {
    pred_data <- NULL
  }

  # Create comparison data frame for probability plot metrics
  if ("probability" %in% criteria) {
    prob_data <- data.frame(
      model = names(gof_list),
      family = sapply(gof_list, function(x) x$family),
      PP_Corr = sapply(gof_list, function(x) x$probability_plots$pp_correlation),
      PP_Area = sapply(gof_list, function(x) x$probability_plots$pp_area),
      QQ_Corr = sapply(gof_list, function(x) x$probability_plots$qq_correlation),
      QQ_MAE = sapply(gof_list, function(x) x$probability_plots$qq_mae)
    )

    # Sort by PP correlation (higher is better)
    prob_data <- prob_data[order(prob_data$PP_Corr, decreasing = TRUE), ]
  } else {
    prob_data <- NULL
  }

  # Combine all comparison data
  comparison_data <- list(
    information = ic_data,
    distance = dist_data,
    prediction = pred_data,
    probability = prob_data
  )

  # Create plots if requested
  comparison_plots <- list()

  if (plot) {
    plot_types <- c("radar", "bar", "table")
    if (plot_type == "all") {
      plot_type <- plot_types
    } else {
      plot_type <- match.arg(plot_type, plot_types, several.ok = TRUE)
    }

    # Check if ggplot2 is available for plotting
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      warning("Package 'ggplot2' is required for plotting. Plots will not be generated.")
    } else {
      # Create plots for each criterion and plot type
      for (criterion in criteria) {
        if (criterion == "information") {
          if ("bar" %in% plot_type) {
            # Create bar plot for information criteria
            ic_plot <- .create_bar_plot(
              ic_data, "Information Criteria (lower is better)",
              c("AIC", "BIC", "AICc"), FALSE
            )
            comparison_plots$ic_bar <- ic_plot
          }

          if ("table" %in% plot_type) {
            # Create table plot for information criteria
            ic_table <- .create_table_plot(ic_data, "Information Criteria Comparison")
            comparison_plots$ic_table <- ic_table
          }
        }

        if (criterion == "distance") {
          if ("bar" %in% plot_type) {
            # Create bar plot for distance tests
            dist_plot <- .create_bar_plot(
              dist_data, "Distance-Based Tests (lower is better)",
              c("KS", "CvM", "AD"), FALSE
            )
            comparison_plots$dist_bar <- dist_plot
          }

          if ("table" %in% plot_type) {
            # Create table plot for distance tests
            dist_table <- .create_table_plot(dist_data, "Distance-Based Tests Comparison")
            comparison_plots$dist_table <- dist_table
          }
        }

        if (criterion == "prediction") {
          if ("bar" %in% plot_type) {
            # Create bar plot for prediction metrics
            pred_plot <- .create_bar_plot(
              pred_data, "Prediction Metrics (lower is better)",
              c("MAE", "RMSE", "CRPS"), FALSE
            )
            comparison_plots$pred_bar <- pred_plot
          }

          if ("table" %in% plot_type) {
            # Create table plot for prediction metrics
            pred_table <- .create_table_plot(pred_data, "Prediction Metrics Comparison")
            comparison_plots$pred_table <- pred_table
          }
        }

        if (criterion == "probability") {
          if ("bar" %in% plot_type) {
            # Create bar plot for probability plot metrics
            prob_plot <- .create_bar_plot(
              prob_data, "Probability Plot Metrics",
              c("PP_Corr", "QQ_Corr"), TRUE
            )
            comparison_plots$prob_bar <- prob_plot
          }

          if ("table" %in% plot_type) {
            # Create table plot for probability plot metrics
            prob_table <- .create_table_plot(prob_data, "Probability Plot Metrics Comparison")
            comparison_plots$prob_table <- prob_table
          }
        }

        # Create radar plots if requested
        if ("radar" %in% plot_type) {
          radar_plot <- .create_radar_plot(gof_list, criteria)
          comparison_plots$radar <- radar_plot
        }
      }

      # Display the plots
      if (requireNamespace("gridExtra", quietly = TRUE) && length(comparison_plots) > 0) {
        # Select plots to display
        plots_to_show <- list()

        # Prioritize radar plot if available
        if (!is.null(comparison_plots$radar)) {
          plots_to_show$radar <- comparison_plots$radar
        }

        # Add bar plots if available
        bar_plots <- comparison_plots[grep("_bar$", names(comparison_plots))]
        if (length(bar_plots) > 0) {
          plots_to_show <- c(plots_to_show, bar_plots)
        }

        # Add table plots if available
        table_plots <- comparison_plots[grep("_table$", names(comparison_plots))]
        if (length(table_plots) > 0) {
          plots_to_show <- c(plots_to_show, table_plots)
        }

        # Determine layout
        n_plots <- length(plots_to_show)
        if (n_plots > 0) {
          ncols <- min(2, n_plots)
          nrows <- ceiling(n_plots / ncols)

          # Create combined plot
          grid_plot <- gridExtra::grid.arrange(
            grobs = plots_to_show,
            ncol = ncols,
            top = grid::textGrob(
              "Model Comparison Summary",
              gp = grid::gpar(fontsize = 14, fontface = "bold")
            )
          )
          print(grid_plot)
        }
      }
    }
  }

  # Return comparison data and plots
  result <- list(
    comparison_data = comparison_data,
    plots = comparison_plots
  )

  class(result) <- c("gkwgof_comparison", "list")

  return(result)
}

#' Create Bar Plot for Model Comparison
#'
#' @param data Data frame containing the comparison data
#' @param title Plot title
#' @param metrics Vector of metric names to include in the plot
#' @param higher_better Logical; if TRUE, higher values indicate better fit
#' @return A ggplot2 object
#' @keywords internal
.create_bar_plot <- function(data, title, metrics, higher_better = FALSE) {
  # Check if data is available
  if (is.null(data) || nrow(data) == 0) {
    return(NULL)
  }

  # Check if all requested metrics are in the data
  if (!all(metrics %in% names(data))) {
    available_metrics <- intersect(metrics, names(data))
    if (length(available_metrics) == 0) {
      return(NULL)
    }
    metrics <- available_metrics
  }

  # Reshape data for plotting
  plot_data <- tidyr::gather(data, "metric", "value", metrics)

  # Create labels for models
  plot_data$model_label <- paste0(plot_data$model, " (", plot_data$family, ")")

  # Create bar plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = model_label, y = value, fill = metric)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::labs(
      title = title,
      x = "Model",
      y = "Value"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 11, face = "bold"),
      axis.title = ggplot2::element_text(size = 9),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank()
    )

  # If higher is better, sort in descending order
  if (higher_better) {
    p <- p + ggplot2::coord_flip()
  } else {
    p <- p + ggplot2::coord_flip()
  }

  return(p)
}

#' Create Table Plot for Model Comparison
#'
#' @param data Data frame containing the comparison data
#' @param title Table title
#' @return A ggplot2 object
#' @keywords internal
.create_table_plot <- function(data, title) {
  # Check if data is available
  if (is.null(data) || nrow(data) == 0) {
    return(NULL)
  }

  # Format numeric columns to 4 decimal places
  numeric_cols <- sapply(data, is.numeric)
  if (any(numeric_cols)) {
    data[numeric_cols] <- round(data[numeric_cols], 4)
  }

  # Create table plot
  if (requireNamespace("gridExtra", quietly = TRUE)) {
    # Create the table grob
    table_plot <- gridExtra::tableGrob(
      data,
      rows = NULL,
      theme = gridExtra::ttheme_minimal(
        core = list(fg_params = list(hjust = 0, x = 0.1)),
        colhead = list(fg_params = list(hjust = 0, x = 0.1, fontface = "bold"))
      )
    )

    # Add title
    title_grob <- grid::textGrob(
      title,
      gp = grid::gpar(fontsize = 12, fontface = "bold"),
      just = "left",
      x = 0.05
    )

    # Combine title and table
    plot <- gridExtra::grid.arrange(
      title_grob,
      table_plot,
      heights = grid::unit(c(0.1, 0.9), "npc")
    )

    return(plot)
  } else {
    # Fall back to simple data frame
    return(data)
  }
}

#' Create Radar Plot for Model Comparison
#'
#' @param gof_list List of gkwgof objects
#' @param criteria Vector of criteria to include in the plot
#' @return A radar plot
#' @keywords internal
.create_radar_plot <- function(gof_list, criteria) {
  # Check if the fmsb package is available
  if (!requireNamespace("fmsb", quietly = TRUE)) {
    return(NULL)
  }

  # Define metrics to include in the radar plot
  metrics <- list()

  if ("information" %in% criteria) {
    metrics$AIC <- list(
      extract = function(x) x$information_criteria$AIC,
      transform = function(x) -x, # Transform so higher is better
      label = "AIC"
    )
  }

  if ("distance" %in% criteria) {
    metrics$KS <- list(
      extract = function(x) x$distance_tests$ks,
      transform = function(x) -x, # Transform so higher is better
      label = "KS"
    )
    metrics$AD <- list(
      extract = function(x) x$distance_tests$ad,
      transform = function(x) -x, # Transform so higher is better
      label = "AD"
    )
  }

  if ("prediction" %in% criteria) {
    metrics$RMSE <- list(
      extract = function(x) x$prediction$rmse,
      transform = function(x) -x, # Transform so higher is better
      label = "RMSE"
    )
  }

  if ("probability" %in% criteria) {
    metrics$PP_Corr <- list(
      extract = function(x) x$probability_plots$pp_correlation,
      transform = function(x) x, # Already higher is better
      label = "P-P Correlation"
    )
    metrics$QQ_Corr <- list(
      extract = function(x) x$probability_plots$qq_correlation,
      transform = function(x) x, # Already higher is better
      label = "Q-Q Correlation"
    )
  }

  # Extract and transform metrics for each model
  radar_data <- data.frame(
    model = names(gof_list),
    stringsAsFactors = FALSE
  )

  for (metric_name in names(metrics)) {
    metric <- metrics[[metric_name]]
    values <- sapply(gof_list, function(x) {
      raw_value <- metric$extract(x)
      metric$transform(raw_value)
    })
    radar_data[[metric_name]] <- values
  }

  # Prepare data for radar plot
  radar_matrix <- as.matrix(radar_data[, -1])
  rownames(radar_matrix) <- radar_data$model

  # Add max and min rows required by fmsb
  radar_matrix <- rbind(
    apply(radar_matrix, 2, max),
    apply(radar_matrix, 2, min),
    radar_matrix
  )

  # Create radar plot
  old_par <- par(mar = c(1, 1, 3, 1))
  on.exit(par(old_par))

  radar_colors <- grDevices::rainbow(nrow(radar_matrix) - 2)

  fmsb::radarchart(
    radar_matrix,
    pcol = radar_colors,
    pfcol = scales::alpha(radar_colors, 0.3),
    plwd = 2,
    plty = 1,
    cglcol = "grey",
    cglty = 1,
    axislabcol = "grey20",
    caxislabels = seq(0, 1, 0.25),
    axistype = 1,
    title = "Model Comparison (higher is better)"
  )

  # Add legend
  legend(
    "topright",
    legend = rownames(radar_matrix)[-c(1, 2)],
    col = radar_colors,
    lty = 1,
    lwd = 2,
    bty = "n",
    cex = 0.8
  )

  # Return invisible NULL as the plot is already displayed
  return(invisible(NULL))
}




#' Extract Key Statistics from gkwgof Objects
#'
#' @description
#' Extracts the most important goodness-of-fit statistics from one or more gkwgof objects
#' into a concise data frame format for easy comparison and reporting.
#'
#' @param ... One or more objects of class "gkwgof", or a list of such objects.
#' @param statistics Character vector specifying which statistics to include. Available options are:
#'        "all" (default), "information" (for AIC, BIC), "distance" (for KS, AD),
#'        "correlation" (for P-P, Q-Q correlations), or "prediction" (for RMSE, MAE).
#'
#' @return A data frame containing the requested statistics for each gkwgof object.
#'
#' @examples
#' \donttest{
#' # Generate sample data
#' set.seed(123)
#' data <- rkw(n = 200, alpha = 2.5, beta = 1.8)
#'
#' # Fit multiple models
#' fit_kw <- gkwfit(data, family = "kw")
#' fit_beta <- gkwfit(data, family = "beta")
#'
#' # Calculate goodness-of-fit statistics for each model
#' gof_kw <- gkwgof(fit_kw, print_summary = FALSE)
#' gof_beta <- gkwgof(fit_beta, print_summary = FALSE)
#'
#' # Extract key statistics
#' summary_stats <- extract_gof_stats(gof_kw, gof_beta)
#' print(summary_stats)
#'
#' # Extract only information criteria
#' ic_stats <- extract_gof_stats(gof_kw, gof_beta, statistics = "information")
#' print(ic_stats)
#' }
#'
#' @export
extract_gof_stats <- function(..., statistics = "all") {
  # Process input arguments
  args <- list(...)

  # If first argument is a list, use that instead
  if (length(args) == 1 && is.list(args[[1]]) && !inherits(args[[1]], "gkwgof")) {
    gof_objects <- args[[1]]
  } else {
    gof_objects <- args
  }

  # Check if all elements are gkwgof objects
  if (!all(sapply(gof_objects, inherits, "gkwgof"))) {
    stop("All inputs must be gkwgof objects.")
  }

  # Determine names for the objects
  if (is.null(names(gof_objects))) {
    if (length(gof_objects) == 1) {
      names(gof_objects) <- gof_objects[[1]]$family
    } else {
      # Try to extract names from calling expression
      call_names <- as.character(match.call(expand.dots = TRUE)[-1])
      call_names <- call_names[call_names != "statistics"]

      if (length(call_names) == length(gof_objects)) {
        names(gof_objects) <- call_names
      } else {
        # Use default names
        names(gof_objects) <- paste0("Model", seq_along(gof_objects))
      }
    }
  }

  # Define statistics to include
  available_statistics <- c("information", "distance", "correlation", "prediction")
  if ("all" %in% statistics) {
    statistics <- available_statistics
  } else {
    statistics <- match.arg(statistics, c(available_statistics, "all"), several.ok = TRUE)
  }

  # Extract statistics for each object
  result <- data.frame(
    model = names(gof_objects),
    family = sapply(gof_objects, function(x) x$family),
    n_params = sapply(gof_objects, function(x) length(x$coefficients)),
    sample_size = sapply(gof_objects, function(x) x$sample_size),
    stringsAsFactors = FALSE
  )

  # Add information criteria if requested
  if ("information" %in% statistics) {
    result$AIC <- sapply(gof_objects, function(x) x$information_criteria$AIC)
    result$BIC <- sapply(gof_objects, function(x) x$information_criteria$BIC)
    result$AICc <- sapply(gof_objects, function(x) x$information_criteria$AICc)
  }

  # Add distance-based tests if requested
  if ("distance" %in% statistics) {
    result$KS <- sapply(gof_objects, function(x) x$distance_tests$ks)
    result$AD <- sapply(gof_objects, function(x) x$distance_tests$ad)
    result$CvM <- sapply(gof_objects, function(x) x$distance_tests$cvm)
  }

  # Add correlation measures if requested
  if ("correlation" %in% statistics) {
    result$PP_Corr <- sapply(gof_objects, function(x) x$probability_plots$pp_correlation)
    result$QQ_Corr <- sapply(gof_objects, function(x) x$probability_plots$qq_correlation)
  }

  # Add prediction metrics if requested
  if ("prediction" %in% statistics) {
    result$RMSE <- sapply(gof_objects, function(x) x$prediction$rmse)
    result$MAE <- sapply(gof_objects, function(x) x$prediction$mae)
    result$CRPS <- sapply(gof_objects, function(x) x$prediction$crps)
  }

  # Add likelihood-based measures
  result$logLik <- sapply(gof_objects, function(x) x$likelihood$loglik)
  result$pseudo_R2 <- sapply(gof_objects, function(x) x$likelihood$pseudo_r_squared)

  # Round numeric columns to 4 decimal places
  numeric_cols <- sapply(result, is.numeric)
  result[numeric_cols] <- round(result[numeric_cols], 4)

  return(result)
}

#' Summary Method for gkwgof Objects
#'
#' @description
#' Provides a concise summary of the goodness-of-fit analysis results.
#'
#' @param object An object of class "gkwgof".
#' @param ... Additional arguments (not used).
#'
#' @return A list containing key summary statistics.
#'
#' @export
summary.gkwgof <- function(object, ...) {
  # Create a named list of key statistics
  summary_stats <- list(
    family = object$family,
    sample_size = object$sample_size,
    parameters = object$coefficients,

    # Key fit statistics
    distance_tests = list(
      ks = object$distance_tests$ks,
      ad = object$distance_tests$ad
    ),
    information_criteria = list(
      AIC = object$information_criteria$AIC,
      BIC = object$information_criteria$BIC
    ),
    likelihood = list(
      loglik = object$likelihood$loglik,
      pseudo_r_squared = object$likelihood$pseudo_r_squared
    ),
    probability_metrics = list(
      pp_correlation = object$probability_plots$pp_correlation,
      qq_correlation = object$probability_plots$qq_correlation
    ),
    moments = list(
      theoretical = object$moments$theoretical,
      sample = object$moments$sample,
      rmse = object$moments$rmse
    )
  )

  # Add p-values if available
  if (!is.null(object$p_values)) {
    summary_stats$p_values <- object$p_values
  }

  class(summary_stats) <- "summary.gkwgof"
  return(summary_stats)
}

#' Print Method for summary.gkwgof Objects
#'
#' @description
#' Prints a formatted summary of goodness-of-fit results.
#'
#' @param x An object of class "summary.gkwgof".
#' @param ... Additional arguments (not used).
#'
#' @return The input object invisibly.
#'
#' @export
print.summary.gkwgof <- function(x, ...) {
  # Print header
  cat("\n")
  cat("Summary of Goodness-of-Fit Analysis\n")
  cat("----------------------------------\n\n")

  # Distribution family and sample size
  cat(sprintf("Distribution Family: %s\n", toupper(x$family)))
  cat(sprintf("Sample Size: %d\n\n", x$sample_size))

  # Parameters
  cat("Estimated Parameters:\n")
  for (i in seq_along(x$parameters)) {
    cat(sprintf("  %-10s = %10.6f\n", names(x$parameters)[i], x$parameters[i]))
  }
  cat("\n")

  # Key statistics table
  cat("Key Goodness-of-Fit Statistics:\n")
  cat(sprintf("  %-25s = %10.6f\n", "Log-likelihood", x$likelihood$loglik))
  cat(sprintf("  %-25s = %10.6f\n", "AIC", x$information_criteria$AIC))
  cat(sprintf("  %-25s = %10.6f\n", "BIC", x$information_criteria$BIC))
  cat(sprintf("  %-25s = %10.6f\n", "Kolmogorov-Smirnov", x$distance_tests$ks))
  cat(sprintf("  %-25s = %10.6f\n", "Anderson-Darling", x$distance_tests$ad))
  cat(sprintf("  %-25s = %10.6f\n", "P-P Plot Correlation", x$probability_metrics$pp_correlation))
  cat(sprintf("  %-25s = %10.6f\n", "Pseudo-R^2", x$likelihood$pseudo_r_squared))

  # P-values if available
  if (!is.null(x$p_values)) {
    cat("\nBootstrap P-Values:\n")
    cat(sprintf("  %-25s = %10.6f\n", "Kolmogorov-Smirnov", x$p_values$ks))
    cat(sprintf("  %-25s = %10.6f\n", "Anderson-Darling", x$p_values$ad))
  }

  cat("\n")

  # Return invisibly
  invisible(x)
}
