#' Fit All or Selected Generalized Kumaraswamy Family Distributions and Compare Them
#'
#' @description
#' Fits all seven or a user-specified subset of distributions from the Generalized
#' Kumaraswamy (GKw) family to data using maximum likelihood estimation through
#' Template Model Builder (TMB). It provides a comprehensive comparison of fit quality
#' across the selected families through statistics and visualization.
#'
#' @param data A numeric vector with values strictly between 0 and 1.
#' @param family A character vector specifying which families to fit. Options are
#'   "gkw", "bkw", "kkw", "ekw", "mc", "kw", and "beta". If \code{NULL} (default),
#'   all seven distributions will be fitted.
#' @param method Optimization method to use. One of: \code{"nlminb"} (default), \code{"Nelder-Mead"},
#'   \code{"BFGS"}, \code{"CG"}, \code{"L-BFGS-B"} or \code{"SANN"}.
#' @param use_moments Logical; if \code{TRUE}, attempts to use method of moments estimates
#'   as initial values. Default: \code{FALSE}.
#' @param profile Logical; if \code{TRUE}, computes likelihood profiles for parameters. Default: \code{TRUE}.
#' @param npoints Integer; number of points to use in profile likelihood calculations. Default: 20.
#' @param plot Logical; if \code{TRUE}, generates comparison plots. Default: \code{TRUE}.
#' @param optimizer.control List of control parameters passed to the chosen optimizer. Default: \code{list()}.
#' @param gof_tests Character vector specifying which goodness-of-fit tests to perform.
#'   Options are "ks" (Kolmogorov-Smirnov), "ad" (Anderson-Darling), and "cvm" (Cramer-von Mises).
#'   Default: c("ks", "ad", "cvm").
#' @param theme_fn Function to apply a custom theme to plots. Default: \code{ggplot2::theme_minimal}.
#' @param export_report Logical; if \code{TRUE}, generates an R Markdown report summarizing results.
#'   Default: \code{FALSE}.
#' @param report_file Character; file path for the R Markdown report if \code{export_report = TRUE}.
#'   Default: "gkw_comparison_report.html".
#'
#' @details
#' This function fits all seven distributions in the GKw family:
#' \itemize{
#'   \item \strong{GKw}: 5 parameters (\eqn{\alpha, \beta, \gamma, \delta, \lambda}) - All positive.
#'   \item \strong{BKw}: 4 parameters (\eqn{\alpha, \beta, \gamma, \delta}), \eqn{\lambda = 1} fixed - All positive.
#'   \item \strong{KKw}: 4 parameters (\eqn{\alpha, \beta, \delta, \lambda}), \eqn{\gamma = 1} fixed - All positive.
#'   \item \strong{EKw}: 3 parameters (\eqn{\alpha, \beta, \lambda}), \eqn{\gamma = 1, \delta = 0} fixed - All positive.
#'   \item \strong{Mc} (McDonald / Beta Power): 3 parameters (\eqn{\gamma, \delta, \lambda}), \eqn{\alpha = 1, \beta = 1} fixed - All positive.
#'   \item \strong{Kw} (Kumaraswamy): 2 parameters (\eqn{\alpha, \beta}), \eqn{\gamma = 1, \delta = 0, \lambda = 1} fixed - All positive.
#'   \item \strong{Beta}: 2 parameters (\eqn{\gamma, \delta}), \eqn{\alpha = 1, \beta = 1, \lambda = 1} fixed - All positive.
#' }
#'
#' The function generates comparison statistics including AIC, BIC, AICc, log-likelihood values,
#' and various goodness-of-fit measures. It also produces visualizations with all fitted
#' distributions overlaid on diagnostic plots.
#'
#' @return A list containing:
#' \item{fits}{A list of \code{gkwfit} objects for all fitted distribution families.}
#' \item{comparison}{A data frame with comparison statistics (AIC, BIC, log-likelihood, etc.) for all models.}
#' \item{plots}{A ggplot2 object with diagnostic plots for all models if \code{plot = TRUE}.}
#' \item{metrics}{A list with additional comparative metrics including RMSE and MAE.}
#'
#' @examples
#' \donttest{
#' # Generate a sample dataset (n = 1000)
#' set.seed(123)
#' n <- 1000
#'
#' # Create predictors
#' x1 <- runif(n, -2, 2)
#' x2 <- rnorm(n)
#' x3 <- factor(rbinom(n, 1, 0.4))
#'
#' # Simulate Kumaraswamy distributed data
#' # True parameters with specific relationships to predictors
#' true_alpha <- exp(0.7 + 0.3 * x1)
#' true_beta <- exp(1.2 - 0.2 * x2 + 0.4 * (x3 == "1"))
#'
#' # Generate random responses (assuming rkw function is available)
#' # If not available, use the beta distribution as an approximation
#'
#' y <- rkw(n, alpha = true_alpha, beta = true_beta)
#'
#' # Create data frame
#' df <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)
#'
#' # Split into training and test sets
#' set.seed(456)
#' train_idx <- sample(n, 800)
#' train_data <- df[train_idx, ]
#' test_data <- df[-train_idx, ]
#'
#' # ====================================================================
#' # Example 1: Basic usage - Fit a Kumaraswamy model and make predictions
#' # ====================================================================
#'
#' # Fit the model
#' kw_model <- gkwreg(y ~ x1 | x2 + x3, data = train_data, family = "kw")
#'
#' # Predict mean response for test data
#' pred_mean <- predict(kw_model, newdata = test_data, type = "response")
#'
#' # Calculate prediction error
#' mse <- mean((test_data$y - pred_mean)^2)
#' cat("Mean Squared Error:", mse, "\n")
#'
#' # ====================================================================
#' # Example 2: Different prediction types
#' # ====================================================================
#'
#' # Create a grid of values for visualization
#' x1_grid <- seq(-2, 2, length.out = 100)
#' grid_data <- data.frame(x1 = x1_grid, x2 = 0, x3 = 0)
#'
#' # Predict different quantities
#' pred_mean <- predict(kw_model, newdata = grid_data, type = "response")
#' pred_var <- predict(kw_model, newdata = grid_data, type = "variance")
#' pred_params <- predict(kw_model, newdata = grid_data, type = "parameter")
#' pred_alpha <- predict(kw_model, newdata = grid_data, type = "alpha")
#' pred_beta <- predict(kw_model, newdata = grid_data, type = "beta")
#'
#' # Plot predicted mean and parameters against x1
#' plot(x1_grid, pred_mean,
#'   type = "l", col = "blue",
#'   xlab = "x1", ylab = "Predicted Mean", main = "Mean Response vs x1"
#' )
#' plot(x1_grid, pred_var,
#'   type = "l", col = "red",
#'   xlab = "x1", ylab = "Predicted Variance", main = "Response Variance vs x1"
#' )
#' plot(x1_grid, pred_alpha,
#'   type = "l", col = "purple",
#'   xlab = "x1", ylab = "Alpha", main = "Alpha Parameter vs x1"
#' )
#' plot(x1_grid, pred_beta,
#'   type = "l", col = "green",
#'   xlab = "x1", ylab = "Beta", main = "Beta Parameter vs x1"
#' )
#'
#' # ====================================================================
#' # Example 3: Computing densities, CDFs, and quantiles
#' # ====================================================================
#'
#' # Select a single observation
#' obs_data <- test_data[1, ]
#'
#' # Create a sequence of y values for plotting
#' y_seq <- seq(0.01, 0.99, length.out = 100)
#'
#' # Compute density at each y value
#' dens_values <- predict(kw_model,
#'   newdata = obs_data,
#'   type = "density", at = y_seq, elementwise = FALSE
#' )
#'
#' # Compute CDF at each y value
#' cdf_values <- predict(kw_model,
#'   newdata = obs_data,
#'   type = "probability", at = y_seq, elementwise = FALSE
#' )
#'
#' # Compute quantiles for a sequence of probabilities
#' prob_seq <- seq(0.1, 0.9, by = 0.1)
#' quant_values <- predict(kw_model,
#'   newdata = obs_data,
#'   type = "quantile", at = prob_seq, elementwise = FALSE
#' )
#'
#' # Plot density and CDF
#' plot(y_seq, dens_values,
#'   type = "l", col = "blue",
#'   xlab = "y", ylab = "Density", main = "Predicted PDF"
#' )
#' plot(y_seq, cdf_values,
#'   type = "l", col = "red",
#'   xlab = "y", ylab = "Cumulative Probability", main = "Predicted CDF"
#' )
#'
#' # ====================================================================
#' # Example 4: Prediction under different distributional assumptions
#' # ====================================================================
#'
#' # Fit models with different families
#' beta_model <- gkwreg(y ~ x1 | x2 + x3, data = train_data, family = "beta")
#' gkw_model <- gkwreg(y ~ x1 | x2 + x3 | 1 | 1 | x3, data = train_data, family = "gkw")
#'
#' # Predict means using different families
#' pred_kw <- predict(kw_model, newdata = test_data, type = "response")
#' pred_beta <- predict(beta_model, newdata = test_data, type = "response")
#' pred_gkw <- predict(gkw_model, newdata = test_data, type = "response")
#'
#' # Calculate MSE for each family
#' mse_kw <- mean((test_data$y - pred_kw)^2)
#' mse_beta <- mean((test_data$y - pred_beta)^2)
#' mse_gkw <- mean((test_data$y - pred_gkw)^2)
#'
#' cat("MSE by family:\n")
#' cat("Kumaraswamy:", mse_kw, "\n")
#' cat("Beta:", mse_beta, "\n")
#' cat("GKw:", mse_gkw, "\n")
#'
#' # Compare predictions from different families visually
#' plot(test_data$y, pred_kw,
#'   col = "blue", pch = 16,
#'   xlab = "Observed", ylab = "Predicted", main = "Predicted vs Observed"
#' )
#' points(test_data$y, pred_beta, col = "red", pch = 17)
#' points(test_data$y, pred_gkw, col = "green", pch = 18)
#' abline(0, 1, lty = 2)
#' legend("topleft",
#'   legend = c("Kumaraswamy", "Beta", "GKw"),
#'   col = c("blue", "red", "green"), pch = c(16, 17, 18)
#' )
#'
#' # Compare models using AIC
#' # Note: AIC is applied to each model individually, not as a list
#' aic_values <- c(
#'   KW = AIC(kw_model),
#'   Beta = AIC(beta_model),
#'   GKw = AIC(gkw_model)
#' )
#'
#' # Compare models using BIC
#' bic_values <- c(
#'   KW = BIC(kw_model),
#'   Beta = BIC(beta_model),
#'   GKw = BIC(gkw_model)
#' )
#'
#' # Display model comparison results
#' model_comparison <- data.frame(
#'   Family = c("KW", "Beta", "GKw"),
#'   AIC = aic_values,
#'   BIC = bic_values,
#'   MSE = c(mse_kw, mse_beta, mse_gkw)
#' )
#' print(model_comparison[order(model_comparison$AIC), ])
#'
#' # ====================================================================
#' # Example 5: Working with linear predictors and link functions
#' # ====================================================================
#'
#' # Extract linear predictors and parameter values
#' lp <- predict(kw_model, newdata = test_data, type = "link")
#' params <- predict(kw_model, newdata = test_data, type = "parameter")
#'
#' # Verify that inverse link transformation works correctly
#' # For Kumaraswamy model, alpha and beta use log links by default
#' alpha_from_lp <- exp(lp$alpha)
#' beta_from_lp <- exp(lp$beta)
#'
#' # Compare with direct parameter predictions
#' cat("Manual inverse link vs direct parameter prediction:\n")
#' cat("Alpha difference:", max(abs(alpha_from_lp - params$alpha)), "\n")
#' cat("Beta difference:", max(abs(beta_from_lp - params$beta)), "\n")
#'
#' # ====================================================================
#' # Example 6: Elementwise calculations
#' # ====================================================================
#'
#' # Generate probabilities specific to each observation
#' probs <- runif(nrow(test_data), 0.1, 0.9)
#'
#' # Calculate quantiles for each observation at its own probability level
#' quant_elementwise <- predict(kw_model,
#'   newdata = test_data,
#'   type = "quantile", at = probs, elementwise = TRUE
#' )
#'
#' # Calculate probabilities at each observation's actual value
#' prob_at_y <- predict(kw_model,
#'   newdata = test_data,
#'   type = "probability", at = test_data$y, elementwise = TRUE
#' )
#'
#' # Create Q-Q plot
#' plot(sort(prob_at_y), seq(0, 1, length.out = length(prob_at_y)),
#'   xlab = "Empirical Probability", ylab = "Theoretical Probability",
#'   main = "P-P Plot", type = "l"
#' )
#' abline(0, 1, lty = 2, col = "red")
#'
#' # ====================================================================
#' # Example 7: Predicting for the original data
#' # ====================================================================
#'
#' # Fit a model with original data
#' full_model <- gkwreg(y ~ x1 + x2 + x3 | x1 + x2 + x3, data = df, family = "kw")
#'
#' # Get fitted values using predict and compare with model's fitted.values
#' fitted_from_predict <- predict(full_model, type = "response")
#' fitted_from_model <- full_model$fitted.values
#'
#' # Compare results
#' cat(
#'   "Max difference between predict() and fitted.values:",
#'   max(abs(fitted_from_predict - fitted_from_model)), "\n"
#' )
#'
#' # ====================================================================
#' # Example 8: Handling missing data
#' # ====================================================================
#'
#' # Create test data with some missing values
#' test_missing <- test_data
#' test_missing$x1[1:5] <- NA
#' test_missing$x2[6:10] <- NA
#'
#' # Predict with different na.action options
#' pred_na_pass <- try(
#'   predict(kw_model, newdata = test_missing, na.action = na.pass),
#'   silent = TRUE
#' )
#'
#' if (!inherits(pred_na_pass, "try-error")) {
#'   # Show which positions have NAs
#'   cat("Rows with missing predictors:", which(is.na(pred_na_pass)), "\n")
#' } else {
#'   cat("na.pass resulted in an error. This is expected if the model requires complete cases.\n")
#' }
#'
#' # Try with na.omit
#' pred_na_omit <- try(
#'   predict(kw_model, newdata = test_missing, na.action = na.omit),
#'   silent = TRUE
#' )
#'
#' if (!inherits(pred_na_omit, "try-error")) {
#'   cat("Length after na.omit:", length(pred_na_omit), "\n")
#'   cat("(Original data had", nrow(test_missing), "rows)\n")
#' } else {
#'   cat("na.omit resulted in an error. Check model implementation.\n")
#' }
#'
#' # ====================================================================
#' # Example 9: Simulating different distribution families
#' # ====================================================================
#'
#' # Simulate data from different distributions
#' # Beta data
#' set.seed(123)
#' gamma_param <- 2
#' delta_param <- 5
#' y_beta <- rbeta_(1000, gamma_param, delta_param)
#'
#' # Kumaraswamy data
#' set.seed(123)
#' alpha_param <- 1.5
#' beta_param <- 3.0
#' y_kw <- rkw(1000, alpha = alpha_param, beta = beta_param)
#'
#' # Basic GKw data (using one approach if rgkw not available)
#' set.seed(123)
#' y_gkw <- rgkw(1000, alpha = 1.2, beta = 2.5, gamma = 1.8, delta = 0.6, lambda = 1.2)
#'
#' # Create data frames with just the response
#' df_beta <- data.frame(y = y_beta)
#' df_kw <- data.frame(y = y_kw)
#' df_gkw <- data.frame(y = y_gkw)
#'
#' # Fit models to each dataset
#' model_beta <- gkwreg(y ~ 1, data = df_beta, family = "beta")
#' model_kw <- gkwreg(y ~ 1, data = df_kw, family = "kw")
#' model_gkw <- gkwreg(y ~ 1, data = df_gkw, family = "gkw")
#'
#' # Use predict to get densities at a grid of points
#' eval_points <- seq(0.01, 0.99, length.out = 100)
#'
#' # Get densities
#' dens_beta <- predict(model_beta, type = "density", at = eval_points)
#' dens_kw <- predict(model_kw, type = "density", at = eval_points)
#' dens_gkw <- predict(model_gkw, type = "density", at = eval_points)
#'
#' # Plot density comparisons
#' plot(eval_points, as.numeric(unique(dens_beta)),
#'   type = "l", col = "red",
#'   xlab = "y", ylab = "Density",
#'   main = "Fitted Density Functions for Different Distributions"
#' )
#' lines(eval_points, as.numeric(unique(dens_kw)), col = "blue", lty = 2)
#' lines(eval_points, as.numeric(unique(dens_gkw)), col = "green", lty = 3)
#' legend("topright",
#'   legend = c("Beta", "Kumaraswamy", "GKw"),
#'   col = c("red", "blue", "green"), lty = 1:3
#' )
#'
#' # ====================================================================
#' # Example 10: Nested model comparison with different families
#' # ====================================================================
#'
#' # Simulate data from GKw distribution
#' set.seed(123)
#' n <- 1000
#' x1 <- runif(n, -1, 1)
#'
#' # First, simulate from a model with covariate effects
#' alpha <- exp(0.5 + 0.2 * x1)
#' beta <- exp(0.8 - 0.3 * x1)
#' gamma <- rep(1, n) # fixed (for Kw and KKw)
#' delta <- rep(0, n) # fixed (for Kw and EKw)
#' lambda <- rep(1, n) # fixed (for Kw and BKw)
#'
#' # Generate Kumaraswamy data directly (since we fixed gamma=1, delta=0, lambda=1)
#' y <- rkw(n, alpha = alpha, beta = beta)
#'
#' # Create data frame
#' sim_df <- data.frame(y = y, x1 = x1)
#'
#' # Fit different family models
#' kw_model <- gkwreg(y ~ x1 | x1, data = sim_df, family = "kw")
#' beta_model <- gkwreg(y ~ x1 | x1, data = sim_df, family = "beta")
#' gkw_model <- gkwreg(y ~ x1 | x1 | 1 | 1 | 1, data = sim_df, family = "gkw")
#'
#' # Compare AIC of the models (individually, not as a list)
#' aic_kw <- AIC(kw_model)
#' aic_beta <- AIC(beta_model)
#' aic_gkw <- AIC(gkw_model)
#'
#' # Compare BIC of the models (individually, not as a list)
#' bic_kw <- BIC(kw_model)
#' bic_beta <- BIC(beta_model)
#' bic_gkw <- BIC(gkw_model)
#'
#' # Create comparison table
#' model_comparison <- data.frame(
#'   Family = c("KW", "Beta", "GKw"),
#'   LogLik = c(logLik(kw_model), logLik(beta_model), logLik(gkw_model)),
#'   AIC = c(aic_kw, aic_beta, aic_gkw),
#'   BIC = c(bic_kw, bic_beta, bic_gkw)
#' )
#'
#' # Display comparison sorted by AIC
#' print(model_comparison[order(model_comparison$AIC), ])
#'
#' # Perform likelihood ratio test for nested models
#' # Function to perform LRT
#' lr_test <- function(full_model, reduced_model, df_diff) {
#'   lr_stat <- 2 * (as.numeric(logLik(full_model)) - as.numeric(logLik(reduced_model)))
#'   p_value <- 1 - pchisq(lr_stat, df = df_diff)
#'   return(c(statistic = lr_stat, p_value = p_value))
#' }
#'
#' # Test if GKw is significantly better than Kw
#' # GKw has 3 more parameters: gamma, delta, lambda
#' lrt_gkw_kw <- lr_test(gkw_model, kw_model, df_diff = 3)
#'
#' # Display LRT results
#' cat("\nLikelihood Ratio Test: GKw vs Kw\n")
#' cat("LR statistic:", round(lrt_gkw_kw["statistic"], 4), "\n")
#' cat("p-value:", format.pval(lrt_gkw_kw["p_value"]), "\n")
#' cat("Conclusion:", ifelse(lrt_gkw_kw["p_value"] < 0.05,
#'   "Reject H0: Full model (GKw) is better",
#'   "Fail to reject H0: Simpler model (Kw) is adequate"
#' ), "\n")
#' }
#'
#' @author Lopes, J. E.
#' @export
gkwfitall <- function(data,
                      family = NULL,
                      method = "nlminb",
                      use_moments = FALSE,
                      profile = TRUE,
                      npoints = 20,
                      plot = TRUE,
                      optimizer.control = list(),
                      gof_tests = c("ks", "ad", "cvm"),
                      theme_fn = ggplot2::theme_minimal,
                      export_report = FALSE,
                      report_file = "gkw_comparison_report.html") {
  # Enhanced data validation
  if (missing(data) || !is.numeric(data)) {
    stop("'data' must be a numeric vector")
  }

  if (length(data) < 5) {
    stop("Data must have at least 5 observations to fit all GKw family models")
  }

  # More robust boundary checking
  epsilon <- .Machine$double.eps^0.5
  boundary_values <- sum(data <= epsilon | data >= (1 - epsilon))
  if (boundary_values > 0) {
    if (boundary_values > length(data) * 0.1) {
      stop(paste0(
        boundary_values, " observations (",
        round(boundary_values / length(data) * 100, 1),
        "%) are at or near boundaries (0 or 1). Consider transforming your data."
      ))
    } else {
      warning(paste0(
        boundary_values, " observations (",
        round(boundary_values / length(data) * 100, 1),
        "%) are near boundaries (0 or 1). Results may be unreliable."
      ))
    }
  }

  # List of all valid family distributions
  all_families <- c("gkw", "bkw", "kkw", "ekw", "mc", "kw", "beta")

  # Determine which distributions to fit
  if (is.null(family)) {
    families <- all_families
    # message("Fitting all GKw family distributions...")
  } else {
    family <- as.character(family)
    unknown_families <- setdiff(family, all_families)
    if (length(unknown_families) > 0) {
      stop(
        "Unknown family name(s): ", paste(unknown_families, collapse = ", "),
        ". Valid options are: ", paste(all_families, collapse = ", ")
      )
    }
    families <- family
    message(paste0(
      "Fitting ", length(families), " GKw family distribution(s): ",
      paste(families, collapse = ", ")
    ))
  }

  # Check dependencies for plotting
  if (plot && (!requireNamespace("ggplot2", quietly = TRUE) ||
    !requireNamespace("patchwork", quietly = TRUE))) {
    warning("Packages 'ggplot2' and 'patchwork' are required for plotting. Setting plot = FALSE.")
    plot <- FALSE
  }

  # Sequential fitting
  fits <- list()
  total_families <- length(families)

  # message(paste0("Starting sequential fitting of ", total_families, " distributions..."))

  for (i in seq_along(families)) {
    fam <- families[i]
    message(paste0("  Fitting ", fam, " distribution... (", i, "/", total_families, ")"))

    tryCatch(
      {
        fits[[fam]] <- gkwfit(
          data = data,
          family = fam,
          method = method,
          use_moments = use_moments,
          profile = profile,
          npoints = npoints,
          plot = FALSE,
          optimizer.control = optimizer.control,
          silent = TRUE
        )
      },
      error = function(e) {
        warning(paste0("Failed to fit ", fam, " distribution: ", e$message))
      }
    )
  }

  # Remove NULL elements (failed fits)
  fits <- fits[!sapply(fits, is.null)]

  # If no models could be fitted, stop
  if (length(fits) == 0) {
    stop("Could not fit any of the specified GKw family distributions to the data.")
  }

  # Create comparison table
  comparison <- create_comparison_table(fits, gof_tests)

  # Calculate additional fit metrics
  metrics <- calculate_fit_metrics(fits, data)

  # Create comparison plots if requested
  if (plot) {
    plots <- create_comparison_plots(fits, data, theme_fn)
  } else {
    plots <- NULL
  }

  # Generate report if requested
  if (export_report) {
    if (!requireNamespace("rmarkdown", quietly = TRUE)) {
      warning("Package 'rmarkdown' is required for report generation. Setting export_report = FALSE.")
    } else {
      generate_report(fits, comparison, metrics, plots, data, report_file)
    }
  }

  # Return results
  result <- list(
    fits = fits,
    comparison = comparison,
    plots = plots,
    metrics = metrics
  )

  class(result) <- "gkwfitall"
  return(result)
}


#' Create comparison table of fit statistics with expanded metrics
#'
#' @param fits List of fitted gkwfit models
#' @param gof_tests Character vector of goodness-of-fit tests to perform
#' @return Data frame with comparison statistics
#' @keywords internal
create_comparison_table <- function(fits, gof_tests = c("ks", "ad", "cvm")) {
  # Create data frame with model names as rows
  model_names <- names(fits)
  n_models <- length(model_names)

  # Define all columns for comparison
  comparison_cols <- c(
    "Family", "Parameters", "LogLik", "AIC", "BIC", "AICc",
    "RMSE", "MAE", "Convergence",
    # Standard error columns
    "alpha_coef", "beta_coef", "gama_coef", "delta_coef", "lambda_coef",
    "alpha_se", "beta_se", "gama_se", "delta_se", "lambda_se"
  )

  # Add GoF test columns based on requested tests
  if ("ks" %in% gof_tests) {
    comparison_cols <- c(comparison_cols, "KS_stat", "KS_pvalue")
  }
  if ("ad" %in% gof_tests) {
    comparison_cols <- c(comparison_cols, "AD_stat", "AD_pvalue")
  }
  if ("cvm" %in% gof_tests) {
    comparison_cols <- c(comparison_cols, "CvM_stat", "CvM_pvalue")
  }

  # Create empty data frame
  comparison <- data.frame(matrix(NA, nrow = n_models, ncol = length(comparison_cols)))
  colnames(comparison) <- comparison_cols
  rownames(comparison) <- model_names

  # Fill Family column
  comparison$Family <- model_names

  # Fill in comparison statistics
  for (i in seq_along(model_names)) {
    family <- model_names[i]
    fit <- fits[[family]]

    # Basic fit statistics
    comparison$Parameters[i] <- fit$df
    comparison$LogLik[i] <- fit$loglik
    comparison$AIC[i] <- fit$AIC
    comparison$BIC[i] <- fit$BIC
    comparison$AICc[i] <- fit$AICc
    comparison$Convergence[i] <- fit$convergence

    # Calculate RMSE and MAE
    data_sorted <- sort(fit$data)
    n <- length(data_sorted)
    probs <- (1:n - 0.5) / n

    # Get quantile function for the model
    quant_func <- get_quantile_function(fit)

    # Calculate theoretical quantiles
    theo_quant <- tryCatch(
      {
        sapply(probs, quant_func)
      },
      error = function(e) {
        warning(paste0("Error calculating theoretical quantiles for ", family, ": ", e$message))
        rep(NA, length(probs))
      }
    )

    # Calculate RMSE and MAE
    if (!any(is.na(theo_quant))) {
      comparison$RMSE[i] <- sqrt(mean((data_sorted - theo_quant)^2))
      comparison$MAE[i] <- mean(abs(data_sorted - theo_quant))
    }

    # Goodness-of-fit statistics
    if ("ks" %in% gof_tests && !is.null(fit$gof) && !is.null(fit$gof$ks)) {
      comparison$KS_stat[i] <- fit$gof$ks$statistic
      comparison$KS_pvalue[i] <- fit$gof$ks$p.value
    }

    if ("ad" %in% gof_tests && !is.null(fit$gof) && !is.null(fit$gof$ad)) {
      comparison$AD_stat[i] <- fit$gof$ad$statistic
      comparison$AD_pvalue[i] <- fit$gof$ad$p.value
    } else if ("ad" %in% gof_tests) {
      # Calculate Anderson-Darling test if not available
      cdf_func <- get_cdf_function(fit)
      data_sorted <- sort(fit$data)
      n <- length(data_sorted)

      u <- tryCatch(
        {
          sapply(data_sorted, cdf_func)
        },
        error = function(e) {
          warning(paste0("Error calculating CDF for AD test (", family, "): ", e$message))
          return(NULL)
        }
      )

      if (!is.null(u) && !any(is.na(u))) {
        # AD test implementation
        logp1 <- log(u)
        logp2 <- log(1 - rev(u))
        h <- (2 * seq_len(n) - 1) * (logp1 + logp2[seq_len(n)])
        A2 <- -n - mean(h)

        # Asymptotic p-value approximation
        comparison$AD_stat[i] <- A2
        comparison$AD_pvalue[i] <- exp(0.731 - 2.492 * A2 + 0.042 * A2^2)
        if (comparison$AD_pvalue[i] > 1) comparison$AD_pvalue[i] <- 1
      }
    }

    if ("cvm" %in% gof_tests && !is.null(fit$gof) && !is.null(fit$gof$cvm)) {
      comparison$CvM_stat[i] <- fit$gof$cvm$statistic
      comparison$CvM_pvalue[i] <- fit$gof$cvm$p.value
    } else if ("cvm" %in% gof_tests) {
      # Calculate Cramer-von Mises test if not available
      cdf_func <- get_cdf_function(fit)
      data_sorted <- sort(fit$data)
      n <- length(data_sorted)

      u <- tryCatch(
        {
          sapply(data_sorted, cdf_func)
        },
        error = function(e) {
          warning(paste0("Error calculating CDF for CvM test (", family, "): ", e$message))
          return(NULL)
        }
      )

      if (!is.null(u) && !any(is.na(u))) {
        # CvM test implementation
        W2 <- sum((u - (2 * seq_len(n) - 1) / (2 * n))^2) + 1 / (12 * n)

        # Asymptotic p-value approximation
        comparison$CvM_stat[i] <- W2
        comparison$CvM_pvalue[i] <- 1 - stats::pchisq(W2 * (1 + 0.5 / n), df = 1)
      }
    }

    # Extract coefficients and standard errors
    param_mapping <- c(
      alpha_coef = "alpha",
      beta_coef = "beta",
      gama_coef = "gamma", # Map 'gama_coef' to 'gamma' parameter in the model
      delta_coef = "delta",
      lambda_coef = "lambda"
    )

    for (col_name in names(param_mapping)) {
      param_name <- param_mapping[[col_name]]
      se_col_name <- gsub("_coef", "_se", col_name)

      # Check if parameter exists in coefficients
      if (!is.null(fit$coefficients) && param_name %in% names(fit$coefficients)) {
        comparison[[col_name]][i] <- fit$coefficients[param_name]

        # Check if standard error is available
        if (!is.null(fit$std.errors) && param_name %in% names(fit$std.errors)) {
          comparison[[se_col_name]][i] <- fit$std.errors[param_name]
        }
      }
    }
  }

  # Sort by AIC (ascending)
  comparison <- comparison[order(comparison$AIC), ]

  return(comparison)
}

#' Calculate additional fit metrics for all models
#'
#' @param fits List of fitted gkwfit models
#' @param data Original data vector
#' @return List with additional fit metrics
#' @keywords internal
calculate_fit_metrics <- function(fits, data) {
  metrics <- list()

  # Calculate goodness-of-fit metrics based on CDF differences
  cdf_metrics <- data.frame(
    Family = character(),
    Mean_abs_error = numeric(),
    Max_abs_error = numeric(),
    Mean_squared_error = numeric(),
    Wasserstein_distance = numeric(),
    stringsAsFactors = FALSE
  )

  sorted_data <- sort(data)
  n <- length(sorted_data)
  empirical_cdf <- (1:n) / n

  for (family in names(fits)) {
    fit <- fits[[family]]
    cdf_func <- get_cdf_function(fit)

    # Calculate theoretical CDF
    theor_cdf <- tryCatch(
      {
        sapply(sorted_data, cdf_func)
      },
      error = function(e) {
        warning(paste0("Error calculating CDF for metrics (", family, "): ", e$message))
        rep(NA, length(sorted_data))
      }
    )

    if (!any(is.na(theor_cdf))) {
      # Calculate CDF-based metrics
      cdf_errors <- empirical_cdf - theor_cdf

      cdf_metrics <- rbind(cdf_metrics, data.frame(
        Family = family,
        Mean_abs_error = mean(abs(cdf_errors)),
        Max_abs_error = max(abs(cdf_errors)),
        Mean_squared_error = mean(cdf_errors^2),
        Wasserstein_distance = mean(abs(sorted_data - get_quantile_function(fit)(empirical_cdf))),
        stringsAsFactors = FALSE
      ))
    }
  }

  # Add metrics to result
  metrics$cdf_metrics <- cdf_metrics[order(cdf_metrics$Mean_squared_error), ]

  # Calculate model weights based on information criteria
  aic_values <- sapply(fits, function(x) x$AIC)
  bic_values <- sapply(fits, function(x) x$BIC)

  # AIC weights
  delta_aic <- aic_values - min(aic_values)
  aic_weights <- exp(-0.5 * delta_aic) / sum(exp(-0.5 * delta_aic))

  # BIC weights
  delta_bic <- bic_values - min(bic_values)
  bic_weights <- exp(-0.5 * delta_bic) / sum(exp(-0.5 * delta_bic))

  # Create model weights table
  model_weights <- data.frame(
    Family = names(fits),
    AIC = aic_values,
    Delta_AIC = delta_aic,
    AIC_weight = aic_weights,
    BIC = bic_values,
    Delta_BIC = delta_bic,
    BIC_weight = bic_weights,
    stringsAsFactors = FALSE
  )

  # Sort by AIC weight (descending)
  metrics$model_weights <- model_weights[order(-model_weights$AIC_weight), ]

  return(metrics)
}

#' Create enhanced comparison plots of all fitted distributions
#'
#' @param fits List of fitted gkwfit models
#' @param data Original data vector
#' @param theme_fn Function to apply a custom theme to plots
#' @return A ggplot2 object with multiple panels
#' @keywords internal
create_comparison_plots <- function(fits, data, theme_fn = ggplot2::theme_minimal) {
  # Check if required packages are available
  if (!requireNamespace("ggplot2", quietly = TRUE) ||
    !requireNamespace("patchwork", quietly = TRUE)) {
    warning("Packages 'ggplot2' and 'patchwork' are required for plotting.")
    return(NULL)
  }

  # Extract model families
  families <- names(fits)

  # Calculate density values on a grid for all models
  x_grid <- seq(0.001, 0.999, length.out = 200)
  density_data <- data.frame(x = numeric(0), density = numeric(0), family = character(0))

  for (family in families) {
    fit <- fits[[family]]
    density_func <- get_density_function(fit)

    # Calculate density values
    density_values <- tryCatch(
      {
        sapply(x_grid, density_func)
      },
      error = function(e) {
        warning(paste0("Error calculating density values for ", family, ": ", e$message))
        rep(NA, length(x_grid))
      }
    )

    # Add to data frame
    density_data <- rbind(
      density_data,
      data.frame(
        x = x_grid,
        density = density_values,
        family = family
      )
    )
  }

  # Prepare data for P-P and Q-Q plots
  pp_data <- data.frame(Empirical = numeric(0), Theoretical = numeric(0), family = character(0))
  qq_data <- data.frame(Theoretical = numeric(0), Empirical = numeric(0), family = character(0))

  for (family in families) {
    fit <- fits[[family]]

    # Get empirical values
    ecdf_vals <- stats::ecdf(data)(sort(data))

    # Get theoretical CDF values
    cdf_func <- get_cdf_function(fit)
    theor_cdf <- tryCatch(
      {
        sapply(sort(data), cdf_func)
      },
      error = function(e) {
        warning(paste0("Error calculating theoretical CDF for ", family, ": ", e$message))
        rep(NA, length(data))
      }
    )

    # Add to P-P data
    pp_data <- rbind(
      pp_data,
      data.frame(
        Empirical = ecdf_vals,
        Theoretical = theor_cdf,
        family = family
      )
    )

    # Get theoretical quantiles for Q-Q plot
    quant_func <- get_quantile_function(fit)
    theor_quant <- tryCatch(
      {
        sapply(stats::ppoints(length(data)), quant_func)
      },
      error = function(e) {
        warning(paste0("Error calculating theoretical quantiles for ", family, ": ", e$message))
        rep(NA, length(data))
      }
    )

    # Add to Q-Q data
    qq_data <- rbind(
      qq_data,
      data.frame(
        Theoretical = theor_quant,
        Empirical = sort(data),
        family = family
      )
    )
  }

  # Create enhanced histogram with density curve
  p1 <- ggplot2::ggplot() +
    ggplot2::geom_histogram(
      ggplot2::aes(x = data, y = ggplot2::after_stat(density)),
      bins = min(30, ceiling(sqrt(length(data)))),
      fill = "lightblue", color = "darkblue", alpha = 0.7
    ) +
    ggplot2::geom_line(
      data = density_data,
      ggplot2::aes(x = x, y = density, color = family, group = family),
      linewidth = 1
    ) +
    ggplot2::labs(
      title = "(A) Histogram with Fitted Densities",
      x = "Data", y = "Density"
    ) +
    theme_fn() +
    ggplot2::scale_color_brewer(palette = "Set1") +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
    )

  # Create enhanced P-P plot
  p2 <- ggplot2::ggplot(pp_data, ggplot2::aes(x = Theoretical, y = Empirical, color = family)) +
    ggplot2::geom_point(alpha = 0.7, size = 2) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", linewidth = 0.5) +
    ggplot2::labs(
      title = "(B) P-P Plot",
      x = "Theoretical Probability", y = "Empirical Probability"
    ) +
    theme_fn() +
    ggplot2::scale_color_brewer(palette = "Set1") +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
    )

  # Create enhanced Q-Q plot
  p3 <- ggplot2::ggplot(qq_data, ggplot2::aes(x = Theoretical, y = Empirical, color = family)) +
    ggplot2::geom_point(alpha = 0.7, size = 2) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", linewidth = 0.5) +
    ggplot2::labs(
      title = "(C) Q-Q Plot",
      x = "Theoretical Quantiles", y = "Empirical Quantiles"
    ) +
    theme_fn() +
    ggplot2::scale_color_brewer(palette = "Set1") +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
    )

  # Create enhanced residual plot
  residual_data <- data.frame(Empirical = numeric(0), Residual = numeric(0), family = character(0))

  for (family in families) {
    fit <- fits[[family]]

    # Get theoretical CDF values
    cdf_func <- get_cdf_function(fit)
    theor_cdf <- tryCatch(
      {
        sapply(sort(data), cdf_func)
      },
      error = function(e) {
        warning(paste0("Error calculating theoretical CDF for residuals (", family, "): ", e$message))
        rep(NA, length(data))
      }
    )

    # Calculate residuals
    ecdf_vals <- stats::ecdf(data)(sort(data))
    residuals <- ecdf_vals - theor_cdf

    # Add to data frame
    residual_data <- rbind(
      residual_data,
      data.frame(
        Empirical = sort(data),
        Residual = residuals,
        family = family
      )
    )
  }

  # Create enhanced residual plot
  p4 <- ggplot2::ggplot(residual_data, ggplot2::aes(x = Empirical, y = Residual, color = family)) +
    ggplot2::geom_point(alpha = 0.7, size = 2) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
    ggplot2::geom_smooth(ggplot2::aes(group = family), method = "loess", se = FALSE, linewidth = 0.5, alpha = 0.5) +
    ggplot2::labs(
      title = "(D) Residual Plot (ECDF - CDF)",
      x = "Empirical Data", y = "Difference"
    ) +
    theme_fn() +
    ggplot2::scale_color_brewer(palette = "Set1") +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
    )

  # Combine plots with a single shared legend at the bottom
  combined_plots <- patchwork::wrap_plots(p1, p2, p3, p4) +
    patchwork::plot_layout(guides = "collect") &
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(
      shape = 16,
      linetype = 1
    ))) &
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.box = "horizontal",
      legend.text = ggplot2::element_text(size = 10),
      legend.key.size = ggplot2::unit(1, "lines")
    )

  return(combined_plots)
}

#' Generate R Markdown report with analysis results
#'
#' @param fits List of fitted gkwfit models
#' @param comparison Data frame with comparison statistics
#' @param metrics List with additional fit metrics
#' @param plots ggplot2 object with diagnostic plots
#' @param data Original data vector
#' @param output_file File path for the output report
#' @return Invisibly returns the path to the generated report
#' @keywords internal
generate_report <- function(fits, comparison, metrics, plots, data, output_file) {
  # Create temporary Rmd file
  temp_rmd <- tempfile(fileext = ".Rmd")

  # Extract best model name for use in the report
  best_model_name <- comparison$Family[1]

  # Create report content
  rmd_content <- c(
    "---",
    "title: \"Comparative Analysis of Generalized Kumaraswamy Family Distributions\"",
    "date: \"`r format(Sys.time(), '%d %B, %Y')`\"",
    "output: ",
    "  html_document:",
    "    toc: true",
    "    toc_float: true",
    "    theme: cosmo",
    "    highlight: tango",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
    "library(ggplot2)",
    "library(knitr)",
    "library(kableExtra)",
    "library(patchwork)",
    "```",
    "",
    "## Executive Summary",
    "",
    paste(
      "This report presents a comparative analysis of Generalized Kumaraswamy (GKw) family distributions fitted to the data.",
      "The analysis includes comparison of information criteria, goodness-of-fit tests, and diagnostic visualizations."
    ),
    "",
    "### Key Findings",
    "",
    paste("- **Best Model**:", best_model_name),
    paste("- **Total Observations**:", length(data)),
    paste("- **AIC Value of Best Model**:", round(comparison$AIC[1], 2)),
    paste("- **BIC Value of Best Model**:", round(comparison$BIC[1], 2)),
    "",
    "## Descriptive Statistics of the Data",
    "",
    "```{r}",
    "# Summary statistics",
    "stats <- data.frame(",
    "  Statistic = c(\"Minimum\", \"First Quartile\", \"Median\", \"Mean\", \"Third Quartile\", \"Maximum\", \"Standard Deviation\", \"Skewness\", \"Kurtosis\"),",
    "  Value = c(",
    "    min(data), quantile(data, 0.25), median(data), mean(data), quantile(data, 0.75), max(data),",
    "    sd(data), ",
    "    (sum((data - mean(data))^3) / length(data)) / (sd(data)^3), # Skewness",
    "    (sum((data - mean(data))^4) / length(data)) / (sd(data)^4)  # Kurtosis",
    "  )",
    ")",
    "kable(stats, digits = 4, format = \"html\") %>%",
    "  kable_styling(bootstrap_options = c(\"striped\", \"hover\"), full_width = FALSE)",
    "```",
    "",
    "```{r fig.height=4, fig.width=10}",
    "# Data visualization",
    "p1 <- ggplot() +",
    "  geom_histogram(aes(x = data), bins = 30, fill = \"steelblue\", color = \"black\") +",
    "  labs(title = \"Histogram\", x = \"Value\", y = \"Frequency\") +",
    "  theme_minimal()",
    "",
    "p2 <- ggplot() +",
    "  geom_boxplot(aes(y = data), fill = \"steelblue\") +",
    "  labs(title = \"Boxplot\", y = \"Value\") +",
    "  theme_minimal() +",
    "  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())",
    "",
    "p3 <- ggplot() +",
    "  stat_ecdf(aes(x = data), geom = \"step\") +",
    "  labs(title = \"ECDF\", x = \"Value\", y = \"Probability\") +",
    "  theme_minimal()",
    "",
    "p1 + p2 + p3 + plot_layout(ncol = 3)",
    "```",
    "",
    "## Model Comparison",
    "",
    "### Information Criteria",
    "",
    "```{r}",
    "# Comparison table",
    "kable(comparison[, c(\"Family\", \"Parameters\", \"LogLik\", \"AIC\", \"BIC\", \"AICc\")], ",
    "      col.names = c(\"Family\", \"Parameters\", \"Log-Likelihood\", \"AIC\", \"BIC\", \"AICc\"),",
    "      digits = 4, format = \"html\") %>%",
    "  kable_styling(bootstrap_options = c(\"striped\", \"hover\"), full_width = FALSE) %>%",
    "  row_spec(1, background = \"#e6f7ff\")",
    "```",
    "",
    "### Model Weights",
    "",
    "```{r}",
    "# Weights based on AIC and BIC",
    "kable(metrics$model_weights[, c(\"Family\", \"AIC_weight\", \"BIC_weight\")], ",
    "      col.names = c(\"Family\", \"AIC Weight\", \"BIC Weight\"),",
    "      digits = 4, format = \"html\") %>%",
    "  kable_styling(bootstrap_options = c(\"striped\", \"hover\"), full_width = FALSE) %>%",
    "  row_spec(1, background = \"#e6f7ff\")",
    "```",
    "",
    "### Goodness-of-Fit Tests",
    "",
    "```{r}",
    "# Extract GoF test columns",
    "gof_cols <- grep(\"(KS|AD|CvM)_(stat|pvalue)\", names(comparison), value = TRUE)",
    "if (length(gof_cols) > 0) {",
    "  gof_table <- comparison[, c(\"Family\", gof_cols)]",
    "  # Rename columns",
    "  col_names <- c(\"Family\")",
    "  for (col in gof_cols) {",
    "    if (grepl(\"KS_stat\", col)) col_names <- c(col_names, \"KS Statistic\")",
    "    if (grepl(\"KS_pvalue\", col)) col_names <- c(col_names, \"KS p-value\")",
    "    if (grepl(\"AD_stat\", col)) col_names <- c(col_names, \"AD Statistic\")",
    "    if (grepl(\"AD_pvalue\", col)) col_names <- c(col_names, \"AD p-value\")",
    "    if (grepl(\"CvM_stat\", col)) col_names <- c(col_names, \"CvM Statistic\")",
    "    if (grepl(\"CvM_pvalue\", col)) col_names <- c(col_names, \"CvM p-value\")",
    "  }",
    "  ",
    "  kable(gof_table, col.names = col_names, digits = 4, format = \"html\") %>%",
    "    kable_styling(bootstrap_options = c(\"striped\", \"hover\"), full_width = FALSE) %>%",
    "    row_spec(1, background = \"#e6f7ff\")",
    "} else {",
    "  cat(\"No goodness-of-fit tests available.\")",
    "}",
    "```",
    "",
    "### Additional Fit Metrics",
    "",
    "```{r}",
    "# CDF-based metrics",
    "kable(metrics$cdf_metrics, ",
    "      col.names = c(\"Family\", \"Mean Absolute Error\", \"Maximum Absolute Error\", \"Mean Squared Error\", \"Wasserstein Distance\"),",
    "      digits = 6, format = \"html\") %>%",
    "  kable_styling(bootstrap_options = c(\"striped\", \"hover\"), full_width = FALSE) %>%",
    "  row_spec(which(metrics$cdf_metrics$Family == comparison$Family[1]), background = \"#e6f7ff\")",
    "```",
    "",
    "## Estimated Parameters",
    "",
    "```{r}",
    "# Extract estimated parameters",
    "param_cols <- grep(\"_coef$\", names(comparison), value = TRUE)",
    "se_cols <- grep(\"_se$\", names(comparison), value = TRUE)",
    "",
    "if (length(param_cols) > 0) {",
    "  # Prepare parameter table",
    "  param_table <- comparison[, c(\"Family\", param_cols)]",
    "  col_names <- c(\"Family\")",
    "  for (col in param_cols) {",
    "    param_name <- gsub(\"_coef\", \"\", col)",
    "    col_names <- c(col_names, paste0(param_name))",
    "  }",
    "  ",
    "  kable(param_table, col.names = col_names, digits = 4, format = \"html\") %>%",
    "    kable_styling(bootstrap_options = c(\"striped\", \"hover\"), full_width = FALSE) %>%",
    "    row_spec(1, background = \"#e6f7ff\")",
    "} else {",
    "  cat(\"No estimated parameters available.\")",
    "}",
    "```",
    "",
    "```{r}",
    "# Standard errors",
    "if (length(se_cols) > 0) {",
    "  # Prepare standard error table",
    "  se_table <- comparison[, c(\"Family\", se_cols)]",
    "  col_names <- c(\"Family\")",
    "  for (col in se_cols) {",
    "    param_name <- gsub(\"_se\", \"\", col)",
    "    col_names <- c(col_names, paste0(\"SE(\", param_name, \")\"))",
    "  }",
    "  ",
    "  kable(se_table, col.names = col_names, digits = 4, format = \"html\") %>%",
    "    kable_styling(bootstrap_options = c(\"striped\", \"hover\"), full_width = FALSE) %>%",
    "    row_spec(1, background = \"#e6f7ff\")",
    "} else {",
    "  cat(\"No standard errors available.\")",
    "}",
    "```",
    "",
    "## Diagnostic Visualizations",
    "",
    "```{r fig.height=10, fig.width=12}",
    "# Diagnostic plots",
    "plots",
    "```",
    "",
    "## Best Model Details",
    "",
    paste0("The ", best_model_name, " model was identified as the best fit for the data, based on the AIC criterion."),
    "",
    "```{r}",
    "# Best model summary",
    "best_model <- fits[[comparison$Family[1]]]",
    "if (!is.null(best_model$coefficients)) {",
    "  params <- data.frame(",
    "    Parameter = names(best_model$coefficients),",
    "    Estimate = best_model$coefficients",
    "  )",
    "  if (!is.null(best_model$std.errors)) {",
    "    params$`Standard Error` <- best_model$std.errors[params$Parameter]",
    "    params$`z-value` <- params$Estimate / params$`Standard Error`",
    "    params$`p-value` <- 2 * pnorm(-abs(params$`z-value`))",
    "  }",
    "  ",
    "  kable(params, digits = 4, format = \"html\") %>%",
    "    kable_styling(bootstrap_options = c(\"striped\", \"hover\"), full_width = FALSE)",
    "} else {",
    "  cat(\"Parameter details not available for the best model.\")",
    "}",
    "```",
    "",
    "## Conclusions",
    "",
    "```{r}",
    "# Determine quality of fit based on p-values",
    "has_pvalues <- any(grep(\"_pvalue\", names(comparison)))",
    "significant_pvalues <- FALSE",
    "if (has_pvalues) {",
    "  pvalue_cols <- grep(\"_pvalue\", names(comparison), value = TRUE)",
    "  significant_pvalues <- any(comparison[1, pvalue_cols] < 0.05, na.rm = TRUE)",
    "}",
    "",
    "# Determine if model captures data well based on RMSE",
    "captures_well <- TRUE",
    "if (!is.null(comparison$RMSE)) {",
    "  captures_well <- comparison$RMSE[1] < mean(comparison$RMSE, na.rm = TRUE)",
    "}",
    "",
    "# Determine where the model performs best",
    "performs_well_in_tails <- FALSE",
    "if (any(grep(\"KS_pvalue\", names(comparison)))) {",
    "  performs_well_in_tails <- comparison$KS_pvalue[1] > 0.05",
    "}",
    "",
    "# Check if AD test passes",
    "ad_test_passes <- FALSE",
    "if (any(grep(\"AD_pvalue\", names(comparison)))) {",
    "  ad_test_passes <- comparison$AD_pvalue[1] > 0.05",
    "}",
    "",
    "# Check if model is complex",
    "is_complex_model <- comparison$Parameters[1] > 3",
    "simplest_model <- comparison$Family[which.min(comparison$Parameters)]",
    "```",
    "",
    "Based on the analysis results, we can conclude that:",
    "",
    paste0("1. The ", best_model_name, " model provides the best fit to the data among all GKw family distributions considered."),
    paste0("2. This model had the lowest AIC value (", round(comparison$AIC[1], 2), ") and an AIC weight of ", round(metrics$model_weights$AIC_weight[1], 4), "."),
    "3. The goodness-of-fit tests indicate `r if(significant_pvalues) {\"some discrepancies\"} else {\"a good fit\"}` between the theoretical model and the observed data.",
    "",
    "The diagnostic visualizations show that the ", best_model_name, " model `r if(captures_well) {\"captures well\"} else {\"may not adequately capture\"}` the behavior of the data, especially `r if(performs_well_in_tails) {\"in the tails of the distribution\"} else {\"in the central region of the distribution\"}`. ",
    "",
    "## Recommendations",
    "",
    paste0("1. For statistical modeling of this data, we recommend using the ", best_model_name, " distribution."),
    "2. `r if(is_complex_model) {paste0(\"Consider using simpler models like \", simplest_model, \" if interpretability is a priority, as it has fewer parameters.\")} else {\"This model offers a good balance between fit and parsimony.\"}` ",
    "3. For applications requiring high precision in distribution tails, `r if(ad_test_passes) {paste0(\"the \", best_model_name, \" distribution is suitable, as indicated by the Anderson-Darling test.\")} else {\"it may be necessary to explore other distribution families beyond GKw.\"}` ",
    "",
    "## References",
    "",
    "1. Kumaraswamy, P. (1980). A generalized probability density function for double-bounded random processes. Journal of Hydrology, 46(1-2), 79-88.",
    "2. Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized distributions. Journal of Statistical Computation and Simulation, 81(7), 883-898.",
    "3. Kristensen, K., Nielsen, A., Berg, C. W., Skaug, H., & Bell, B. M. (2016). TMB: Automatic Differentiation and Laplace Approximation. Journal of Statistical Software, 70(5), 1-21."
  )

  # Write Rmd content to file
  writeLines(rmd_content, temp_rmd)

  # Render the report
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package 'rmarkdown' is required to render the report")
  }

  tryCatch({
    message("Generating report...")
    rmarkdown::render(temp_rmd, output_file = output_file, quiet = TRUE, output_dir = ".")
    message(paste0("Report generated successfully: ", output_file))
  }, error = function(e) {
    warning(paste0("Failed to generate report: ", e$message))
  }, finally = {
    # Clean up temp file
    if (file.exists(temp_rmd)) {
      file.remove(temp_rmd)
    }
  })

  invisible(output_file)
}

#' Get the density function for a fitted GKw distribution model
#'
#' @param fit An object of class \code{"gkwfit"} from the gkwfit function
#' @return A function that computes density values for the fitted distribution
#' @keywords internal
get_density_function <- function(fit) {
  # Extract parameters from fit
  family <- fit$family
  coef <- fit$coefficients

  if (family == "gkw") {
    # Full Generalized Kumaraswamy
    alpha <- coef["alpha"]
    beta <- coef["beta"]
    gamma <- coef["gamma"]
    delta <- coef["delta"]
    lambda <- coef["lambda"]

    function(x) {
      dgkw(x, alpha, beta, gamma, delta, lambda, log_prob = FALSE)
    }
  } else if (family == "bkw") {
    # Beta-Kumaraswamy (lambda = 1)
    alpha <- coef["alpha"]
    beta <- coef["beta"]
    gamma <- coef["gamma"]
    delta <- coef["delta"]

    function(x) {
      dbkw(x, alpha, beta, gamma, delta, log_prob = FALSE)
    }
  } else if (family == "kkw") {
    # Kumaraswamy-Kumaraswamy (gamma = 1)
    alpha <- coef["alpha"]
    beta <- coef["beta"]
    delta <- coef["delta"]
    lambda <- coef["lambda"]

    function(x) {
      dkkw(x, alpha, beta, delta, lambda, log_prob = FALSE)
    }
  } else if (family == "ekw") {
    # Extended Kumaraswamy (gamma = 1, delta = 0)
    alpha <- coef["alpha"]
    beta <- coef["beta"]
    lambda <- coef["lambda"]

    function(x) {
      dekw(x, alpha, beta, lambda, log_prob = FALSE)
    }
  } else if (family == "mc") {
    # McDonald / Beta Power (alpha = 1, beta = 1)
    gamma <- coef["gamma"]
    delta <- coef["delta"]
    lambda <- coef["lambda"]

    function(x) {
      dmc(x, gamma, delta, lambda, log_prob = FALSE)
    }
  } else if (family == "kw") {
    # Kumaraswamy (gamma = 1, delta = 0, lambda = 1)
    alpha <- coef["alpha"]
    beta <- coef["beta"]

    function(x) {
      dkw(x, alpha, beta, log_prob = FALSE)
    }
  } else if (family == "beta") {
    # Beta (alpha = 1, beta = 1, lambda = 1)
    gamma <- coef["gamma"]
    delta <- coef["delta"]

    function(x) {
      dbeta_(x, gamma, delta, log_prob = FALSE)
    }
  } else {
    stop(paste0("Unknown distribution family: ", family))
  }
}

#' Get the CDF function for a fitted GKw distribution model
#'
#' @param fit An object of class \code{"gkwfit"} from the gkwfit function
#' @return A function that computes CDF values for the fitted distribution
#' @keywords internal
get_cdf_function <- function(fit) {
  # Extract parameters from fit
  family <- fit$family
  coef <- fit$coefficients

  if (family == "gkw") {
    # Full Generalized Kumaraswamy
    alpha <- coef["alpha"]
    beta <- coef["beta"]
    gamma <- coef["gamma"]
    delta <- coef["delta"]
    lambda <- coef["lambda"]

    function(x) {
      pgkw(x, alpha, beta, gamma, delta, lambda)
    }
  } else if (family == "bkw") {
    # Beta-Kumaraswamy (lambda = 1)
    alpha <- coef["alpha"]
    beta <- coef["beta"]
    gamma <- coef["gamma"]
    delta <- coef["delta"]

    function(x) {
      pbkw(x, alpha, beta, gamma, delta)
    }
  } else if (family == "kkw") {
    # Kumaraswamy-Kumaraswamy (gamma = 1)
    alpha <- coef["alpha"]
    beta <- coef["beta"]
    delta <- coef["delta"]
    lambda <- coef["lambda"]

    function(x) {
      pkkw(x, alpha, beta, delta, lambda)
    }
  } else if (family == "ekw") {
    # Extended Kumaraswamy (gamma = 1, delta = 0)
    alpha <- coef["alpha"]
    beta <- coef["beta"]
    lambda <- coef["lambda"]

    function(x) {
      pekw(x, alpha, beta, lambda)
    }
  } else if (family == "mc") {
    # McDonald / Beta Power (alpha = 1, beta = 1)
    gamma <- coef["gamma"]
    delta <- coef["delta"]
    lambda <- coef["lambda"]

    function(x) {
      pmc(x, gamma, delta, lambda)
    }
  } else if (family == "kw") {
    # Kumaraswamy (gamma = 1, delta = 0, lambda = 1)
    alpha <- coef["alpha"]
    beta <- coef["beta"]

    function(x) {
      pkw(x, alpha, beta)
    }
  } else if (family == "beta") {
    # Beta (alpha = 1, beta = 1, lambda = 1)
    gamma <- coef["gamma"]
    delta <- coef["delta"]

    function(x) {
      pbeta_(x, gamma, delta)
    }
  } else {
    stop(paste0("Unknown distribution family: ", family))
  }
}

#' Get the quantile function for a fitted GKw distribution model
#'
#' @param fit An object of class \code{"gkwfit"} from the gkwfit function
#' @return A function that computes quantiles for the fitted distribution
#' @keywords internal
get_quantile_function <- function(fit) {
  # Extract parameters from fit
  family <- fit$family
  coef <- fit$coefficients

  if (family == "gkw") {
    # Full Generalized Kumaraswamy
    alpha <- coef["alpha"]
    beta <- coef["beta"]
    gamma <- coef["gamma"]
    delta <- coef["delta"]
    lambda <- coef["lambda"]

    function(p) {
      qgkw(p, alpha, beta, gamma, delta, lambda)
    }
  } else if (family == "bkw") {
    # Beta-Kumaraswamy (lambda = 1)
    alpha <- coef["alpha"]
    beta <- coef["beta"]
    gamma <- coef["gamma"]
    delta <- coef["delta"]

    function(p) {
      qbkw(p, alpha, beta, gamma, delta)
    }
  } else if (family == "kkw") {
    # Kumaraswamy-Kumaraswamy (gamma = 1)
    alpha <- coef["alpha"]
    beta <- coef["beta"]
    delta <- coef["delta"]
    lambda <- coef["lambda"]

    function(p) {
      qkkw(p, alpha, beta, delta, lambda)
    }
  } else if (family == "ekw") {
    # Extended Kumaraswamy (gamma = 1, delta = 0)
    alpha <- coef["alpha"]
    beta <- coef["beta"]
    lambda <- coef["lambda"]

    function(p) {
      qekw(p, alpha, beta, lambda)
    }
  } else if (family == "mc") {
    # McDonald / Beta Power (alpha = 1, beta = 1)
    gamma <- coef["gamma"]
    delta <- coef["delta"]
    lambda <- coef["lambda"]

    function(p) {
      qmc(p, gamma, delta, lambda)
    }
  } else if (family == "kw") {
    # Kumaraswamy (gamma = 1, delta = 0, lambda = 1)
    alpha <- coef["alpha"]
    beta <- coef["beta"]

    function(p) {
      qkw(p, alpha, beta)
    }
  } else if (family == "beta") {
    # Beta (alpha = 1, beta = 1, lambda = 1)
    gamma <- coef["gamma"]
    delta <- coef["delta"]

    function(p) {
      qbeta_(p, gamma, delta)
    }
  } else {
    stop(paste0("Unknown distribution family: ", family))
  }
}


#' Print method for gkwfitall objects
#'
#' @param x An object of class \code{"gkwfitall"}
#' @param ... Additional arguments (currently ignored)
#' @return Invisibly returns the input object
#' @author Lopes, J. E.
#' @export
print.gkwfitall <- function(x, ...) {
  cat("Fit Comparison of Generalized Kumaraswamy Family Distributions\n\n")

  cat("Number of families fitted:", length(x$fits), "\n")
  cat("Families:", paste(names(x$fits), collapse = ", "), "\n\n")

  cat("Comparison table (ordered by AIC):\n")

  # Select most informative columns for display
  display_cols <- intersect(
    c(
      "Family", "Parameters", "LogLik", "AIC", "BIC", "AICc", "KS_stat", "KS_pvalue",
      "AD_stat", "AD_pvalue", "RMSE", "MAE", "Convergence"
    ),
    names(x$comparison)
  )

  print(x$comparison[, display_cols])

  cat("\nBest fitting model:", x$comparison$Family[1], "\n")

  # Show model weights if available
  if (!is.null(x$metrics) && !is.null(x$metrics$model_weights)) {
    cat("\nModel weights (based on AIC):\n")
    print(x$metrics$model_weights[, c("Family", "AIC_weight")])
  }

  invisible(x)
}

#' Summary method for gkwfitall objects
#'
#' @param object An object of class \code{"gkwfitall"}
#' @param ... Additional arguments (currently ignored)
#' @return A summarized version of the gkwfitall object
#' @author Lopes, J. E.
#' @export
summary.gkwfitall <- function(object, ...) {
  # Create a summary object
  result <- list(
    comparison = object$comparison,
    best_model = object$fits[[object$comparison$Family[1]]],
    n_models = length(object$fits),
    model_names = names(object$fits),
    metrics = object$metrics
  )

  class(result) <- "summary.gkwfitall"
  return(result)
}

#' Print method for summary.gkwfitall objects
#'
#' @param x An object of class \code{"summary.gkwfitall"}
#' @param ... Additional arguments (currently ignored)
#' @return Invisibly returns the input object
#' @author Lopes, J. E.
#' @export
print.summary.gkwfitall <- function(x, ...) {
  cat("Summary of Generalized Kumaraswamy Family Distributions Fit Comparison\n\n")

  cat("Number of families fitted:", x$n_models, "\n")
  cat("Families:", paste(x$model_names, collapse = ", "), "\n\n")

  cat("Comparison table (ordered by AIC):\n")

  display_cols <- intersect(
    c("Family", "Parameters", "LogLik", "AIC", "BIC", "KS_stat", "KS_pvalue", "RMSE"),
    names(x$comparison)
  )

  print(x$comparison[, display_cols])

  cat("\nBest fitting model:", x$comparison$Family[1], "\n\n")

  # Show model weights if available
  if (!is.null(x$metrics) && !is.null(x$metrics$model_weights)) {
    cat("Model weights (based on AIC):\n")
    print(x$metrics$model_weights[, c("Family", "AIC_weight")])
    cat("\n")
  }

  cat("Summary of best fitting model:\n")
  print(summary(x$best_model))

  invisible(x)
}

#' Plot method for gkwfitall objects
#'
#' @param x An object of class \code{"gkwfitall"}
#' @param which Character vector specifying which plots to show. Options are "all" (default),
#'   "density", "pp", "qq", "residuals", "aic", or "parameters".
#' @param theme_fn Function to apply a custom theme to plots. Default: \code{ggplot2::theme_minimal}.
#' @param ... Additional arguments passed to plotting functions
#' @return Invisibly returns the input object
#' @author Lopes, J. E.
#' @export
plot.gkwfitall <- function(x, which = "all", theme_fn = ggplot2::theme_minimal, ...) {
  if (is.null(x$plots) && which == "all") {
    stop("No plots available. The 'gkwfitall' object was created with plot = FALSE.")
  }

  # For standard plots
  if (which == "all" && !is.null(x$plots)) {
    print(x$plots)
    return(invisible(x))
  }

  # Check required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for custom plotting.")
  }

  # Specific plots
  if (which == "aic" || which == "parameters") {
    # AIC/BIC comparison plot
    if (which == "aic") {
      df_ic <- x$comparison[, c("Family", "AIC", "BIC")]
      df_ic_long <- reshape2::melt(df_ic,
        id.vars = "Family",
        variable.name = "Criterion", value.name = "Value"
      )

      p <- ggplot2::ggplot(df_ic_long, ggplot2::aes(x = stats::reorder(Family, Value), y = Value, fill = Criterion)) +
        ggplot2::geom_bar(stat = "identity", position = "dodge") +
        ggplot2::labs(
          title = "Information Criteria Comparison",
          x = "Family", y = "Value"
        ) +
        ggplot2::coord_flip() +
        theme_fn() +
        ggplot2::scale_fill_brewer(palette = "Set1")

      print(p)
    }

    # Parameter estimates plot
    if (which == "parameters") {
      param_cols <- grep("_coef$", names(x$comparison), value = TRUE)
      if (length(param_cols) > 0) {
        df_params <- x$comparison[, c("Family", param_cols)]
        df_params_long <- reshape2::melt(df_params,
          id.vars = "Family",
          variable.name = "Parameter", value.name = "Value"
        )
        df_params_long$Parameter <- gsub("_coef", "", df_params_long$Parameter)

        p <- ggplot2::ggplot(df_params_long, ggplot2::aes(x = Parameter, y = Value, fill = Family)) +
          ggplot2::geom_bar(stat = "identity", position = "dodge") +
          ggplot2::labs(
            title = "Parameter Estimates by Model",
            x = "Parameter", y = "Value"
          ) +
          theme_fn() +
          ggplot2::scale_fill_brewer(palette = "Set1")

        print(p)
      } else {
        warning("No parameter estimates available for plotting.")
      }
    }
  } else {
    # Recalculate diagnostic plots if requested
    fits <- x$fits
    data <- fits[[1]]$data

    if (which == "density") {
      # Calculate density values on a grid for all models
      x_grid <- seq(0.001, 0.999, length.out = 200)
      density_data <- data.frame(x = numeric(0), density = numeric(0), family = character(0))

      for (family in names(fits)) {
        fit <- fits[[family]]
        density_func <- get_density_function(fit)

        # Calculate density values
        density_values <- tryCatch(
          {
            sapply(x_grid, density_func)
          },
          error = function(e) {
            warning(paste0("Error calculating density values for ", family, ": ", e$message))
            rep(NA, length(x_grid))
          }
        )

        # Add to data frame
        density_data <- rbind(
          density_data,
          data.frame(
            x = x_grid,
            density = density_values,
            family = family
          )
        )
      }

      # Create histogram with density curve
      p <- ggplot2::ggplot() +
        ggplot2::geom_histogram(
          ggplot2::aes(x = data, y = ggplot2::after_stat(density)),
          bins = min(30, ceiling(sqrt(length(data)))),
          fill = "lightblue", color = "darkblue", alpha = 0.7
        ) +
        ggplot2::geom_line(
          data = density_data,
          ggplot2::aes(x = x, y = density, color = family, group = family),
          linewidth = 1
        ) +
        ggplot2::labs(
          title = "Histogram with Fitted Densities",
          x = "Data", y = "Density"
        ) +
        theme_fn() +
        ggplot2::scale_color_brewer(palette = "Set1")

      print(p)
    } else if (which == "pp" || which == "qq" || which == "residuals") {
      # Prepare data for P-P and Q-Q plots
      pp_data <- data.frame(Empirical = numeric(0), Theoretical = numeric(0), family = character(0))
      qq_data <- data.frame(Theoretical = numeric(0), Empirical = numeric(0), family = character(0))

      for (family in names(fits)) {
        fit <- fits[[family]]

        # Get empirical values
        ecdf_vals <- stats::ecdf(data)(sort(data))

        # Get theoretical CDF values
        cdf_func <- get_cdf_function(fit)
        theor_cdf <- tryCatch(
          {
            sapply(sort(data), cdf_func)
          },
          error = function(e) {
            warning(paste0("Error calculating theoretical CDF for ", family, ": ", e$message))
            rep(NA, length(data))
          }
        )

        # Add to P-P data
        pp_data <- rbind(
          pp_data,
          data.frame(
            Empirical = ecdf_vals,
            Theoretical = theor_cdf,
            family = family
          )
        )

        # Get theoretical quantiles for Q-Q plot
        quant_func <- get_quantile_function(fit)
        theor_quant <- tryCatch(
          {
            sapply(stats::ppoints(length(data)), quant_func)
          },
          error = function(e) {
            warning(paste0("Error calculating theoretical quantiles for ", family, ": ", e$message))
            rep(NA, length(data))
          }
        )

        # Add to Q-Q data
        qq_data <- rbind(
          qq_data,
          data.frame(
            Theoretical = theor_quant,
            Empirical = sort(data),
            family = family
          )
        )
      }

      # P-P plot
      if (which == "pp") {
        p <- ggplot2::ggplot(pp_data, ggplot2::aes(x = Theoretical, y = Empirical, color = family)) +
          ggplot2::geom_point(alpha = 0.7, size = 2) +
          ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", linewidth = 0.5) +
          ggplot2::labs(
            title = "P-P Plot",
            x = "Theoretical Probability", y = "Empirical Probability"
          ) +
          theme_fn() +
          ggplot2::scale_color_brewer(palette = "Set1")

        print(p)
      }

      # Q-Q plot
      if (which == "qq") {
        p <- ggplot2::ggplot(qq_data, ggplot2::aes(x = Theoretical, y = Empirical, color = family)) +
          ggplot2::geom_point(alpha = 0.7, size = 2) +
          ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", linewidth = 0.5) +
          ggplot2::labs(
            title = "Q-Q Plot",
            x = "Theoretical Quantiles", y = "Empirical Quantiles"
          ) +
          theme_fn() +
          ggplot2::scale_color_brewer(palette = "Set1")

        print(p)
      }

      # Residual plot
      if (which == "residuals") {
        residual_data <- data.frame(Empirical = numeric(0), Residual = numeric(0), family = character(0))

        for (family in names(fits)) {
          fit <- fits[[family]]

          # Get theoretical CDF values
          cdf_func <- get_cdf_function(fit)
          theor_cdf <- tryCatch(
            {
              sapply(sort(data), cdf_func)
            },
            error = function(e) {
              warning(paste0("Error calculating theoretical CDF for residuals (", family, "): ", e$message))
              rep(NA, length(data))
            }
          )

          # Calculate residuals
          ecdf_vals <- stats::ecdf(data)(sort(data))
          residuals <- ecdf_vals - theor_cdf

          # Add to data frame
          residual_data <- rbind(
            residual_data,
            data.frame(
              Empirical = sort(data),
              Residual = residuals,
              family = family
            )
          )
        }

        p <- ggplot2::ggplot(residual_data, ggplot2::aes(x = Empirical, y = Residual, color = family)) +
          ggplot2::geom_point(alpha = 0.7, size = 2) +
          ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
          ggplot2::geom_smooth(ggplot2::aes(group = family), method = "loess", se = FALSE, linewidth = 0.5, alpha = 0.5) +
          ggplot2::labs(
            title = "Residual Plot (ECDF - CDF)",
            x = "Empirical Data", y = "Difference"
          ) +
          theme_fn() +
          ggplot2::scale_color_brewer(palette = "Set1")

        print(p)
      }
    } else {
      warning("Invalid 'which' parameter. Choose from 'all', 'density', 'pp', 'qq', 'residuals', 'aic', or 'parameters'.")
    }
  }

  invisible(x)
}
