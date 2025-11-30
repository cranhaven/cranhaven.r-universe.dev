#' @title Predictions from a Fitted Generalized Kumaraswamy Regression Model
#'
#' @description
#' Computes predictions and related quantities from a fitted Generalized
#' Kumaraswamy (GKw) regression model object. This method can extract fitted values,
#' predicted means, linear predictors, parameter values, variances, densities,
#' probabilities, and quantiles based on the estimated model. Predictions can be made
#' for new data or for the original data used to fit the model.
#'
#' @param object An object of class \code{"gkwreg"}, typically the result of a
#'   call to \code{\link{gkwreg}}.
#' @param newdata An optional data frame containing the variables needed for prediction.
#'   If omitted, predictions are made for the data used to fit the model.
#' @param type A character string specifying the type of prediction. Options are:
#'   \describe{
#'     \item{\code{"response"} or \code{"mean"}}{Predicted mean response (default).}
#'     \item{\code{"link"}}{Linear predictors for each parameter before applying
#'       inverse link functions.}
#'     \item{\code{"parameter"}}{Parameter values on their original scale (after
#'       applying inverse link functions).}
#'     \item{\code{"alpha"}, \code{"beta"}, \code{"gamma"}, \code{"delta"},
#'       \code{"lambda"}}{Values for a specific distribution parameter.}
#'     \item{\code{"variance"}}{Predicted variance of the response.}
#'     \item{\code{"density"} or \code{"pdf"}}{Density function values at points
#'       specified by \code{at}.}
#'     \item{\code{"probability"} or \code{"cdf"}}{Cumulative distribution function
#'       values at points specified by \code{at}.}
#'     \item{\code{"quantile"}}{Quantiles corresponding to probabilities specified
#'       by \code{at}.}
#'   }
#' @param na.action Function determining how to handle missing values in \code{newdata}.
#'   Default is \code{stats::na.pass}, which returns \code{NA} for cases with missing
#'   predictors.
#' @param at Numeric vector of values at which to evaluate densities, probabilities,
#'   or for which to compute quantiles, depending on \code{type}. Required for
#'   \code{type = "density"}, \code{type = "probability"}, or \code{type = "quantile"}.
#'   Defaults to 0.5.
#' @param elementwise Logical. If \code{TRUE} and \code{at} has the same length as
#'   the number of observations, applies each value in \code{at} to the corresponding
#'   observation. If \code{FALSE} (default), applies all values in \code{at} to each
#'   observation, returning a matrix.
#' @param family Character string specifying the distribution family to use for
#'   calculations. If \code{NULL} (default), uses the family from the fitted model.
#'   Options match those in \code{\link{gkwreg}}: \code{"gkw"}, \code{"bkw"},
#'   \code{"kkw"}, \code{"ekw"}, \code{"mc"}, \code{"kw"}, \code{"beta"}.
#' @param ... Additional arguments (currently not used).
#'
#' @details
#' The \code{predict.gkwreg} function provides a flexible framework for obtaining
#' predictions and inference from fitted Generalized Kumaraswamy regression models.
#' It handles all subfamilies of GKw distributions and respects the parametrization
#' and link functions specified in the original model.
#'
#' \subsection{Prediction Types}{
#' The function supports several types of predictions:
#' \itemize{
#'   \item \strong{Response/Mean}: Computes the expected value of the response variable
#'     based on the model parameters. For most GKw family distributions, this requires
#'     numerical integration or special formulas.
#'
#'   \item \strong{Link}: Returns the linear predictors for each parameter without
#'     applying inverse link functions. These are the values \eqn{\eta_j = X\beta_j}
#'     for each parameter \eqn{j}.
#'
#'   \item \strong{Parameter}: Computes the distribution parameter values on their original
#'     scale by applying the appropriate inverse link functions to the linear predictors.
#'     For example, if alpha uses a log link, then \eqn{\alpha = \exp(X\beta_\alpha)}.
#'
#'   \item \strong{Individual Parameters}: Extract specific parameter values (alpha, beta,
#'     gamma, delta, lambda) on their original scale.
#'
#'   \item \strong{Variance}: Estimates the variance of the response based on the model
#'     parameters. For some distributions, analytical formulas are used; for others,
#'     numerical approximations are employed.
#'
#'   \item \strong{Density/PDF}: Evaluates the probability density function at specified
#'     points given the model parameters.
#'
#'   \item \strong{Probability/CDF}: Computes the cumulative distribution function at
#'     specified points given the model parameters.
#'
#'   \item \strong{Quantile}: Calculates quantiles corresponding to specified probabilities
#'     given the model parameters.
#' }
#' }
#'
#' \subsection{Link Functions}{
#' The function respects the link functions specified in the original model for each
#' parameter. The supported link functions are:
#' \itemize{
#'   \item \code{"log"}: \eqn{g(\mu) = \log(\mu)}, \eqn{g^{-1}(\eta) = \exp(\eta)}
#'   \item \code{"logit"}: \eqn{g(\mu) = \log(\mu/(1-\mu))}, \eqn{g^{-1}(\eta) = 1/(1+\exp(-\eta))}
#'   \item \code{"probit"}: \eqn{g(\mu) = \Phi^{-1}(\mu)}, \eqn{g^{-1}(\eta) = \Phi(\eta)}
#'   \item \code{"cauchy"}: \eqn{g(\mu) = \tan(\pi*(\mu-0.5))}, \eqn{g^{-1}(\eta) = 0.5 + (1/\pi) \arctan(\eta)}
#'   \item \code{"cloglog"}: \eqn{g(\mu) = \log(-\log(1-\mu))}, \eqn{g^{-1}(\eta) = 1 - \exp(-\exp(\eta))}
#'   \item \code{"identity"}: \eqn{g(\mu) = \mu}, \eqn{g^{-1}(\eta) = \eta}
#'   \item \code{"sqrt"}: \eqn{g(\mu) = \sqrt{\mu}}, \eqn{g^{-1}(\eta) = \eta^2}
#'   \item \code{"inverse"}: \eqn{g(\mu) = 1/\mu}, \eqn{g^{-1}(\eta) = 1/\eta}
#'   \item \code{"inverse-square"}: \eqn{g(\mu) = 1/\sqrt{\mu}}, \eqn{g^{-1}(\eta) = 1/\eta^2}
#' }
#' }
#'
#' \subsection{Family-Specific Constraints}{
#' The function enforces appropriate constraints for each distribution family:
#' \itemize{
#'   \item \code{"gkw"}: All 5 parameters (\eqn{\alpha, \beta, \gamma, \delta, \lambda}) are used.
#'   \item \code{"bkw"}: \eqn{\lambda = 1} is fixed.
#'   \item \code{"kkw"}: \eqn{\gamma = 1} is fixed.
#'   \item \code{"ekw"}: \eqn{\gamma = 1, \delta = 0} are fixed.
#'   \item \code{"mc"}: \eqn{\alpha = 1, \beta = 1} are fixed.
#'   \item \code{"kw"}: \eqn{\gamma = 1, \delta = 0, \lambda = 1} are fixed.
#'   \item \code{"beta"}: \eqn{\alpha = 1, \beta = 1, \lambda = 1} are fixed.
#' }
#' }
#'
#' \subsection{Parameter Bounds}{
#' All parameters are constrained to their valid ranges:
#' \itemize{
#'   \item \eqn{\alpha, \beta, \gamma, \lambda > 0}
#'   \item \eqn{0 < \delta < 1}
#' }
#' }
#'
#' \subsection{Using with New Data}{
#' When providing \code{newdata}, ensure it contains all variables used in the model's
#' formula. The function extracts the terms for each parameter's model matrix and applies
#' the appropriate link functions to calculate predictions. If any variables are missing,
#' the function will attempt to substitute reasonable defaults or raise an error if
#' critical variables are absent.
#' }
#'
#' \subsection{Using for Model Evaluation}{
#' The function is useful for model checking, generating predicted values for plotting,
#' and evaluating the fit of different distribution families. By specifying the \code{family}
#' parameter, you can compare predictions under different distributional assumptions.
#' }
#'
#' @return The return value depends on the \code{type} argument:
#' \itemize{
#'   \item For \code{type = "response"}, \code{type = "variance"}, or individual
#'     parameters (\code{type = "alpha"}, etc.): A numeric vector of length equal
#'     to the number of rows in \code{newdata} (or the original data).
#'
#'   \item For \code{type = "link"} or \code{type = "parameter"}: A data frame with
#'     columns for each parameter and rows corresponding to observations.
#'
#'   \item For \code{type = "density"}, \code{type = "probability"}, or
#'     \code{type = "quantile"}:
#'     \itemize{
#'       \item If \code{elementwise = TRUE}: A numeric vector of length equal to
#'         the number of rows in \code{newdata} (or the original data).
#'       \item If \code{elementwise = FALSE}: A matrix where rows correspond to
#'         observations and columns correspond to the values in \code{at}.
#'     }
#' }
#'
#' @examples
#' \donttest{
#' # Generate a sample dataset (n = 1000)
#' library(gkwdist)
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
#' # Generate random responses
#' y <- rkw(n, alpha = true_alpha, beta = true_beta)
#'
#' # Ensure responses are strictly in (0, 1)
#' y <- pmax(pmin(y, 1 - 1e-7), 1e-7)
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
#' pred_na_pass <- tryCatch(
#'   predict(kw_model, newdata = test_missing, na.action = na.pass),
#'   error = function(e) rep(NA, nrow(test_missing))
#' )
#' pred_na_omit <- tryCatch(
#'   predict(kw_model, newdata = test_missing, na.action = na.omit),
#'   error = function(e) rep(NA, nrow(test_missing))
#' )
#'
#' # Show which positions have NAs
#' cat("Rows with missing predictors:", which(is.na(pred_na_pass)), "\n")
#' cat("Length after na.omit:", length(pred_na_omit), "\n")
#' }
#'
#' @seealso
#' \code{\link{gkwreg}} for fitting Generalized Kumaraswamy regression models,
#' \code{\link{fitted.gkwreg}} for extracting fitted values,
#' \code{\link{residuals.gkwreg}} for calculating residuals,
#' \code{\link{summary.gkwreg}} for model summaries,
#' \code{\link{coef.gkwreg}} for extracting coefficients.
#'
#' @references
#' Cordeiro, G. M., & de Castro, M. (2011). A new family of generalized distributions.
#' \emph{Journal of Statistical Computation and Simulation}, \strong{81}(7), 883-898.
#'
#' Kumaraswamy, P. (1980). A generalized probability density function for double-bounded
#' random processes. \emph{Journal of Hydrology}, \strong{46}(1-2), 79-88.
#'
#' Ferrari, S. L. P., & Cribari-Neto, F. (2004). Beta regression for modelling rates and
#' proportions. \emph{Journal of Applied Statistics}, \strong{31}(7), 799-815.
#'
#' Jones, M. C. (2009). Kumaraswamy's distribution: A beta-type distribution with some
#' tractability advantages. \emph{Statistical Methodology}, \strong{6}(1), 70-81.
#'
#' @author Lopes, J. E. and contributors
#'
#' @keywords models regression predict
#' @importFrom stats predict qchisq pnorm
#' @import gkwdist
#' @method predict gkwreg
#' @export
predict.gkwreg <- function(object, newdata = NULL,
                           type = "response",
                           na.action = stats::na.pass, at = 0.5,
                           elementwise = NULL, family = NULL, ...) {
  # Match type argument
  type <- match.arg(type, c(
    "response", "link", "parameter",
    "alpha", "beta", "gamma", "delta", "lambda",
    "variance", "density", "pdf",
    "probability", "cdf", "quantile"
  ))

  # Aliases for some types
  if (type == "pdf") type <- "density"
  if (type == "cdf") type <- "probability"
  if (type == "mean") type <- "response"

  # Validate object
  if (!inherits(object, "gkwreg")) {
    stop("'object' must be a gkwreg model")
  }

  # Get the family from the object if not specified
  if (is.null(family)) {
    if (!is.null(object$family)) {
      family <- object$family
    } else {
      # Default to gkw for backward compatibility
      family <- "gkw"
      warning("No family specified in the model. Using 'gkw' as default.")
    }
  } else {
    # Validate the family parameter
    family <- match.arg(family, c("gkw", "bkw", "kkw", "ekw", "mc", "kw", "beta"))
  }

  # Import functions from the required package
  if (!requireNamespace("Formula", quietly = TRUE)) {
    stop("Package 'Formula' is needed for this function.")
  }

  # Handle case when we can directly use parameter vectors
  if (is.null(newdata) && !is.null(object$parameter_vectors) &&
    type %in% c(
      "response", "parameter", "alpha", "beta", "gamma", "delta", "lambda",
      "density", "probability", "quantile"
    )) {
    n <- length(object$parameter_vectors$alphaVec)
    params <- matrix(0, nrow = n, ncol = 5)
    params[, 1] <- object$parameter_vectors$alphaVec
    params[, 2] <- object$parameter_vectors$betaVec
    params[, 3] <- object$parameter_vectors$gammaVec
    params[, 4] <- object$parameter_vectors$deltaVec
    params[, 5] <- object$parameter_vectors$lambdaVec

    # Special case for response and fitted values
    if (type == "response" && !is.null(object$fitted.values)) {
      return(object$fitted.values)
    }

    # Handle parameter predictions
    if (type == "parameter") {
      return(data.frame(
        alpha = params[, 1],
        beta = params[, 2],
        gamma = params[, 3],
        delta = params[, 4],
        lambda = params[, 5]
      ))
    } else if (type %in% c("alpha", "beta", "gamma", "delta", "lambda")) {
      param_index <- match(type, c("alpha", "beta", "gamma", "delta", "lambda"))
      return(params[, param_index])
    }

    # For density, probability, and quantile using at
    if (type %in% c("density", "probability", "quantile")) {
      if (!is.numeric(at)) {
        stop("'at' must be numeric")
      }

      if (is.null(elementwise)) {
        elementwise <- length(at) == n
      }

      if (elementwise) {
        if (length(at) != n) {
          stop("For elementwise=TRUE, length of 'at' must equal number of observations")
        }
        eval_y <- at
        eval_params <- params
      } else {
        eval_params <- do.call(rbind, lapply(seq_len(length(at)), function(i) params))
        eval_y <- rep(at, each = n)
      }

      # Use the specific family distribution functions with proper prefixes
      result <- numeric(length(eval_y))

      # Loop through observations to calculate results using the appropriate distribution function
      for (i in seq_along(eval_y)) {
        alpha_i <- eval_params[i, 1]
        beta_i <- eval_params[i, 2]
        gamma_i <- eval_params[i, 3]
        delta_i <- eval_params[i, 4]
        lambda_i <- eval_params[i, 5]
        y_i <- eval_y[i]

        # Use the appropriate distribution function based on family and type
        if (type == "density") {
          # Density functions (d-prefix)
          switch(family,
            "gkw" = {
              result[i] <- dgkw(y_i, alpha_i, beta_i, gamma_i, delta_i, lambda_i)
            },
            "bkw" = {
              result[i] <- dbkw(y_i, alpha_i, beta_i, gamma_i, delta_i)
            },
            "kkw" = {
              result[i] <- dkkw(y_i, alpha_i, beta_i, delta_i, lambda_i)
            },
            "ekw" = {
              result[i] <- dekw(y_i, alpha_i, beta_i, lambda_i)
            },
            "mc" = {
              result[i] <- dmc(y_i, gamma_i, delta_i, lambda_i)
            },
            "kw" = {
              result[i] <- dkw(y_i, alpha_i, beta_i)
            },
            "beta" = {
              result[i] <- dbeta_(y_i, gamma_i, delta_i)
            }
          )
        } else if (type == "probability") {
          # CDF functions (p-prefix)
          switch(family,
            "gkw" = {
              result[i] <- pgkw(y_i, alpha_i, beta_i, gamma_i, delta_i, lambda_i)
            },
            "bkw" = {
              result[i] <- pbkw(y_i, alpha_i, beta_i, gamma_i, delta_i)
            },
            "kkw" = {
              result[i] <- pkkw(y_i, alpha_i, beta_i, delta_i, lambda_i)
            },
            "ekw" = {
              result[i] <- pekw(y_i, alpha_i, beta_i, lambda_i)
            },
            "mc" = {
              result[i] <- pmc(y_i, gamma_i, delta_i, lambda_i)
            },
            "kw" = {
              result[i] <- pkw(y_i, alpha_i, beta_i)
            },
            "beta" = {
              result[i] <- pbeta_(y_i, gamma_i, delta_i)
            }
          )
        } else if (type == "quantile") {
          # Quantile functions (q-prefix)
          switch(family,
            "gkw" = {
              result[i] <- qgkw(y_i, alpha_i, beta_i, gamma_i, delta_i, lambda_i)
            },
            "bkw" = {
              result[i] <- qbkw(y_i, alpha_i, beta_i, gamma_i, delta_i)
            },
            "kkw" = {
              result[i] <- qkkw(y_i, alpha_i, beta_i, delta_i, lambda_i)
            },
            "ekw" = {
              result[i] <- qekw(y_i, alpha_i, beta_i, lambda_i)
            },
            "mc" = {
              result[i] <- qmc(y_i, gamma_i, delta_i, lambda_i)
            },
            "kw" = {
              result[i] <- qkw(y_i, alpha_i, beta_i)
            },
            "beta" = {
              result[i] <- qbeta_(y_i, gamma_i, delta_i)
            }
          )
        }
      }

      if (elementwise) {
        return(result)
      } else {
        result_matrix <- matrix(result, nrow = n, ncol = length(at))
        colnames(result_matrix) <- as.character(at)
        return(result_matrix)
      }
    }
  }

  # Special case for type = "variance" - implement direct calculation
  if (type == "variance" && is.null(newdata) &&
    !is.null(object$parameter_vectors) && !is.null(object$fitted.values)) {
    n <- length(object$fitted.values)
    variances <- numeric(n)

    # Get needed parameters
    alpha_vec <- object$parameter_vectors$alphaVec
    beta_vec <- object$parameter_vectors$betaVec
    gamma_vec <- object$parameter_vectors$gammaVec
    delta_vec <- object$parameter_vectors$deltaVec
    lambda_vec <- object$parameter_vectors$lambdaVec
    means <- object$fitted.values

    # Calculate variance for each observation
    for (i in 1:n) {
      if (family == "beta") {
        # Analytical formula for Beta distribution
        variances[i] <- (gamma_vec[i] * delta_vec[i]) /
          ((gamma_vec[i] + delta_vec[i])^2 * (gamma_vec[i] + delta_vec[i] + 1))
      } else if (family == "kw") {
        # Approximate formula for Kumaraswamy distribution
        variances[i] <- alpha_vec[i] * beta_vec[i] *
          beta(1 + 1 / alpha_vec[i], beta_vec[i]) -
          (alpha_vec[i] * beta_vec[i] * beta(1 + 1 / alpha_vec[i], beta_vec[i]))^2
      } else {
        # Numerical approximation using density function with proper prefix
        h <- 0.001
        mean_i <- means[i]

        # Safely compute y within (0,1)
        mean_plus <- min(0.99999, mean_i + h)
        mean_minus <- max(0.00001, mean_i - h)

        # Use the appropriate density function for the family
        switch(family,
          "gkw" = {
            f_plus <- dgkw(mean_plus, alpha_vec[i], beta_vec[i], gamma_vec[i], delta_vec[i], lambda_vec[i])
            f <- dgkw(mean_i, alpha_vec[i], beta_vec[i], gamma_vec[i], delta_vec[i], lambda_vec[i])
            f_minus <- dgkw(mean_minus, alpha_vec[i], beta_vec[i], gamma_vec[i], delta_vec[i], lambda_vec[i])
          },
          "bkw" = {
            f_plus <- dbkw(mean_plus, alpha_vec[i], beta_vec[i], gamma_vec[i], delta_vec[i])
            f <- dbkw(mean_i, alpha_vec[i], beta_vec[i], gamma_vec[i], delta_vec[i])
            f_minus <- dbkw(mean_minus, alpha_vec[i], beta_vec[i], gamma_vec[i], delta_vec[i])
          },
          "kkw" = {
            f_plus <- dkkw(mean_plus, alpha_vec[i], beta_vec[i], delta_vec[i], lambda_vec[i])
            f <- dkkw(mean_i, alpha_vec[i], beta_vec[i], delta_vec[i], lambda_vec[i])
            f_minus <- dkkw(mean_minus, alpha_vec[i], beta_vec[i], delta_vec[i], lambda_vec[i])
          },
          "ekw" = {
            f_plus <- dekw(mean_plus, alpha_vec[i], beta_vec[i], lambda_vec[i])
            f <- dekw(mean_i, alpha_vec[i], beta_vec[i], lambda_vec[i])
            f_minus <- dekw(mean_minus, alpha_vec[i], beta_vec[i], lambda_vec[i])
          },
          "mc" = {
            f_plus <- dmc(mean_plus, gamma_vec[i], delta_vec[i], lambda_vec[i])
            f <- dmc(mean_i, gamma_vec[i], delta_vec[i], lambda_vec[i])
            f_minus <- dmc(mean_minus, gamma_vec[i], delta_vec[i], lambda_vec[i])
          }
        )

        # Approximate second derivative of log-likelihood
        d2ll <- (f_plus - 2 * f + f_minus) / (h * h)

        # Variance is inverse of the information
        variances[i] <- min(0.25, max(1e-6, 1 / (abs(d2ll) + 1e-10)))
      }
    }

    return(variances)
  }

  # Special case for type = "link" when newdata is NULL
  if (type == "link" && is.null(newdata)) {
    if (!is.null(object$tmb_object) && !is.null(object$tmb_object$env$data)) {
      # Try to extract linear predictors from TMB object if available
      tmb_data <- object$tmb_object$env$data
      if (!is.null(tmb_data$X1) && !is.null(tmb_data$X2) &&
        !is.null(tmb_data$X3) && !is.null(tmb_data$X4) &&
        !is.null(tmb_data$X5)) {
        # Extract coefficients
        coefs <- object$coefficients
        if (is.list(coefs)) {
          beta1 <- coefs$alpha
          beta2 <- coefs$beta
          beta3 <- coefs$gamma
          beta4 <- coefs$delta
          beta5 <- coefs$lambda
        } else {
          # Try using regex patterns to extract coefficients
          beta1 <- coefs[grep("^alpha:", names(coefs))]
          beta2 <- coefs[grep("^beta:", names(coefs))]
          beta3 <- coefs[grep("^gamma:", names(coefs))]
          beta4 <- coefs[grep("^delta:", names(coefs))]
          beta5 <- coefs[grep("^lambda:", names(coefs))]

          # If regex didn't work, try to infer from dimensions
          if (length(beta1) == 0 || length(beta2) == 0) {
            # Extract design matrices
            X1 <- tmb_data$X1
            X2 <- tmb_data$X2
            X3 <- tmb_data$X3
            X4 <- tmb_data$X4
            X5 <- tmb_data$X5

            # Split coefficients based on matrix dimensions
            all_coefs <- coefs
            beta1 <- all_coefs[1:ncol(X1)]
            remaining <- all_coefs[-(1:ncol(X1))]

            beta2 <- remaining[1:ncol(X2)]
            remaining <- remaining[-(1:ncol(X2))]

            beta3 <- remaining[1:ncol(X3)]
            remaining <- remaining[-(1:ncol(X3))]

            beta4 <- remaining[1:ncol(X4)]
            remaining <- remaining[-(1:ncol(X4))]

            beta5 <- remaining[1:ncol(X5)]
          }
        }

        # Calculate linear predictors using TMB data matrices
        X1 <- tmb_data$X1
        X2 <- tmb_data$X2
        X3 <- tmb_data$X3
        X4 <- tmb_data$X4
        X5 <- tmb_data$X5

        eta1 <- as.vector(X1 %*% beta1)
        eta2 <- as.vector(X2 %*% beta2)
        eta3 <- as.vector(X3 %*% beta3)
        eta4 <- as.vector(X4 %*% beta4)
        eta5 <- as.vector(X5 %*% beta5)

        return(data.frame(
          alpha = eta1,
          beta = eta2,
          gamma = eta3,
          delta = eta4,
          lambda = eta5
        ))
      }
    }

    # If we can't extract linear predictors, return a warning and parameter values instead
    if (!is.null(object$parameter_vectors)) {
      warning("Cannot calculate link values without design matrices. Returning parameter values instead.")
      return(data.frame(
        alpha = object$parameter_vectors$alphaVec,
        beta = object$parameter_vectors$betaVec,
        gamma = object$parameter_vectors$gammaVec,
        delta = object$parameter_vectors$deltaVec,
        lambda = object$parameter_vectors$lambdaVec
      ))
    } else {
      stop("Cannot extract linear predictors. Please provide 'newdata' or set x=TRUE when fitting the model.")
    }
  }

  # Get parameter information for the family
  param_info <- .get_family_param_info(family)
  param_names <- param_info$names
  param_positions <- param_info$positions
  fixed_params <- param_info$fixed

  # Identify non-fixed parameters for this family
  non_fixed_params <- setdiff(param_names, names(fixed_params))

  # Prepare model matrices for prediction
  if (is.null(newdata)) {
    # Try different approaches to access model matrices
    if (!is.null(object$x)) {
      # Model matrices stored directly (fitted with x=TRUE)
      matrices <- list()
      for (param in param_names) {
        pos <- param_positions[[param]]
        if (param %in% names(object$x)) {
          matrices[[paste0("X", pos)]] <- object$x[[param]]
        } else {
          # Default intercept-only matrix for missing parameters
          n_obs <- length(object$y)
          matrices[[paste0("X", pos)]] <- matrix(1, n_obs, 1,
            dimnames = list(NULL, "(Intercept)")
          )
        }
      }
    } else if (!is.null(object$model) && !is.null(object$formula)) {
      # Reconstruct matrices from model frame and formula
      matrices <- list()
      formula_obj <- object$formula
      mf <- object$model

      # Ensure formula is a Formula object
      if (!inherits(formula_obj, "Formula")) {
        formula_obj <- Formula::as.Formula(formula_obj)
      }

      # Get number of RHS parts
      n_parts <- length(attr(Formula::Formula(formula_obj), "rhs"))

      # Create matrix for each parameter
      for (i in seq_along(param_names)) {
        param <- param_names[i]
        pos <- param_positions[[param]]

        if (i <= n_parts) {
          # Try to extract matrix for this formula part
          tryCatch(
            {
              X_i <- stats::model.matrix(formula_obj, data = mf, rhs = i)
              matrices[[paste0("X", pos)]] <- X_i
            },
            error = function(e) {
              # Default to intercept-only if extraction fails
              matrices[[paste0("X", pos)]] <- matrix(1, nrow(mf), 1,
                dimnames = list(NULL, "(Intercept)")
              )
            }
          )
        } else {
          # Default to intercept-only for parts beyond those specified
          matrices[[paste0("X", pos)]] <- matrix(1, nrow(mf), 1,
            dimnames = list(NULL, "(Intercept)")
          )
        }
      }
    } else if (!is.null(object$tmb_object) && !is.null(object$tmb_object$env$data)) {
      # Use matrices from TMB object
      tmb_data <- object$tmb_object$env$data
      matrices <- list()

      for (param in param_names) {
        pos <- param_positions[[param]]
        matrix_name <- paste0("X", pos)

        if (!is.null(tmb_data[[matrix_name]])) {
          matrices[[matrix_name]] <- tmb_data[[matrix_name]]
        }
      }
    } else {
      stop("Cannot extract model matrices. Please provide 'newdata' or set x=TRUE when fitting the model.")
    }
  } else {
    # Create model matrices from newdata
    matrices <- list()
    formula <- object$formula

    # Ensure formula is a Formula object
    if (!inherits(formula, "Formula")) {
      formula <- Formula::as.Formula(formula)
    }

    # Get number of RHS parts
    n_parts <- length(attr(Formula::Formula(formula), "rhs"))

    # Process each part of the formula for each parameter
    for (i in seq_along(param_names)) {
      param <- param_names[i]
      pos <- param_positions[[param]]

      if (i <= n_parts) {
        # Extract terms for this formula part
        component_terms <- stats::delete.response(stats::terms(formula, rhs = i))

        # Create model frame and matrix
        tryCatch(
          {
            mf_i <- stats::model.frame(component_terms, newdata,
              na.action = na.action,
              xlev = object$xlevels
            )

            X_i <- stats::model.matrix(component_terms, mf_i)

            if (ncol(X_i) > 0) {
              matrices[[paste0("X", pos)]] <- X_i
            } else {
              matrices[[paste0("X", pos)]] <- matrix(1, nrow(newdata), 1,
                dimnames = list(NULL, "(Intercept)")
              )
            }
          },
          error = function(e) {
            # Use intercept-only matrix if model frame creation fails
            matrices[[paste0("X", pos)]] <- matrix(1, nrow(newdata), 1,
              dimnames = list(NULL, "(Intercept)")
            )
          }
        )
      } else {
        # Default intercept-only matrix for parts beyond those in formula
        matrices[[paste0("X", pos)]] <- matrix(1, nrow(newdata), 1,
          dimnames = list(NULL, "(Intercept)")
        )
      }
    }
  }

  # Fill in any missing matrices with intercept-only
  for (i in 1:max(unlist(param_positions))) {
    matrix_name <- paste0("X", i)
    if (is.null(matrices[[matrix_name]])) {
      n_obs <- nrow(matrices[[1]])
      matrices[[matrix_name]] <- matrix(1, n_obs, 1, dimnames = list(NULL, "(Intercept)"))
    }
  }

  # Extract coefficients for each parameter
  coefs <- list()

  if (is.list(object$coefficients) && !is.null(names(object$coefficients))) {
    # If coefficients are in a named list
    for (param in param_names) {
      if (param %in% names(object$coefficients)) {
        coefs[[param]] <- object$coefficients[[param]]
      } else if (param %in% names(fixed_params)) {
        # Use fixed value for fixed parameters
        coefs[[param]] <- fixed_params[[param]]
      }
    }
  } else {
    # If coefficients are in a vector, extract using parameter names pattern
    all_coefs <- object$coefficients

    for (param in param_names) {
      # Try to find coefficients for this parameter using pattern matching
      param_pattern <- paste0("^", param, ":")
      param_coefs <- all_coefs[grep(param_pattern, names(all_coefs))]

      if (length(param_coefs) > 0) {
        coefs[[param]] <- param_coefs
      } else if (param %in% names(fixed_params)) {
        # Use fixed value for fixed parameters
        coefs[[param]] <- fixed_params[[param]]
      }
    }

    # If pattern matching didn't work, try positional approach using matrix dimensions
    if (all(sapply(coefs, length) == 0) && !is.null(all_coefs)) {
      remaining_coefs <- all_coefs

      for (param in param_names) {
        if (param %in% names(fixed_params)) {
          coefs[[param]] <- fixed_params[[param]]
          next
        }

        pos <- param_positions[[param]]
        X <- matrices[[paste0("X", pos)]]

        if (!is.null(X) && ncol(X) > 0 && length(remaining_coefs) >= ncol(X)) {
          coefs[[param]] <- remaining_coefs[1:ncol(X)]
          remaining_coefs <- remaining_coefs[-(1:ncol(X))]
        }
      }
    }
  }

  # Check if any coefficients are missing
  missing_coefs <- setdiff(non_fixed_params, names(coefs)[sapply(coefs, length) > 0])
  if (length(missing_coefs) > 0) {
    stop(
      "Could not extract coefficients for parameter(s): ",
      paste(missing_coefs, collapse = ", ")
    )
  }

  # Extract link functions
  link_types <- rep(NA, 5)
  names(link_types) <- c("alpha", "beta", "gamma", "delta", "lambda")

  if (!is.null(object$link)) {
    # Link functions are stored directly
    link_map <- c(
      "log" = 1,
      "logit" = 2,
      "probit" = 3,
      "cauchy" = 4,
      "cloglog" = 5,
      "identity" = 6,
      "sqrt" = 7,
      "inverse" = 8,
      "inverse-square" = 9
    )

    for (param in names(object$link)) {
      if (param %in% c("alpha", "beta", "gamma", "delta", "lambda")) {
        link_types[param] <- link_map[object$link[[param]]]
      }
    }
  }

  # Fill in default link types for any missing ones
  if (is.na(link_types["alpha"])) link_types["alpha"] <- 1 # log
  if (is.na(link_types["beta"])) link_types["beta"] <- 1 # log
  if (is.na(link_types["gamma"])) link_types["gamma"] <- 1 # log
  if (is.na(link_types["delta"])) link_types["delta"] <- 2 # logit
  if (is.na(link_types["lambda"])) link_types["lambda"] <- 1 # log

  # Extract scale factors (or use defaults)
  scale_factors <- c(10, 10, 10, 1, 10) # Default scale factors

  # If the type is "link", calculate and return the linear predictors
  if (type == "link") {
    # Initialize results
    n_obs <- nrow(matrices[[1]])
    result <- data.frame(
      alpha = rep(NA_real_, n_obs),
      beta = rep(NA_real_, n_obs),
      gamma = rep(NA_real_, n_obs),
      delta = rep(NA_real_, n_obs),
      lambda = rep(NA_real_, n_obs)
    )

    # Calculate linear predictor for each parameter
    for (param in param_names) {
      if (param %in% names(fixed_params)) next

      pos <- param_positions[[param]]
      X <- matrices[[paste0("X", pos)]]
      beta <- coefs[[param]]

      # Match dimensions of X and beta
      if (length(beta) != ncol(X)) {
        warning(
          "Dimension mismatch for ", param, ": X has ", ncol(X),
          " columns but beta has ", length(beta), " elements."
        )
        next
      }

      result[[param]] <- as.vector(X %*% beta)
    }

    return(result)
  }

  # For the other types, calculate distribution parameters on original scale
  params <- matrix(0, nrow = nrow(matrices[[1]]), ncol = 5)
  colnames(params) <- c("alpha", "beta", "gamma", "delta", "lambda")

  # Calculate parameters for each observation
  for (param in c("alpha", "beta", "gamma", "delta", "lambda")) {
    param_idx <- match(param, c("alpha", "beta", "gamma", "delta", "lambda"))

    if (param %in% names(fixed_params)) {
      # Fixed parameter - same value for all observations
      params[, param_idx] <- fixed_params[[param]]
    } else if (param %in% param_names) {
      # Calculate from model matrix and coefficients
      pos <- param_positions[[param]]
      X <- matrices[[paste0("X", pos)]]
      beta <- coefs[[param]]
      link_type <- link_types[param]

      # Check dimension compatibility
      if (length(beta) != ncol(X)) {
        stop(
          "Dimension mismatch for ", param, ": X has ", ncol(X),
          " columns but beta has ", length(beta), " elements."
        )
      }

      # Calculate linear predictor
      eta <- as.vector(X %*% beta)

      # Apply inverse link function
      params[, param_idx] <- switch(as.character(link_type),
        "1" = exp(eta), # log
        "2" = 1 / (1 + exp(-eta)), # logit
        "3" = pnorm(eta), # probit
        "4" = 0.5 + (1 / pi) * atan(eta), # cauchy
        "5" = 1 - exp(-exp(eta)), # cloglog
        "6" = eta, # identity
        "7" = eta^2, # sqrt
        "8" = 1 / eta, # inverse
        "9" = 1 / sqrt(eta), # inverse-square
        exp(eta) # default to log
      )
    }
  }

  # Apply family-specific constraints to ensure proper parameter ranges
  switch(family,
    "gkw" = {
      # All parameters used, no extra constraints needed
    },
    "bkw" = {
      # lambda = 1 fixed
      params[, "lambda"] <- 1
    },
    "kkw" = {
      # gamma = 1 fixed
      params[, "gamma"] <- 1
    },
    "ekw" = {
      # gamma = 1, delta = 0 fixed
      params[, "gamma"] <- 1
      params[, "delta"] <- 0
    },
    "mc" = {
      # alpha = 1, beta = 1 fixed
      params[, "alpha"] <- 1
      params[, "beta"] <- 1
    },
    "kw" = {
      # gamma = 1, delta = 0, lambda = 1 fixed
      params[, "gamma"] <- 1
      params[, "delta"] <- 0
      params[, "lambda"] <- 1
    },
    "beta" = {
      # alpha = 1, beta = 1, lambda = 1 fixed
      params[, "alpha"] <- 1
      params[, "beta"] <- 1
      params[, "lambda"] <- 1
    }
  )

  # Handle remaining parameter range constraints
  params[, "alpha"] <- pmax(1e-7, params[, "alpha"])
  params[, "beta"] <- pmax(1e-7, params[, "beta"])
  params[, "gamma"] <- pmax(1e-7, params[, "gamma"])
  params[, "delta"] <- pmax(1e-7, pmin(1 - 1e-7, params[, "delta"]))
  params[, "lambda"] <- pmax(1e-7, params[, "lambda"])

  # Return the parameters if requested
  if (type == "parameter") {
    return(data.frame(
      alpha = params[, "alpha"],
      beta = params[, "beta"],
      gamma = params[, "gamma"],
      delta = params[, "delta"],
      lambda = params[, "lambda"]
    ))
  } else if (type %in% c("alpha", "beta", "gamma", "delta", "lambda")) {
    param_idx <- match(type, c("alpha", "beta", "gamma", "delta", "lambda"))
    return(params[, param_idx])
  }

  # Calculate mean response
  if (type == "response") {
    means <- calculateMeans(params, family = family)
    return(means)
  } else if (type == "variance") {
    # Calculate means first
    means <- calculateMeans(params, family = family)
    n_obs <- nrow(params)

    # Calculate variance for each observation
    variances <- numeric(n_obs)

    for (i in 1:n_obs) {
      alpha_i <- params[i, "alpha"]
      beta_i <- params[i, "beta"]
      gamma_i <- params[i, "gamma"]
      delta_i <- params[i, "delta"]
      lambda_i <- params[i, "lambda"]

      # Different variance approximations based on family
      if (family == "beta") {
        variances[i] <- (gamma_i * delta_i) /
          ((gamma_i + delta_i)^2 * (gamma_i + delta_i + 1))
      } else if (family == "kw") {
        variances[i] <- alpha_i * beta_i *
          beta(1 + 1 / alpha_i, beta_i) -
          (alpha_i * beta_i * beta(1 + 1 / alpha_i, beta_i))^2
      } else {
        # Numerical approximation
        h <- 0.001
        mean_i <- means[i]

        # Safely compute for y within (0,1)
        mean_plus <- min(0.999, mean_i + h)
        mean_minus <- max(0.001, mean_i - h)

        # Use the appropriate density function based on family
        switch(family,
          "gkw" = {
            f_plus <- dgkw(mean_plus, alpha_i, beta_i, gamma_i, delta_i, lambda_i)
            f <- dgkw(mean_i, alpha_i, beta_i, gamma_i, delta_i, lambda_i)
            f_minus <- dgkw(mean_minus, alpha_i, beta_i, gamma_i, delta_i, lambda_i)
          },
          "bkw" = {
            f_plus <- dbkw(mean_plus, alpha_i, beta_i, gamma_i, delta_i)
            f <- dbkw(mean_i, alpha_i, beta_i, gamma_i, delta_i)
            f_minus <- dbkw(mean_minus, alpha_i, beta_i, gamma_i, delta_i)
          },
          "kkw" = {
            f_plus <- dkkw(mean_plus, alpha_i, beta_i, delta_i, lambda_i)
            f <- dkkw(mean_i, alpha_i, beta_i, delta_i, lambda_i)
            f_minus <- dkkw(mean_minus, alpha_i, beta_i, delta_i, lambda_i)
          },
          "ekw" = {
            f_plus <- dekw(mean_plus, alpha_i, beta_i, lambda_i)
            f <- dekw(mean_i, alpha_i, beta_i, lambda_i)
            f_minus <- dekw(mean_minus, alpha_i, beta_i, lambda_i)
          },
          "mc" = {
            f_plus <- dmc(mean_plus, gamma_i, delta_i, lambda_i)
            f <- dmc(mean_i, gamma_i, delta_i, lambda_i)
            f_minus <- dmc(mean_minus, gamma_i, delta_i, lambda_i)
          }
        )

        # Approximate second derivative of log-likelihood
        d2ll <- (f_plus - 2 * f + f_minus) / (h * h)

        # Variance is inverse of the information
        variances[i] <- min(0.25, max(1e-6, 1 / (abs(d2ll) + 1e-10)))
      }
    }

    return(variances)
  }

  # For the remaining types (density, probability, quantile), we need the 'at' argument
  if (!is.numeric(at)) {
    stop("'at' must be numeric")
  }

  # Check if the elementwise mode should be used
  if (is.null(elementwise)) {
    elementwise <- length(at) == nrow(params)
  }

  n_obs <- nrow(params)

  # Prepare data for calculation based on elementwise mode
  if (elementwise) {
    if (length(at) != n_obs) {
      stop("For elementwise=TRUE, length of 'at' must equal number of observations")
    }
    eval_y <- at
    eval_params <- params
  } else {
    # Repeat params for each value in 'at'
    eval_params <- do.call(rbind, lapply(seq_len(length(at)), function(i) params))
    # Repeat each value of 'at' for each row of params
    eval_y <- rep(at, each = n_obs)
  }

  # Calculate the requested quantities using appropriate distribution functions
  result <- numeric(length(eval_y))

  for (i in seq_along(eval_y)) {
    alpha_i <- eval_params[i, "alpha"]
    beta_i <- eval_params[i, "beta"]
    gamma_i <- eval_params[i, "gamma"]
    delta_i <- eval_params[i, "delta"]
    lambda_i <- eval_params[i, "lambda"]
    y_i <- eval_y[i]

    # Use the appropriate distribution function based on family and type
    if (type == "density") {
      switch(family,
        "gkw" = {
          result[i] <- dgkw(y_i, alpha_i, beta_i, gamma_i, delta_i, lambda_i)
        },
        "bkw" = {
          result[i] <- dbkw(y_i, alpha_i, beta_i, gamma_i, delta_i)
        },
        "kkw" = {
          result[i] <- dkkw(y_i, alpha_i, beta_i, delta_i, lambda_i)
        },
        "ekw" = {
          result[i] <- dekw(y_i, alpha_i, beta_i, lambda_i)
        },
        "mc" = {
          result[i] <- dmc(y_i, gamma_i, delta_i, lambda_i)
        },
        "kw" = {
          result[i] <- dkw(y_i, alpha_i, beta_i)
        },
        "beta" = {
          result[i] <- dbeta_(y_i, gamma_i, delta_i)
        }
      )
    } else if (type == "probability") {
      switch(family,
        "gkw" = {
          result[i] <- pgkw(y_i, alpha_i, beta_i, gamma_i, delta_i, lambda_i)
        },
        "bkw" = {
          result[i] <- pbkw(y_i, alpha_i, beta_i, gamma_i, delta_i)
        },
        "kkw" = {
          result[i] <- pkkw(y_i, alpha_i, beta_i, delta_i, lambda_i)
        },
        "ekw" = {
          result[i] <- pekw(y_i, alpha_i, beta_i, lambda_i)
        },
        "mc" = {
          result[i] <- pmc(y_i, gamma_i, delta_i, lambda_i)
        },
        "kw" = {
          result[i] <- pkw(y_i, alpha_i, beta_i)
        },
        "beta" = {
          result[i] <- pbeta_(y_i, gamma_i, delta_i)
        }
      )
    } else if (type == "quantile") {
      switch(family,
        "gkw" = {
          result[i] <- qgkw(y_i, alpha_i, beta_i, gamma_i, delta_i, lambda_i)
        },
        "bkw" = {
          result[i] <- qbkw(y_i, alpha_i, beta_i, gamma_i, delta_i)
        },
        "kkw" = {
          result[i] <- qkkw(y_i, alpha_i, beta_i, delta_i, lambda_i)
        },
        "ekw" = {
          result[i] <- qekw(y_i, alpha_i, beta_i, lambda_i)
        },
        "mc" = {
          result[i] <- qmc(y_i, gamma_i, delta_i, lambda_i)
        },
        "kw" = {
          result[i] <- qkw(y_i, alpha_i, beta_i)
        },
        "beta" = {
          result[i] <- qbeta_(y_i, gamma_i, delta_i + 1)
        }
      )
    }
  }

  # Format the result according to elementwise mode
  if (elementwise) {
    return(result)
  } else {
    result_matrix <- matrix(result, nrow = n_obs, ncol = length(at))
    colnames(result_matrix) <- as.character(at)
    return(result_matrix)
  }
}
