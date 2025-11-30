#' @title Extract Fitted Values from a Generalized Kumaraswamy Regression Model
#'
#' @description
#' Extracts the fitted mean values (predicted expected values of the response)
#' from a fitted Generalized Kumaraswamy (GKw) regression model object of class
#' \code{"gkwreg"}. This is an S3 method for the generic
#' \code{\link[stats]{fitted.values}} function.
#'
#' @param object An object of class \code{"gkwreg"}, typically the result of a
#'   call to \code{\link{gkwreg}}.
#' @param family Character string specifying the distribution family under which
#'   the fitted mean values should be calculated. If \code{NULL} (default), the
#'   family stored within the fitted \code{object} is used. Specifying a different
#'   family (e.g., \code{"beta"}) will trigger recalculation of the fitted means
#'   based on that family's mean structure, using the original model's estimated
#'   coefficients mapped to the relevant parameters. Available options match those
#'   in \code{\link{gkwreg}}: \code{"gkw", "bkw", "kkw", "ekw", "mc", "kw", "beta"}.
#' @param ... Additional arguments, currently ignored by this method.
#'
#' @details
#' This function retrieves or calculates the fitted values, which represent the
#' estimated conditional mean of the response variable given the covariates
#' (\eqn{E(Y | X)}).
#'
#' The function attempts to retrieve fitted values efficiently using the following
#' priority:
#' \enumerate{
#'   \item Directly from the \code{fitted.values} component stored in the \code{object},
#'     if available and complete. It includes logic to handle potentially
#'     incomplete stored values via interpolation (\code{\link[stats]{approx}}) for
#'     very large datasets where only a sample might be stored.
#'   \item By recalculating the mean using stored parameter vectors for each
#'     observation (\code{object$parameter_vectors}) and an internal function
#'     (\code{calculateMeans}), if available.
#'   \item From the \code{fitted} component within the TMB report (\code{object$tmb_object$report()}),
#'     if available, potentially using interpolation as above.
#'   \item As a fallback, by calling \code{predict(object, type = "response", family = family)}.
#' }
#' Specifying a \code{family} different from the one used to fit the model will
#' always force recalculation using the \code{predict} method (step 4).
#'
#' @return A numeric vector containing the fitted mean values. These values are
#'   typically bounded between 0 and 1, corresponding to the scale of the original
#'   response variable. The length of the vector corresponds to the number of
#'   observations used in the model fit (considering \code{subset} and \code{na.action}).
#'
#' @author Lopes, J. E.
#'
#' @seealso \code{\link{gkwreg}}, \code{\link{predict.gkwreg}},
#'   \code{\link{residuals.gkwreg}}, \code{\link[stats]{fitted.values}}
#'
#' @keywords fitted methods regression
#'
#' @examples
#' \donttest{
#' require(gkwreg)
#' require(gkwdist)
#'
#' # Example 1: Basic usage with FoodExpenditure data
#' data(FoodExpenditure)
#' FoodExpenditure$prop <- FoodExpenditure$food / FoodExpenditure$income
#'
#' fit_kw <- gkwreg(prop ~ income + persons | income,
#'   data = FoodExpenditure,
#'   family = "kw"
#' )
#'
#' # Extract fitted values
#' fitted_vals <- fitted(fit_kw)
#'
#' # Visualize fit quality
#' plot(FoodExpenditure$prop, fitted_vals,
#'   xlab = "Observed Proportion",
#'   ylab = "Fitted Values",
#'   main = "Observed vs Fitted: Food Expenditure",
#'   pch = 19, col = rgb(0, 0, 1, 0.5)
#' )
#' abline(0, 1, col = "red", lwd = 2)
#'
#' # Calculate R-squared analogue
#' cor(FoodExpenditure$prop, fitted_vals)^2
#'
#' # Example 2: Comparing fitted values across families
#' data(GasolineYield)
#'
#' fit_ekw <- gkwreg(yield ~ batch + temp | temp | batch,
#'   data = GasolineYield,
#'   family = "ekw"
#' )
#'
#' # Fitted values under different family assumptions
#' fitted_ekw <- fitted(fit_ekw)
#' fitted_kw <- fitted(fit_ekw, family = "kw")
#' fitted_beta <- fitted(fit_ekw, family = "beta")
#'
#' # Compare differences
#' comparison <- data.frame(
#'   EKW = fitted_ekw,
#'   KW = fitted_kw,
#'   Beta = fitted_beta,
#'   Diff_EKW_KW = fitted_ekw - fitted_kw,
#'   Diff_EKW_Beta = fitted_ekw - fitted_beta
#' )
#' head(comparison)
#'
#' # Visualize differences
#' par(mfrow = c(1, 2))
#' plot(fitted_ekw, fitted_kw,
#'   xlab = "EKW Fitted", ylab = "KW Fitted",
#'   main = "EKW vs KW Family Assumptions",
#'   pch = 19, col = "darkblue"
#' )
#' abline(0, 1, col = "red", lty = 2)
#'
#' plot(fitted_ekw, fitted_beta,
#'   xlab = "EKW Fitted", ylab = "Beta Fitted",
#'   main = "EKW vs Beta Family Assumptions",
#'   pch = 19, col = "darkgreen"
#' )
#' abline(0, 1, col = "red", lty = 2)
#' par(mfrow = c(1, 1))
#'
#' # Example 3: Diagnostic plot with confidence bands
#' data(ReadingSkills)
#'
#' fit_mc <- gkwreg(
#'   accuracy ~ dyslexia * iq | dyslexia + iq | dyslexia,
#'   data = ReadingSkills,
#'   family = "mc"
#' )
#'
#' fitted_vals <- fitted(fit_mc)
#'
#' # Residual plot
#' residuals_resp <- ReadingSkills$accuracy - fitted_vals
#'
#' plot(fitted_vals, residuals_resp,
#'   xlab = "Fitted Values",
#'   ylab = "Raw Residuals",
#'   main = "Residual Plot: Reading Accuracy",
#'   pch = 19, col = ReadingSkills$dyslexia,
#'   ylim = range(residuals_resp) * 1.2
#' )
#' abline(h = 0, col = "red", lwd = 2, lty = 2)
#' lowess_fit <- lowess(fitted_vals, residuals_resp)
#' lines(lowess_fit, col = "blue", lwd = 2)
#' legend("topright",
#'   legend = c("Control", "Dyslexic", "Zero Line", "Lowess"),
#'   col = c("black", "red", "red", "blue"),
#'   pch = c(19, 19, NA, NA),
#'   lty = c(NA, NA, 2, 1),
#'   lwd = c(NA, NA, 2, 2)
#' )
#'
#' # Example 4: Large dataset efficiency check
#' set.seed(2024)
#' n <- 5000
#' x1 <- rnorm(n)
#' x2 <- runif(n, -2, 2)
#' alpha <- exp(0.3 + 0.5 * x1)
#' beta <- exp(1.2 - 0.4 * x2)
#' y <- rkw(n, alpha, beta)
#' large_data <- data.frame(y = y, x1 = x1, x2 = x2)
#'
#' fit_large <- gkwreg(y ~ x1 | x2,
#'   data = large_data,
#'   family = "kw"
#' )
#'
#' # Time the extraction
#' system.time({
#'   fitted_large <- fitted(fit_large)
#' })
#'
#' # Verify extraction
#' length(fitted_large)
#' summary(fitted_large)
#' }
#'
#' @export
fitted.gkwreg <- function(object, family = NULL, ...) {
  # Check if object is of class "gkwreg"
  if (!inherits(object, "gkwreg")) {
    stop("'object' must be a fitted model of class \"gkwreg\"")
  }

  # Get the family from the object if not specified
  if (is.null(family)) {
    if (!is.null(object$family)) {
      family <- object$family
    } else {
      # Default to gkw for backward compatibility
      family <- "gkw"
      message("No family specified in the model. Using 'gkw' as default.")
    }
  } else {
    # Validate the family parameter
    family <- match.arg(family, c("gkw", "bkw", "kkw", "ekw", "mc", "kw", "beta"))

    # If family is different from the model's family, we need to recalculate
    if (!is.null(object$family) && family != object$family) {
      message(paste0(
        "Using different family (", family, ") than what was used to fit the model (",
        object$family, "). Recalculating fitted values..."
      ))

      # Use predict to calculate fitted values with the new family
      return(stats::predict(object, type = "response", family = family))
    }
  }

  # Determine the number of observations from various possible sources
  n <- if (!is.null(object$nobs)) {
    object$nobs
  } else if (!is.null(object$y)) {
    length(object$y)
  } else if (!is.null(object$model) && !is.null(object$model$y)) {
    length(object$model$y)
  } else if (!is.null(object$parameter_vectors) && !is.null(object$parameter_vectors$alphaVec)) {
    length(object$parameter_vectors$alphaVec)
  } else {
    stop("Cannot determine the number of observations in the model")
  }

  # Method 1: Try to get fitted values directly from the model object
  if (!is.null(object$fitted.values)) {
    fitted_len <- length(object$fitted.values)

    # Check if dimensions match the expected number of observations
    if (fitted_len == n) {
      return(object$fitted.values)
    } else if (fitted_len == 1 && n > 1) {
      message("Only one fitted value found in model object. Recalculating all fitted values...")
    } else if (fitted_len > 1 && fitted_len < n && n > 10000) {
      # For very large datasets, we might have a sample of fitted values - try to interpolate
      message("Partial fitted values found. Interpolating...")

      # Create fitted values vector with NAs
      fitted_values <- rep(NA_real_, n)

      # Map available values to appropriate indices
      sample_size <- fitted_len
      sample_idx <- floor(seq(1, n, length.out = sample_size))
      fitted_values[sample_idx] <- object$fitted.values

      # Interpolate the rest
      idx_with_values <- which(!is.na(fitted_values))
      fitted_values <- stats::approx(
        x = idx_with_values,
        y = fitted_values[idx_with_values],
        xout = seq_len(n),
        rule = 2
      )$y

      return(fitted_values)
    }
  }

  # Method 2: Try to use the parameter vectors if available
  if (!is.null(object$parameter_vectors)) {
    # Check if all necessary parameter vectors are available
    param_vec <- object$parameter_vectors
    if (!is.null(param_vec$alphaVec) &&
      !is.null(param_vec$betaVec) &&
      !is.null(param_vec$gammaVec) &&
      !is.null(param_vec$deltaVec) &&
      !is.null(param_vec$lambdaVec)) {
      # Check if dimensions match
      param_lengths <- c(
        length(param_vec$alphaVec),
        length(param_vec$betaVec),
        length(param_vec$gammaVec),
        length(param_vec$deltaVec),
        length(param_vec$lambdaVec)
      )

      if (all(param_lengths == n)) {
        # Create parameter matrix for calculateMeans
        params <- matrix(0, nrow = n, ncol = 5)
        params[, 1] <- param_vec$alphaVec
        params[, 2] <- param_vec$betaVec
        params[, 3] <- param_vec$gammaVec
        params[, 4] <- param_vec$deltaVec
        params[, 5] <- param_vec$lambdaVec

        # Calculate means using the parameters
        message("Calculating fitted values from parameter vectors...")
        return(calculateMeans(params, family = family))
      }
    }
  }

  # Method 3: Check TMB report for fitted values
  if (!is.null(object$tmb_object)) {
    tmb_report <- try(object$tmb_object$report(), silent = TRUE)

    if (!inherits(tmb_report, "try-error") && "fitted" %in% names(tmb_report)) {
      fitted_from_tmb <- tmb_report$fitted

      if (length(fitted_from_tmb) == n) {
        return(as.vector(fitted_from_tmb))
      } else if (length(fitted_from_tmb) > 1 && length(fitted_from_tmb) < n && n > 10000) {
        # For large datasets, interpolate if we only have a sample
        message("Found partial fitted values in TMB report. Interpolating...")

        # Create fitted values vector with NAs
        fitted_values <- rep(NA_real_, n)

        # Map available values to appropriate indices
        sample_size <- length(fitted_from_tmb)
        sample_idx <- floor(seq(1, n, length.out = sample_size))
        fitted_values[sample_idx] <- fitted_from_tmb

        # Interpolate the rest
        idx_with_values <- which(!is.na(fitted_values))
        fitted_values <- stats::approx(
          x = idx_with_values,
          y = fitted_values[idx_with_values],
          xout = seq_len(n),
          rule = 2
        )$y

        return(fitted_values)
      }
    }
  }

  # Method 4: If all else fails, use predict.gkwreg
  message("Recalculating fitted values using the predict function...")

  # Use predict to calculate fitted values
  fitted_values <- stats::predict(object, type = "response", family = family)

  return(fitted_values)
}


#' @title Extract Residuals from a Generalized Kumaraswamy Regression Model
#'
#' @description
#' Extracts or calculates various types of residuals from a fitted Generalized
#' Kumaraswamy (GKw) regression model object of class \code{"gkwreg"}, useful for
#' model diagnostics.
#'
#' @param object An object of class \code{"gkwreg"}, typically the result of a
#'   call to \code{\link{gkwreg}}.
#' @param type Character string specifying the type of residuals to compute.
#'   Available options are:
#'   \itemize{
#'     \item \code{"response"}: (Default) Raw response residuals:
#'       \eqn{y - \mu}, where \eqn{\mu} is the fitted mean.
#'     \item \code{"pearson"}: Pearson residuals: \eqn{(y - \mu) / \sqrt{V(\mu)}},
#'       where \eqn{V(\mu)} is the variance function of the specified family.
#'     \item \code{"deviance"}: Deviance residuals: Signed square root of the
#'       unit deviances. Sum of squares equals the total deviance.
#'     \item \code{"quantile"}: Randomized quantile residuals (Dunn & Smyth, 1996).
#'       Transformed via the model's CDF and the standard normal quantile function.
#'       Should approximate a standard normal distribution if the model is correct.
#'     \item \code{"modified.deviance"}: (Not typically implemented, placeholder)
#'       Standardized deviance residuals, potentially adjusted for leverage.
#'     \item \code{"cox-snell"}: Cox-Snell residuals: \eqn{-\log(1 - F(y))}, where
#'       \eqn{F(y)} is the model's CDF. Should approximate a standard exponential
#'       distribution if the model is correct.
#'     \item \code{"score"}: (Not typically implemented, placeholder) Score residuals,
#'       related to the derivative of the log-likelihood.
#'     \item \code{"partial"}: Partial residuals for a specific predictor in one
#'       parameter's linear model: \eqn{eta_p + \beta_{pk} x_{ik}}, where \eqn{eta_p}
#'       is the partial linear predictor and \eqn{\beta_{pk} x_{ik}} is the
#'       component associated with the k-th covariate for the i-th observation.
#'       Requires \code{parameter} and \code{covariate_idx}.
#'   }
#' @param covariate_idx Integer. Only used if \code{type = "partial"}. Specifies the
#'   index (column number in the corresponding model matrix) of the covariate for
#'   which to compute partial residuals.
#' @param parameter Character string. Only used if \code{type = "partial"}. Specifies
#'   the distribution parameter (\code{"alpha"}, \code{"beta"}, \code{"gamma"},
#'   \code{"delta"}, or \code{"lambda"}) whose linear predictor contains the
#'   covariate of interest.
#' @param family Character string specifying the distribution family assumptions
#'   to use when calculating residuals (especially for types involving variance,
#'   deviance, CDF, etc.). If \code{NULL} (default), the family stored within the
#'   fitted \code{object} is used. Specifying a different family may be useful
#'   for diagnostic comparisons. Available options match those in
#'   \code{\link{gkwreg}}: \code{"gkw", "bkw", "kkw", "ekw", "mc", "kw", "beta"}.
#' @param ... Additional arguments, currently ignored by this method.
#'
#' @details
#' This function calculates various types of residuals useful for diagnosing the
#' adequacy of a fitted GKw regression model.
#'
#' \itemize{
#'   \item \strong{Response residuals} (\code{type="response"}) are the simplest,
#'     showing raw differences between observed and fitted mean values.
#'   \item \strong{Pearson residuals} (\code{type="pearson"}) account for the
#'     mean-variance relationship specified by the model family. Constant variance
#'     when plotted against fitted values suggests the variance function is appropriate.
#'   \item \strong{Deviance residuals} (\code{type="deviance"}) are related to the
#'     log-likelihood contribution of each observation. Their sum of squares equals
#'     the total model deviance. They often have more symmetric distributions
#'     than Pearson residuals.
#'   \item \strong{Quantile residuals} (\code{type="quantile"}) are particularly useful
#'     for non-standard distributions as they should always be approximately standard
#'     normal if the assumed distribution and model structure are correct. Deviations
#'     from normality in a QQ-plot indicate model misspecification.
#'   \item \strong{Cox-Snell residuals} (\code{type="cox-snell"}) provide another
#'     check of the overall distributional fit. A plot of the sorted residuals
#'     against theoretical exponential quantiles should approximate a straight line
#'     through the origin with slope 1.
#'   \item \strong{Partial residuals} (\code{type="partial"}) help visualize the
#'     marginal relationship between a specific predictor and the response on the
#'     scale of the linear predictor for a chosen parameter, adjusted for other predictors.
#' }
#' Calculations involving the distribution's properties (variance, CDF, PDF) depend
#' heavily on the specified \code{family}. The function relies on internal helper
#' functions (potentially implemented in C++ for efficiency) to compute these based
#' on the fitted parameters for each observation.
#'
#' @return A numeric vector containing the requested type of residuals. The length
#'   corresponds to the number of observations used in the model fit.
#'
#' @author Lopes, J. E.
#'
#' @references
#' Dunn, P. K., & Smyth, G. K. (1996). Randomized Quantile Residuals.
#' \emph{Journal of Computational and Graphical Statistics}, \strong{5}(3), 236-244.
#'
#'
#' Cox, D. R., & Snell, E. J. (1968). A General Definition of Residuals.
#' \emph{Journal of the Royal Statistical Society, Series B (Methodological)},
#' \strong{30}(2), 248-275.
#'
#' McCullagh, P., & Nelder, J. A. (1989). \emph{Generalized Linear Models} (2nd ed.).
#' Chapman and Hall/CRC.
#'
#' @seealso \code{\link{gkwreg}}, \code{\link{fitted.gkwreg}},
#'   \code{\link{predict.gkwreg}}, \code{\link[stats]{residuals}}
#'
#' @keywords residuals methods regression diagnostics
#'
#' @examples
#' \donttest{
#' require(gkwreg)
#' require(gkwdist)
#'
#' # Example 1: Comprehensive residual analysis for FoodExpenditure
#' data(FoodExpenditure)
#' FoodExpenditure$prop <- FoodExpenditure$food / FoodExpenditure$income
#'
#' fit_kw <- gkwreg(
#'   prop ~ income + persons | income + persons,
#'   data = FoodExpenditure,
#'   family = "kw"
#' )
#'
#' # Extract different types of residuals
#' res_response <- residuals(fit_kw, type = "response")
#' res_pearson <- residuals(fit_kw, type = "pearson")
#' res_deviance <- residuals(fit_kw, type = "deviance")
#' res_quantile <- residuals(fit_kw, type = "quantile")
#' res_coxsnell <- residuals(fit_kw, type = "cox-snell")
#'
#' # Summary statistics
#' residual_summary <- data.frame(
#'   Type = c("Response", "Pearson", "Deviance", "Quantile", "Cox-Snell"),
#'   Mean = c(
#'     mean(res_response), mean(res_pearson),
#'     mean(res_deviance), mean(res_quantile),
#'     mean(res_coxsnell)
#'   ),
#'   SD = c(
#'     sd(res_response), sd(res_pearson),
#'     sd(res_deviance), sd(res_quantile),
#'     sd(res_coxsnell)
#'   ),
#'   Min = c(
#'     min(res_response), min(res_pearson),
#'     min(res_deviance), min(res_quantile),
#'     min(res_coxsnell)
#'   ),
#'   Max = c(
#'     max(res_response), max(res_pearson),
#'     max(res_deviance), max(res_quantile),
#'     max(res_coxsnell)
#'   )
#' )
#' print(residual_summary)
#'
#' # Example 2: Diagnostic plots for model assessment
#' data(GasolineYield)
#'
#' fit_ekw <- gkwreg(
#'   yield ~ batch + temp | temp | batch,
#'   data = GasolineYield,
#'   family = "ekw"
#' )
#'
#' # Set up plotting grid
#' par(mfrow = c(2, 3))
#'
#' # Plot 1: Residuals vs Fitted
#' fitted_vals <- fitted(fit_ekw)
#' res_pears <- residuals(fit_ekw, type = "pearson")
#' plot(fitted_vals, res_pears,
#'   xlab = "Fitted Values", ylab = "Pearson Residuals",
#'   main = "Residuals vs Fitted",
#'   pch = 19, col = rgb(0, 0, 1, 0.5)
#' )
#' abline(h = 0, col = "red", lwd = 2, lty = 2)
#' lines(lowess(fitted_vals, res_pears), col = "blue", lwd = 2)
#'
#' # Plot 2: Normal QQ-plot (Quantile Residuals)
#' res_quant <- residuals(fit_ekw, type = "quantile")
#' qqnorm(res_quant,
#'   main = "Normal Q-Q Plot (Quantile Residuals)",
#'   pch = 19, col = rgb(0, 0, 1, 0.5)
#' )
#' qqline(res_quant, col = "red", lwd = 2)
#'
#' # Plot 3: Scale-Location (sqrt of standardized residuals)
#' plot(fitted_vals, sqrt(abs(res_pears)),
#'   xlab = "Fitted Values", ylab = expression(sqrt("|Std. Residuals|")),
#'   main = "Scale-Location",
#'   pch = 19, col = rgb(0, 0, 1, 0.5)
#' )
#' lines(lowess(fitted_vals, sqrt(abs(res_pears))), col = "red", lwd = 2)
#'
#' # Plot 4: Histogram of Quantile Residuals
#' hist(res_quant,
#'   breaks = 15, probability = TRUE,
#'   xlab = "Quantile Residuals",
#'   main = "Histogram with Normal Overlay",
#'   col = "lightblue", border = "white"
#' )
#' curve(dnorm(x, mean(res_quant), sd(res_quant)),
#'   add = TRUE, col = "red", lwd = 2
#' )
#'
#' # Plot 5: Cox-Snell Residual Plot
#' res_cs <- residuals(fit_ekw, type = "cox-snell")
#' plot(qexp(ppoints(length(res_cs))), sort(res_cs),
#'   xlab = "Theoretical Exponential Quantiles",
#'   ylab = "Ordered Cox-Snell Residuals",
#'   main = "Cox-Snell Residual Plot",
#'   pch = 19, col = rgb(0, 0, 1, 0.5)
#' )
#' abline(0, 1, col = "red", lwd = 2)
#'
#' # Plot 6: Residuals vs Index
#' plot(seq_along(res_pears), res_pears,
#'   xlab = "Observation Index", ylab = "Pearson Residuals",
#'   main = "Residuals vs Index",
#'   pch = 19, col = rgb(0, 0, 1, 0.5)
#' )
#' abline(h = 0, col = "red", lwd = 2, lty = 2)
#'
#' par(mfrow = c(1, 1))
#'
#' # Example 3: Partial residual plots for covariate effects
#' data(ReadingSkills)
#'
#' fit_interact <- gkwreg(
#'   accuracy ~ dyslexia * iq | dyslexia + iq,
#'   data = ReadingSkills,
#'   family = "kw"
#' )
#'
#' # Partial residuals for IQ effect on alpha parameter
#' X_alpha <- fit_interact$model_matrices$alpha
#' iq_col_alpha <- which(colnames(X_alpha) == "iq")
#'
#' if (length(iq_col_alpha) > 0) {
#'   res_partial_alpha <- residuals(fit_interact,
#'     type = "partial",
#'     parameter = "alpha",
#'     covariate_idx = iq_col_alpha
#'   )
#'
#'   par(mfrow = c(1, 2))
#'
#'   # Partial residual plot for alpha
#'   plot(ReadingSkills$iq, res_partial_alpha,
#'     xlab = "IQ (z-scores)",
#'     ylab = "Partial Residual (alpha)",
#'     main = "Effect of IQ on Mean (alpha)",
#'     pch = 19, col = ReadingSkills$dyslexia
#'   )
#'   lines(lowess(ReadingSkills$iq, res_partial_alpha),
#'     col = "blue", lwd = 2
#'   )
#'   legend("topleft",
#'     legend = c("Control", "Dyslexic"),
#'     col = c("black", "red"), pch = 19
#'   )
#'
#'   # Partial residuals for IQ effect on beta parameter
#'   X_beta <- fit_interact$model_matrices$beta
#'   iq_col_beta <- which(colnames(X_beta) == "iq")
#'
#'   if (length(iq_col_beta) > 0) {
#'     res_partial_beta <- residuals(fit_interact,
#'       type = "partial",
#'       parameter = "beta",
#'       covariate_idx = iq_col_beta
#'     )
#'
#'     plot(ReadingSkills$iq, res_partial_beta,
#'       xlab = "IQ (z-scores)",
#'       ylab = "Partial Residual (beta)",
#'       main = "Effect of IQ on Precision (beta)",
#'       pch = 19, col = ReadingSkills$dyslexia
#'     )
#'     lines(lowess(ReadingSkills$iq, res_partial_beta),
#'       col = "blue", lwd = 2
#'     )
#'   }
#'
#'   par(mfrow = c(1, 1))
#' }
#'
#' # Example 4: Comparing residuals across different families
#' data(StressAnxiety)
#'
#' fit_kw_stress <- gkwreg(
#'   anxiety ~ stress | stress,
#'   data = StressAnxiety,
#'   family = "kw"
#' )
#'
#' # Quantile residuals under different family assumptions
#' res_quant_kw <- residuals(fit_kw_stress, type = "quantile", family = "kw")
#' res_quant_beta <- residuals(fit_kw_stress, type = "quantile", family = "beta")
#'
#' # Compare normality
#' par(mfrow = c(1, 2))
#'
#' qqnorm(res_quant_kw,
#'   main = "QQ-Plot: Kumaraswamy Residuals",
#'   pch = 19, col = rgb(0, 0, 1, 0.5)
#' )
#' qqline(res_quant_kw, col = "red", lwd = 2)
#'
#' qqnorm(res_quant_beta,
#'   main = "QQ-Plot: Beta Residuals",
#'   pch = 19, col = rgb(0, 0.5, 0, 0.5)
#' )
#' qqline(res_quant_beta, col = "red", lwd = 2)
#'
#' par(mfrow = c(1, 1))
#'
#' # Formal normality tests
#' shapiro_kw <- shapiro.test(res_quant_kw)
#' shapiro_beta <- shapiro.test(res_quant_beta)
#'
#' cat("\nShapiro-Wilk Test Results:\n")
#' cat(
#'   "Kumaraswamy:  W =", round(shapiro_kw$statistic, 4),
#'   ", p-value =", round(shapiro_kw$p.value, 4), "\n"
#' )
#' cat(
#'   "Beta:         W =", round(shapiro_beta$statistic, 4),
#'   ", p-value =", round(shapiro_beta$p.value, 4), "\n"
#' )
#'
#' # Example 5: Outlier detection using standardized residuals
#' data(MockJurors)
#'
#' fit_mc <- gkwreg(
#'   confidence ~ verdict * conflict | verdict + conflict,
#'   data = MockJurors,
#'   family = "mc"
#' )
#'
#' res_dev <- residuals(fit_mc, type = "deviance")
#' res_quant <- residuals(fit_mc, type = "quantile")
#'
#' # Identify potential outliers (|z| > 2.5)
#' outlier_idx <- which(abs(res_quant) > 2.5)
#'
#' if (length(outlier_idx) > 0) {
#'   cat("\nPotential outliers detected at indices:", outlier_idx, "\n")
#'
#'   # Display outlier information
#'   outlier_data <- data.frame(
#'     Index = outlier_idx,
#'     Confidence = MockJurors$confidence[outlier_idx],
#'     Verdict = MockJurors$verdict[outlier_idx],
#'     Conflict = MockJurors$conflict[outlier_idx],
#'     Quantile_Residual = round(res_quant[outlier_idx], 3),
#'     Deviance_Residual = round(res_dev[outlier_idx], 3)
#'   )
#'   print(outlier_data)
#'
#'   # Influence plot
#'   plot(seq_along(res_quant), res_quant,
#'     xlab = "Observation Index",
#'     ylab = "Quantile Residual",
#'     main = "Outlier Detection: Mock Jurors",
#'     pch = 19, col = rgb(0, 0, 1, 0.5)
#'   )
#'   points(outlier_idx, res_quant[outlier_idx],
#'     col = "red", pch = 19, cex = 1.5
#'   )
#'   abline(
#'     h = c(-2.5, 0, 2.5), col = c("orange", "black", "orange"),
#'     lty = c(2, 1, 2), lwd = 2
#'   )
#'   legend("topright",
#'     legend = c("Normal", "Outlier", "±2.5 SD"),
#'     col = c(rgb(0, 0, 1, 0.5), "red", "orange"),
#'     pch = c(19, 19, NA),
#'     lty = c(NA, NA, 2),
#'     lwd = c(NA, NA, 2)
#'   )
#' } else {
#'   cat("\nNo extreme outliers detected (|z| > 2.5)\n")
#' }
#' }
#'
#' @export
residuals.gkwreg <- function(
  object, type = c(
    "response", "pearson", "deviance", "quantile",
    "modified.deviance", "cox-snell",
    "score", "partial"
  ),
  covariate_idx = 1, parameter = "alpha",
  family = NULL, ...
) {
  # Check if object is of class "gkwreg"
  if (!inherits(object, "gkwreg")) {
    stop("'object' must be a fitted model of class \"gkwreg\"")
  }

  # Match argument
  type <- match.arg(type)

  # Get the family from the object if not specified
  if (is.null(family)) {
    if (!is.null(object$family)) {
      family <- object$family
    } else {
      # Default to gkw for backward compatibility
      family <- "gkw"
      message("No family specified in the model. Using 'gkw' as default.")
    }
  } else {
    # Validate the family parameter
    family <- match.arg(family, c("gkw", "bkw", "kkw", "ekw", "mc", "kw", "beta"))

    # If family is different from the model's family, show a message
    if (!is.null(object$family) && family != object$family) {
      message(paste0(
        "Using different family (", family, ") than what was used to fit the model (",
        object$family, ")."
      ))
    }
  }

  # Get response values
  if (is.null(object$y)) {
    stop("Response variable not found in model object. Cannot calculate residuals.")
  }
  y <- object$y

  # Get fitted values - passing the family parameter
  fitted_vals <- stats::fitted(object, family = family)

  # If type is "response", we can calculate quickly
  if (type == "response") {
    return(calculateResponseResiduals(y, fitted_vals))
  }

  # For partial residuals, verify parameters
  if (type == "partial") {
    if (!parameter %in% c("alpha", "beta", "gamma", "delta", "lambda")) {
      stop("parameter must be one of: 'alpha', 'beta', 'gamma', 'delta', 'lambda'")
    }

    # Confirm valid covariate_idx
    model_matrices <- object$model_matrices
    if (is.null(model_matrices)) {
      stop("Model matrices not available in model object")
    }

    X <- model_matrices[[parameter]]
    if (is.null(X)) {
      stop("Model matrix for parameter '", parameter, "' not found")
    }

    p <- ncol(X)
    if (covariate_idx < 1 || covariate_idx > p) {
      stop("covariate_idx must be between 1 and ", p)
    }
  }

  # Get parameters for each observation
  get_parameters <- function(object, family) {
    # Initialize vectors for parameters
    n <- length(object$y)

    # Try to get parameters via predict with the specified family
    if (exists("predict.gkwreg", mode = "function")) {
      tryCatch(
        {
          params_df <- stats::predict(object, type = "parameter", family = family)
          return(list(
            alpha = params_df$alpha,
            beta = params_df$beta,
            gamma = params_df$gamma,
            delta = params_df$delta,
            lambda = params_df$lambda
          ))
        },
        error = function(e) {
          warning("Could not extract parameter values using predict.gkwreg(): ", e$message)
        }
      )
    }

    # If we're using the same family as the model, try to get parameters directly
    if (is.null(object$family) || family == object$family) {
      # Try to get individual parameter vectors from the model object first
      if (!is.null(object$model) && !is.null(object$model$report)) {
        report <- object$model$report()

        # Check if individual parameter vectors are available
        if (all(c("alphaVec", "betaVec", "gammaVec", "deltaVec", "lambdaVec") %in% names(report))) {
          return(list(
            alpha = report$alphaVec,
            beta = report$betaVec,
            gamma = report$gammaVec,
            delta = report$deltaVec,
            lambda = report$lambdaVec
          ))
        }
      }
    }

    # If we get here, we need to approximate using the average parameter values
    # or recompute parameters with the appropriate family constraints

    # Check if we have fitted parameters already
    if (!is.null(object$fitted_parameters)) {
      # Start with the model's fitted parameters
      base_params <- list(
        alpha = rep(object$fitted_parameters$alpha, n),
        beta = rep(object$fitted_parameters$beta, n),
        gamma = rep(object$fitted_parameters$gamma, n),
        delta = rep(object$fitted_parameters$delta, n),
        lambda = rep(object$fitted_parameters$lambda, n)
      )

      # Apply family-specific constraints if needed
      if (family != "gkw") {
        if (family == "bkw") {
          # BKw: lambda = 1
          base_params$lambda <- rep(1.0, n)
        } else if (family == "kkw") {
          # KKw: gamma = 1
          base_params$gamma <- rep(1.0, n)
        } else if (family == "ekw") {
          # EKw: gamma = 1, delta = 0
          base_params$gamma <- rep(1.0, n)
          base_params$delta <- rep(0.0, n)
        } else if (family == "mc") {
          # MC: alpha = 1, beta = 1
          base_params$alpha <- rep(1.0, n)
          base_params$beta <- rep(1.0, n)
        } else if (family == "kw") {
          # KW: lambda = 1, gamma = 1, delta = 0
          base_params$gamma <- rep(1.0, n)
          base_params$delta <- rep(0.0, n)
          base_params$lambda <- rep(1.0, n)
        } else if (family == "beta") {
          # Beta: alpha = 1, beta = 1, lambda = 1
          base_params$alpha <- rep(1.0, n)
          base_params$beta <- rep(1.0, n)
          base_params$lambda <- rep(1.0, n)
        }
      }

      message("Using adjusted parameter values for family ", family, ".")
      return(base_params)
    }

    # If all else fails
    stop("Unable to extract parameter values from the model object.")
  }

  # Get parameters with the specified family
  params <- get_parameters(object, family)

  # Create a parameter matrix for C++ functions that expect it
  param_matrix <- matrix(
    c(params$alpha, params$beta, params$gamma, params$delta, params$lambda),
    ncol = 5
  )

  # Calculate appropriate residuals
  result <- switch(type,
    "pearson" = {
      calculatePearsonResiduals(
        y, fitted_vals, param_matrix,
        family = family
      )
    },
    "deviance" = {
      calculateDevianceResiduals(
        y, fitted_vals, param_matrix,
        family = family
      )
    },
    "quantile" = {
      calculateQuantileResiduals(
        y, param_matrix,
        family = family
      )
    },
    "modified.deviance" = {
      calculateModifiedDevianceResiduals(
        y, fitted_vals, param_matrix,
        family = family
      )
    },
    "cox-snell" = {
      calculateCoxSnellResiduals(
        y, param_matrix,
        family = family
      )
    },
    "score" = {
      calculateScoreResiduals(
        y, fitted_vals, param_matrix,
        family = family
      )
    },
    "partial" = {
      # Get model matrices and coefficients for the specified parameter
      X <- object$model_matrices[[parameter]]
      beta <- object$coefficients[[parameter]]

      # We need C++ 0-based indexing
      c_idx <- covariate_idx - 1

      calculatePartialResiduals(y, fitted_vals, X, beta, c_idx)
    }
  )

  return(result)
}
