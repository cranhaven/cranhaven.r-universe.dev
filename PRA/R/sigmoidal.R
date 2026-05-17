#' Fit a Sigmoidal Model to Data.
#'
#' This function fits a sigmoidal model (Pearl, Gompertz, or Logistic) to the provided data.
#'
#' @srrstats {G1.0} *Software lists primary reference from published academic literature.*
#' @srrstats {G1.1} *Software is the first implementation within **R** of these sigmoidal models for project risk management.*
#' @srrstats {G1.4} *Software uses [`roxygen2`](https://roxygen2.r-lib.org/) to document all functions.*
#' @srrstats {G2.0} *Implements assertions on input structure - validates column existence in data frame.*
#' @srrstats {G2.0a} *Parameter documentation explicitly states expected input structure (data frame with named columns).*
#' @srrstats {G2.1} *Implements assertions on column existence in data frame.*
#' @srrstats {G2.1a} *Parameter documentation explicitly states data types expected.*
#' @srrstats {G2.3a} *Uses explicit validation of model_type against valid options.*
#' @srrstats {G2.3b} *Uses tolower() to ensure model_type parameter is not case sensitive.*
#' @srrstats {G2.7} *Accepts data.frame as standard tabular input.*
#' @srrstats {G2.15} *Implements checks for NaN values via is.nan() prior to processing.*
#' @srrstats {G2.16} *Implements checks for Inf/-Inf values via is.infinite() prior to processing.*
#' @srrstats {G5.2a} *Each error message produced by stop() is unique.*
#'
#' @param data A data frame containing the time (x_col) and completion (y_col) vectors.
#' @param x_col The name of the time vector.
#' @param y_col The name of the completion vector.
#' @param model_type The name of the sigmoidal model (Pearl, Gompertz, or Logistic).
#' @return The function returns a list of results for the sigmoidal model.
#' @references
#' Damnjanovic, Ivan, and Kenneth Reinschmidt. Data analytics for engineering and
#' construction project risk management. No. 172534. Cham, Switzerland: Springer, 2020.
#' @examples
#' # Set up a data frame of time and completion percentage data
#' data <- data.frame(time = 1:10, completion = c(5, 15, 40, 60, 70, 75, 80, 85, 90, 95))
#'
#' # Fit a logistic model to the data.
#' fit <- fit_sigmoidal(data, "time", "completion", "logistic")
#'
#' # Use the model to predict future completion times.
#' predictions <- predict_sigmoidal(fit, seq(min(data$time), max(data$time),
#'   length.out = 100
#' ), "logistic")
#'
#' # Predict with 95% confidence bounds
#' predictions_ci <- predict_sigmoidal(fit, seq(min(data$time), max(data$time),
#'   length.out = 100
#' ), "logistic", conf_level = 0.95)
#'
#' @importFrom minpack.lm nlsLM
#' @importFrom stats median
#' @export

# Fit a Sigmoidal Model
fit_sigmoidal <- function(data, x_col, y_col, model_type) {
  # Convert model_type to lowercase for case-insensitive matching
  model_type <- tolower(model_type)

  # Error handling
  if (!x_col %in% names(data)) {
    stop(paste("Column", x_col, "not found in data frame."))
  }
  if (!y_col %in% names(data)) {
    stop(paste("Column", y_col, "not found in data frame."))
  }

  x <- data[[x_col]]
  y <- data[[y_col]]

  if (any(is.nan(x)) || any(is.nan(y))) {
    stop("Data columns must not contain NaN values.")
  }
  if (anyNA(x) || anyNA(y)) {
    stop("Data columns must not contain NA values.")
  }
  if (any(is.infinite(x)) || any(is.infinite(y))) {
    stop("Data columns must not contain infinite values.")
  }

  # Pearl Sigmoidal Model
  pearl <- function(x, K, r, t0) {
    K / (1 + exp(-r * (x - t0)))
  }

  # Gompertz Sigmoidal Model
  gompertz <- function(x, A, b, c) {
    A * exp(-b * exp(-c * x))
  }

  # Logistic Sigmoidal Model
  logistic <- function(x, K, r, t0) {
    K / (1 + exp(-r * (x - t0)))
  }

  if (model_type == "pearl") {
    fit <- minpack.lm::nlsLM(y ~ pearl(x, K, r, t0), start = list(K = max(y), r = 0.1, t0 = stats::median(x)))
  } else if (model_type == "gompertz") {
    fit <- minpack.lm::nlsLM(y ~ gompertz(x, A, b, c), start = list(A = max(y), b = 1, c = 0.1))
  } else if (model_type == "logistic") {
    fit <- minpack.lm::nlsLM(y ~ logistic(x, K, r, t0), start = list(K = max(y), r = 0.1, t0 = stats::median(x)))
  } else {
    stop("Invalid model type. Choose 'pearl', 'gompertz', or 'logistic'.")
  }

  class(fit) <- c("pra_sigmoidal_fit", class(fit))
  return(fit)
}

#' Predict a Sigmoidal Function Using Fitted Model.
#'
#' This function predicts values using a fitted sigmoidal model (Pearl, Gompertz,
#' or Logistic) over a specified range of time values.
#'
#' @srrstats {G1.0} *Software lists primary reference from published academic literature.*
#' @srrstats {G1.1} *Software is the first implementation within **R** of these sigmoidal prediction methods for project risk management.*
#' @srrstats {G1.4} *Software uses [`roxygen2`](https://roxygen2.r-lib.org/) to document all functions.*
#' @srrstats {G2.0} *Implements assertions on input structure - validates fitted model is not NULL.*
#' @srrstats {G2.0a} *Parameter documentation explicitly states expected input structure.*
#' @srrstats {G2.1} *Implements assertions on types of inputs via is.numeric() checks.*
#' @srrstats {G2.1a} *Parameter documentation explicitly states data types expected.*
#' @srrstats {G2.3a} *Uses explicit validation of model_type against valid options.*
#' @srrstats {G2.3b} *Uses tolower() to ensure model_type parameter is not case sensitive.*
#' @srrstats {G2.15} *Implements checks for NaN values via is.nan() prior to processing.*
#' @srrstats {G2.16} *Implements checks for Inf/-Inf values via is.infinite() prior to processing.*
#' @srrstats {G5.2a} *Each error message produced by stop() is unique.*
#'
#' @param fit A list containing the results of a sigmoidal model.
#' @param x_range A vector of time values for the prediction.
#' @param model_type The type of model (Pearl, Gompertz, or Logistic) for the prediction.
#' @param conf_level Optional confidence level for confidence bounds (e.g., 0.95 for 95%).
#'   If NULL (default), no confidence bounds are computed.
#' @return The function returns a data frame containing the time (x), predicted values (pred),
#'   and optionally lower (lwr) and upper (upr) confidence bounds.
#' @references
#' Damnjanovic, Ivan, and Kenneth Reinschmidt. Data analytics for engineering and
#' construction project risk management. No. 172534. Cham, Switzerland: Springer, 2020.
#' @examples
#' # Set up a data frame of time and completion percentage data
#' data <- data.frame(time = 1:10, completion = c(5, 15, 40, 60, 70, 75, 80, 85, 90, 95))
#'
#' # Fit a logistic model to the data.
#' fit <- fit_sigmoidal(data, "time", "completion", "logistic")
#'
#' # Use the model to predict future completion times.
#' predictions <- predict_sigmoidal(fit, seq(min(data$time), max(data$time),
#'   length.out = 100
#' ), "logistic")
#'
#' # Predict with 95% confidence bounds
#' predictions_ci <- predict_sigmoidal(fit, seq(min(data$time), max(data$time),
#'   length.out = 100
#' ), "logistic", conf_level = 0.95)
#'
#' @importFrom stats predict coef vcov df.residual qt
#' @export
# Predict a Sigmoidal Function
predict_sigmoidal <- function(fit, x_range, model_type, conf_level = NULL) {
  # Convert model_type to lowercase for case-insensitive matching
  model_type <- tolower(model_type)

  # Error handling
  if (is.null(fit)) {
    stop("Fitted model is NULL.")
  }
  if (!is.numeric(x_range)) {
    stop("x_range must be a numeric vector.")
  }
  if (any(is.nan(x_range))) {
    stop("x_range must not contain NaN values.")
  }
  if (anyNA(x_range)) {
    stop("x_range must not contain NA values.")
  }
  if (any(is.infinite(x_range))) {
    stop("x_range must not contain infinite values.")
  }

  new_data <- data.frame(x = x_range)

  # Define model functions for gradient computation
  pearl <- function(x, K, r, t0) {
    K / (1 + exp(-r * (x - t0)))
  }

  gompertz <- function(x, A, b, c) {
    A * exp(-b * exp(-c * x))
  }

  logistic <- function(x, K, r, t0) {
    K / (1 + exp(-r * (x - t0)))
  }

  if (!model_type %in% c("pearl", "gompertz", "logistic")) {
    stop("Invalid model type. Choose 'pearl', 'gompertz', or 'logistic'.")
  }

  new_data$pred <- stats::predict(fit, newdata = new_data)

  # Compute confidence bounds if requested
  if (!is.null(conf_level)) {
    if (conf_level <= 0 || conf_level >= 1) {
      stop("conf_level must be between 0 and 1 (e.g., 0.95 for 95% CI).")
    }

    # Get model parameters and variance-covariance matrix
    params <- stats::coef(fit)
    vcov_mat <- stats::vcov(fit)
    df_resid <- stats::df.residual(fit)

    # Compute gradient for each prediction point using numerical differentiation
    eps <- 1e-6
    n_params <- length(params)
    n_points <- length(x_range)
    grad_matrix <- matrix(0, nrow = n_points, ncol = n_params)

    for (j in seq_len(n_params)) {
      params_plus <- params
      params_plus[j] <- params_plus[j] + eps

      if (model_type == "pearl" || model_type == "logistic") {
        pred_plus <- pearl(x_range, params_plus["K"], params_plus["r"], params_plus["t0"])
        pred_base <- pearl(x_range, params["K"], params["r"], params["t0"])
      } else {
        pred_plus <- gompertz(x_range, params_plus["A"], params_plus["b"], params_plus["c"])
        pred_base <- gompertz(x_range, params["A"], params["b"], params["c"])
      }

      grad_matrix[, j] <- (pred_plus - pred_base) / eps
    }

    # Compute standard errors using the delta method
    se <- sqrt(rowSums((grad_matrix %*% vcov_mat) * grad_matrix))

    # Compute confidence bounds
    t_crit <- stats::qt((1 + conf_level) / 2, df_resid)
    new_data$lwr <- new_data$pred - t_crit * se
    new_data$upr <- new_data$pred + t_crit * se
  }

  return(new_data)
}

#' Plot a Fitted Sigmoidal Model.
#'
#' This function creates a base R plot of a fitted sigmoidal model with the
#' original data points, fitted curve, and optional confidence bounds.
#'
#' @srrstats {G1.0} *Software lists primary reference from published academic literature.*
#' @srrstats {G1.1} *Software is the first implementation within **R** of this sigmoidal plotting method for project risk management.*
#' @srrstats {G1.4} *Software uses [`roxygen2`](https://roxygen2.r-lib.org/) to document all functions.*
#' @srrstats {G2.0} *Implements assertions on input structure - validates fitted model and column existence.*
#' @srrstats {G2.0a} *Parameter documentation explicitly states expected input structure.*
#' @srrstats {G2.1} *Implements assertions on column existence in data frame.*
#' @srrstats {G2.1a} *Parameter documentation explicitly states data types expected.*
#' @srrstats {G2.3b} *Uses tolower() to ensure model_type parameter is not case sensitive.*
#' @srrstats {G2.7} *Accepts data.frame as standard tabular input.*
#' @srrstats {G2.15} *Implements checks for NaN values via is.nan() prior to processing.*
#' @srrstats {G2.16} *Implements checks for Inf/-Inf values via is.infinite() prior to processing.*
#' @srrstats {G5.2a} *Each error message produced by stop() is unique.*
#'
#' @param fit A fitted sigmoidal model object from fit_sigmoidal.
#' @param data The original data frame used to fit the model.
#' @param x_col The name of the x (time) column in the data.
#' @param y_col The name of the y (completion) column in the data.
#' @param model_type The type of model (pearl, gompertz, or logistic).
#' @param conf_level Optional confidence level for confidence bounds (e.g., 0.95 for 95%).
#'   If NULL (default), no confidence bounds are plotted.
#' @param n_points Number of points to use for the fitted curve (default 100).
#' @param main Plot title. If NULL, a default title is generated.
#' @param xlab X-axis label. If NULL, uses x_col.
#' @param ylab Y-axis label. If NULL, uses y_col.
#' @param line_col Color for the fitted curve (default "red").
#' @param ci_col Color for the confidence band (default "lightblue").
#' @param pch Point character for data points (default 16).
#' @param ... Additional arguments passed to plot().
#' @return Invisibly returns the predictions data frame.
#' @references
#' Damnjanovic, Ivan, and Kenneth Reinschmidt. Data analytics for engineering and
#' construction project risk management. No. 172534. Cham, Switzerland: Springer, 2020.
#'
#' @examples
#' # Set up a data frame of time and completion percentage data
#' data <- data.frame(time = 1:10, completion = c(5, 15, 40, 60, 70, 75, 80, 85, 90, 95))
#'
#' # Fit a logistic model to the data.
#' fit <- fit_sigmoidal(data, "time", "completion", "logistic")
#'
#' # Plot the fitted model
#' plot_sigmoidal(fit, data, "time", "completion", "logistic")
#'
#' # Plot with 95% confidence bounds
#' plot_sigmoidal(fit, data, "time", "completion", "logistic", conf_level = 0.95)
#'
#' # Customize the plot
#' plot_sigmoidal(fit, data, "time", "completion", "logistic",
#'   conf_level = 0.95,
#'   main = "Project Completion Forecast",
#'   xlab = "Time (weeks)",
#'   ylab = "Completion (%)",
#'   line_col = "blue",
#'   ci_col = "lightgray"
#' )
#'
#' @importFrom graphics plot lines polygon points
#' @export
plot_sigmoidal <- function(fit, data, x_col, y_col, model_type,
                           conf_level = NULL, n_points = 100,
                           main = NULL, xlab = NULL, ylab = NULL,
                           line_col = "red", ci_col = "lightblue",
                           pch = 16, ...) {
  # Convert model_type to lowercase for case-insensitive matching
  model_type <- tolower(model_type)

  # Error handling
  if (is.null(fit)) {
    stop("Fitted model is NULL.")
  }
  if (!x_col %in% names(data)) {
    stop(paste("Column", x_col, "not found in data frame."))
  }
  if (!y_col %in% names(data)) {
    stop(paste("Column", y_col, "not found in data frame."))
  }

  # Extract data
  x <- data[[x_col]]
  y <- data[[y_col]]

  if (any(is.nan(x)) || any(is.nan(y))) {
    stop("Data columns must not contain NaN values for plotting.")
  }
  if (anyNA(x) || anyNA(y)) {
    stop("Data columns must not contain NA values for plotting.")
  }
  if (any(is.infinite(x)) || any(is.infinite(y))) {
    stop("Data columns must not contain infinite values for plotting.")
  }

  # Generate prediction range
  x_range <- seq(min(x), max(x), length.out = n_points)

  # Get predictions with optional confidence bounds
  predictions <- predict_sigmoidal(fit, x_range, model_type, conf_level)

  # Set default labels
  if (is.null(main)) {
    main <- paste("Fitted", tools::toTitleCase(model_type), "Model")
  }
  if (is.null(xlab)) {
    xlab <- x_col
  }
  if (is.null(ylab)) {
    ylab <- y_col
  }

  # Determine y-axis limits
  if (!is.null(conf_level) && "lwr" %in% names(predictions)) {
    ylim <- range(c(y, predictions$lwr, predictions$upr), na.rm = TRUE)
  } else {
    ylim <- range(c(y, predictions$pred), na.rm = TRUE)
  }

  # Create plot
  graphics::plot(x, y,
    pch = pch, main = main, xlab = xlab, ylab = ylab,
    ylim = ylim, ...
  )

  # Add confidence band if requested
  if (!is.null(conf_level) && "lwr" %in% names(predictions)) {
    graphics::polygon(
      x = c(predictions$x, rev(predictions$x)),
      y = c(predictions$lwr, rev(predictions$upr)),
      col = ci_col, border = NA
    )
    # Re-plot points on top of the confidence band
    graphics::points(x, y, pch = pch)
  }

  # Add fitted curve
  graphics::lines(predictions$x, predictions$pred, col = line_col, lwd = 2)

  invisible(predictions)
}

#' Print method for Sigmoidal Model
#'
#' Displays the summary of the fitted sigmoidal model in a readable format.
#' @param x An object of class \code{"pra_sigmoidal_fit"} as returned by
#'   \code{\link{fit_sigmoidal}}.
#' @param ... Additional arguments (not used).
#' @return No return value, called for side effects.
#' @examples
#' # Set up a data frame of time and completion percentage data
#' data <- data.frame(time = 1:10, completion = c(
#'   5, 15, 40, 60, 70, 75, 80, 85,
#'   90, 95
#' ))
#'
#' # Fit a logistic model to the data.
#' fit <- fit_sigmoidal(data, "time", "completion", "logistic")
#' # Print the model summary
#' print(fit)
#' @export
#' @method print pra_sigmoidal_fit
print.pra_sigmoidal_fit <- function(x, ...) {
  cat("Sigmoidal Model Fit Summary:\n")
  # Temporarily strip custom class so stats:::print.nls handles the nls part
  class(x) <- setdiff(class(x), "pra_sigmoidal_fit")
  print(summary(x))
}
