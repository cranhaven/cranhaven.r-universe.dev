## Contains small functions for various usage
##
## Dependency: base, stats
## Author: SR Dhruba, Dec 2020
################################################################################

## Normalization functions...
#' Normalize vector in \[0, 1\]
#'
#' This function normalizes a given vector between 0 and 1.
#' @param x Vector containing data.
#' @return The normalized vector.
#' @keywords normalization
#' @export
#' @examples
#' x <- rnorm(100, 0.2, 0.3)
#' x_norm <- norm01(x)
#' print(range(x_norm))
#'
################################################################################
norm01 <- function(x) (x - min(x)) / diff(range(x))


#' Normalize matrix per column in \[0, 1\]
#'
#' This function normalizes each column of a dataframe or matrix (-alike)
#' between 0 and 1.
#' @param X Dataframe or matrix (-alike) containing data.
#' @return The normalized dataframe.
#' @keywords normalization
#' @export
#' @examples
#' X <- matrix(rnorm(1000, 0.2, 0.3), nrow = 100)
#' X_norm <- norm_data(X)
#' print(range(X_norm))
#'
################################################################################
norm_data <- function(X) as.data.frame(apply(X, MARGIN = 2, FUN = norm01))


#' Standardize matrix per column
#'
#' This function standardized each column of a dataframe or matrix (-alike) to
#' have \eqn{mean = 0} and \eqn{sd = 1}.
#' @param X Dataframe or matrix (-alike) containing data.
#' @return The standardized dataframe.
#' @keywords normalization standardization zscore
#' @export
#' @examples
#' X <- matrix(rnorm(100, 0.2, 0.3), nrow = 20)
#' X_std <- zscore(X)
#' print(apply(X_std, 2, mean))
#' print(apply(X_std, 2, sd))
#'
################################################################################
zscore <- function(X) as.data.frame(apply(X, MARGIN = 2, FUN = scale))


#' Restrict data in a given interval
#'
#' This function filters a data vector using a given interval so that only the
#' values falling inside the interval remains and any value that is less than
#' the leftmost end gets replaced by that end-value, and similarly, any value
#' greater than the rightmost end gets replaced by that end-value.
#'
#' @param x Vector containing data.
#' @param lims Limit for the values. Values falling within this limit will pass
#' without any change. Any value `x < lims[1]` will get replaced by `lims[1]`,
#' and any value `x > lims[2]` will get replaced by `lims[2]`. Defaults to
#' `c(0, 1)`.
#' @return The filtered vector.
#' @keywords filter
#' @export
#' @examples
#' x <- rnorm(100, 0, 1)
#' x_filt <- confined(x, lims = c(-0.5, 0.5))
#' print(range(x_filt))
#'
################################################################################
confined <- function(x, lims = c(0, 1)) {
  x[x < lims[1]] <- lims[1];      x[x > lims[2]] <- lims[2]
  x
}


#' Evaluate Regression Model Performance using Various Metrics
#'
#' This function produces the predictive performance for a regression model
#' using various common performance metrics such as MSE, R-squared, or
#' Correlation coefficients.
#'
#' @param y_obs Observed response values
#' @param y_pred Predicted response values
#' @param measures Performance measures. One can specify a single measure or a
#' vector containing multiple measures in terms of common error or similarity
#' metrics. The available options are roughly divided into 3 categories -
#' * "MSE", "RMSE", "NRMSE" for mean squared error, root mean squared error, and
#' normalized root mean squared error, respectively.
#' * "MAE", "NMAE" for mean absolute error, and normalized mean absolute error,
#' respectively.
#' * "PCC", "SCC", "RSQ" for Pearson's correlation, Spearman's correlation, and
#' R-squared, respectively.
#'
#' Defaults to `c("NRMSE", "NMAE", "PCC")`.
#'
#' @return A vector containing the performance metric values.
#' @keywords regression-performance model-evaluation
#' @export
#' @examples
#' set.seed(654321)
#' x <- rnorm(1000, 0.2, 0.5)
#' y <- x^2 + rnorm(1000, 0, 0.1)
#' y_fit <- predict(lm(y ~ x))
#' print(performance(y, y_fit, measures = c("MSE", "RSQ")))
#'
################################################################################
performance <- function(y_obs, y_pred, measures = c("NRMSE", "NMAE", "PCC")) {

  ## Initialize results array...
  perf_vals <- c()

  for (mes in measures) {

    ## Calculate squared error...
    if (grepl(pattern = "MSE", mes, ignore.case = TRUE)) {
      num <- mean((y_obs - y_pred)^2)
      den <- if (mes == "NRMSE") mean((y_obs - mean(y_obs))^2) else 1
      pow <- if (mes == "MSE") 1 else 0.5
      perf_vals[mes] <- (num / den)^pow
    }

    ## Calculate absolute error...
    else if (grepl(pattern = "MAE", mes, ignore.case = TRUE)) {
      num <- mean(abs(y_obs - y_pred))
      den <- if (mes == "NMAE") mean(abs(y_obs - mean(y_obs))) else 1
      perf_vals[mes] <- num / den
    }

    ## Calculate similarity measures...
    else if (grepl(pattern = "CC", mes, ignore.case = TRUE)) {
      alg <- if (mes == "SCC") "spearman" else "pearson"
      perf_vals[mes] <- stats::cor(y_obs, y_pred, method = alg)
    }

    else if (toupper(mes) == "RSQ") {
      perf_vals[mes] <- summary(stats::lm(y_obs ~ y_pred))$r.squared
    }

    ## Doesn't match any...
    else
      stop("Invalid measure! Please check the available options using ?get_perf")
  }

  perf_vals
}
