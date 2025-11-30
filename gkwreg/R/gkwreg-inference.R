#' @title Extract Log-Likelihood from Generalized Kumaraswamy Regression Models
#'
#' @description
#' Extracts the log-likelihood value from a fitted Generalized Kumaraswamy (GKw)
#' regression model object.
#'
#' @param object An object of class \code{"gkwreg"}, typically obtained from
#'   \code{\link{gkwreg}}.
#' @param ... Currently not used.
#'
#' @details
#' The log-likelihood is extracted from the fitted model object and returned as
#' an object of class \code{"logLik"} with appropriate attributes for the number
#' of parameters (\code{df}) and observations (\code{nobs}). These attributes
#' are required for information criteria calculations.
#'
#' For a GKw regression model with parameter vector \eqn{\theta}, the log-likelihood
#' is defined as:
#' \deqn{\ell(\theta \mid y) = \sum_{i=1}^n \log f(y_i; \alpha_i, \beta_i, \gamma_i, \delta_i, \lambda_i)}
#' where \eqn{f(\cdot)} is the probability density function of the specified GKw
#' family distribution, and the parameters may depend on covariates through link
#' functions.
#'
#' @return An object of class \code{"logLik"} containing the log-likelihood value
#'   with the following attributes:
#'   \describe{
#'     \item{\code{df}}{Number of estimated parameters}
#'     \item{\code{nobs}}{Number of observations}
#'   }
#'
#' @author Lopes, J. E.
#'
#' @seealso \code{\link{gkwreg}}, \code{\link{AIC.gkwreg}}, \code{\link{BIC.gkwreg}}
#'
#' @examples
#' \donttest{
#' # Load example data
#' data(GasolineYield)
#'
#' # Fit a Kumaraswamy regression model
#' fit <- gkwreg(yield ~ batch + temp, data = GasolineYield, family = "kw")
#'
#' # Extract log-likelihood
#' ll <- logLik(fit)
#' print(ll)
#'
#' # Access attributes
#' cat("Log-likelihood:", as.numeric(ll), "\n")
#' cat("Parameters:", attr(ll, "df"), "\n")
#' cat("Observations:", attr(ll, "nobs"), "\n")
#' }
#'
#' @importFrom stats logLik
#' @method logLik gkwreg
#' @export
logLik.gkwreg <- function(object, ...) {
  if (!inherits(object, "gkwreg")) {
    stop("'object' must be of class 'gkwreg'", call. = FALSE)
  }

  # Extract log-likelihood value
  val <- object$loglik
  if (is.null(val) || !is.finite(val)) {
    stop("log-likelihood not available in fitted model object", call. = FALSE)
  }

  # Extract number of parameters
  df <- object$npar
  if (is.null(df)) {
    df <- length(object$coefficients)
  }

  # Extract number of observations
  nobs <- object$nobs
  if (is.null(nobs)) {
    nobs <- length(object$y)
  }

  # Create logLik object with attributes
  structure(
    val,
    df = as.integer(df),
    nobs = as.integer(nobs),
    class = "logLik"
  )
}


#' @title Akaike Information Criterion for GKw Regression Models
#'
#' @description
#' Calculates the Akaike Information Criterion (AIC) for fitted Generalized
#' Kumaraswamy regression models.
#'
#' @param object An object of class \code{"gkwreg"}, typically obtained from
#'   \code{\link{gkwreg}}.
#' @param ... Optionally more fitted model objects.
#' @param k Numeric, the penalty per parameter. Default is \code{k = 2} for
#'   classical AIC. Setting \code{k = log(n)} gives BIC-equivalent penalty.
#'
#' @details
#' The AIC is computed as:
#' \deqn{AIC = -2\ell(\hat{\theta}) + k \cdot p}
#' where \eqn{\ell(\hat{\theta})} is the maximized log-likelihood and \eqn{p}
#' is the number of estimated parameters.
#'
#' When multiple objects are provided, a data frame comparing all models is
#' returned. Lower AIC values indicate better models, balancing goodness-of-fit
#' against model complexity.
#'
#' For small sample sizes, consider the corrected AIC (AICc):
#' \deqn{AICc = AIC + \frac{2p(p+1)}{n-p-1}}
#' where \eqn{n} is the sample size. This correction is not automatically applied
#' but can be calculated manually.
#'
#' @return If only one object is provided, returns a numeric value with the AIC.
#'   If multiple objects are provided, returns a data frame with columns \code{df}
#'   and \code{AIC}, with rows named according to the object names in the call.
#'
#' @author Lopes, J. E.
#'
#' @references
#' Akaike, H. (1974). A new look at the statistical model identification.
#' \emph{IEEE Transactions on Automatic Control}, \strong{19}(6), 716--723.
#' \doi{10.1109/TAC.1974.1100705}
#'
#' Burnham, K. P., & Anderson, D. R. (2004). Multimodel inference: Understanding
#' AIC and BIC in model selection. \emph{Sociological Methods & Research},
#' \strong{33}(2), 261--304. \doi{10.1177/0049124104268644}
#'
#' @seealso \code{\link{gkwreg}}, \code{\link{logLik.gkwreg}}, \code{\link{BIC.gkwreg}}
#'
#' @examples
#' \donttest{
#' # Load example data
#' data(GasolineYield)
#'
#' # Fit competing models
#' fit1 <- gkwreg(yield ~ batch, data = GasolineYield, family = "kw")
#' fit2 <- gkwreg(yield ~ batch + temp, data = GasolineYield, family = "kw")
#' fit3 <- gkwreg(yield ~ temp, data = GasolineYield, family = "kw")
#'
#' # Calculate AIC for single model
#' AIC(fit1)
#'
#' # Compare multiple models (with proper names)
#' AIC(fit1, fit2, fit3)
#'
#' # Use different penalty
#' AIC(fit1, k = 4)
#' }
#'
#' @importFrom stats AIC logLik
#' @method AIC gkwreg
#' @export
AIC.gkwreg <- function(object, ..., k = 2) {
  if (!inherits(object, "gkwreg")) {
    stop("'object' must be of class 'gkwreg'", call. = FALSE)
  }

  # Handle multiple objects
  dots <- list(...)
  if (length(dots) > 0L) {
    # Get the names of the objects from the call
    object_names <- as.character(match.call(expand.dots = FALSE)$...)

    # If names are not informative (e.g., from do.call), try deparse
    if (length(object_names) == 0L || all(object_names == "")) {
      object_names <- paste0("Model", seq_along(dots))
    }

    # Get the name of the first object
    first_name <- as.character(substitute(object))
    if (length(first_name) > 1L) {
      first_name <- deparse(substitute(object))
    }

    # Combine all objects
    all_objects <- c(list(object), dots)
    all_names <- c(first_name, object_names)

    # Calculate AIC for each object
    aic_vals <- vapply(all_objects, function(obj) {
      if (inherits(obj, "gkwreg")) {
        ll <- logLik(obj)
        df <- attr(ll, "df")
        if (is.null(df) || is.na(df)) {
          return(NA_real_)
        }
        return(-2 * as.numeric(ll) + k * df)
      } else {
        # For non-gkwreg objects, use stats::AIC
        return(stats::AIC(obj, k = k))
      }
    }, FUN.VALUE = numeric(1L))

    # Get degrees of freedom for each object
    df_vals <- vapply(all_objects, function(obj) {
      if (inherits(obj, "gkwreg")) {
        ll <- logLik(obj)
        df <- attr(ll, "df")
        return(if (is.null(df) || is.na(df)) NA_integer_ else as.integer(df))
      } else {
        ll <- logLik(obj)
        df <- attr(ll, "df")
        return(if (is.null(df) || is.na(df)) NA_integer_ else as.integer(df))
      }
    }, FUN.VALUE = integer(1L))

    # Create result data frame
    result <- data.frame(
      df = df_vals,
      AIC = aic_vals,
      row.names = all_names,
      stringsAsFactors = FALSE
    )

    return(result)
  }

  # Single object: use stored value if k = 2, otherwise compute
  if (identical(k, 2) && !is.null(object$aic)) {
    return(object$aic)
  }

  # Compute AIC from log-likelihood
  ll <- logLik(object)
  df <- attr(ll, "df")

  if (is.null(df) || is.na(df)) {
    stop("number of parameters not available", call. = FALSE)
  }

  return(-2 * as.numeric(ll) + k * df)
}


#' @title Bayesian Information Criterion for GKw Regression Models
#'
#' @description
#' Calculates the Bayesian Information Criterion (BIC), also known as the
#' Schwarz Information Criterion (SIC), for fitted Generalized Kumaraswamy
#' regression models.
#'
#' @param object An object of class \code{"gkwreg"}, typically obtained from
#'   \code{\link{gkwreg}}.
#' @param ... Optionally more fitted model objects.
#'
#' @details
#' The BIC is computed as:
#' \deqn{BIC = -2\ell(\hat{\theta}) + p \cdot \log(n)}
#' where \eqn{\ell(\hat{\theta})} is the maximized log-likelihood, \eqn{p} is
#' the number of estimated parameters, and \eqn{n} is the sample size.
#'
#' When multiple objects are provided, a data frame comparing all models is
#' returned. Lower BIC values indicate better models. BIC penalizes model
#' complexity more heavily than AIC, particularly for large samples, and tends
#' to favor more parsimonious models.
#'
#' The BIC can be derived from a Bayesian perspective as an approximation to
#' the logarithm of the Bayes factor, under certain regularity conditions and
#' assuming uniform priors.
#'
#' @return If only one object is provided, returns a numeric value with the BIC.
#'   If multiple objects are provided, returns a data frame with columns \code{df}
#'   and \code{BIC}, with rows named according to the object names in the call.
#'
#' @author Lopes, J. E.
#'
#' @references
#' Schwarz, G. (1978). Estimating the dimension of a model.
#' \emph{The Annals of Statistics}, \strong{6}(2), 461--464.
#' \doi{10.1214/aos/1176344136}
#'
#' @seealso \code{\link{gkwreg}}, \code{\link{logLik.gkwreg}}, \code{\link{AIC.gkwreg}}
#'
#' @examples
#' \donttest{
#' # Load example data
#' data(GasolineYield)
#'
#' # Fit competing models
#' fit1 <- gkwreg(yield ~ batch, data = GasolineYield, family = "kw")
#' fit2 <- gkwreg(yield ~ batch + temp, data = GasolineYield, family = "kw")
#' fit3 <- gkwreg(yield ~ temp, data = GasolineYield, family = "kw")
#'
#' # Calculate BIC for single model
#' BIC(fit1)
#'
#' # Compare multiple models (with proper names)
#' BIC(fit1, fit2, fit3)
#' }
#'
#' @importFrom stats BIC logLik
#' @method BIC gkwreg
#' @export
BIC.gkwreg <- function(object, ...) {
  if (!inherits(object, "gkwreg")) {
    stop("'object' must be of class 'gkwreg'", call. = FALSE)
  }

  # Handle multiple objects
  dots <- list(...)
  if (length(dots) > 0L) {
    # Get the names of the objects from the call
    object_names <- as.character(match.call(expand.dots = FALSE)$...)

    # If names are not informative (e.g., from do.call), try deparse
    if (length(object_names) == 0L || all(object_names == "")) {
      object_names <- paste0("Model", seq_along(dots))
    }

    # Get the name of the first object
    first_name <- as.character(substitute(object))
    if (length(first_name) > 1L) {
      first_name <- deparse(substitute(object))
    }

    # Combine all objects
    all_objects <- c(list(object), dots)
    all_names <- c(first_name, object_names)

    # Calculate BIC for each object
    bic_vals <- vapply(all_objects, function(obj) {
      if (inherits(obj, "gkwreg")) {
        ll <- logLik(obj)
        df <- attr(ll, "df")
        nobs <- attr(ll, "nobs")
        if (is.null(df) || is.na(df) || is.null(nobs) || is.na(nobs)) {
          return(NA_real_)
        }
        return(-2 * as.numeric(ll) + df * log(nobs))
      } else {
        # For non-gkwreg objects, use stats::BIC
        return(stats::BIC(obj))
      }
    }, FUN.VALUE = numeric(1L))

    # Get degrees of freedom for each object
    df_vals <- vapply(all_objects, function(obj) {
      if (inherits(obj, "gkwreg")) {
        ll <- logLik(obj)
        df <- attr(ll, "df")
        return(if (is.null(df) || is.na(df)) NA_integer_ else as.integer(df))
      } else {
        ll <- logLik(obj)
        df <- attr(ll, "df")
        return(if (is.null(df) || is.na(df)) NA_integer_ else as.integer(df))
      }
    }, FUN.VALUE = integer(1L))

    # Create result data frame
    result <- data.frame(
      df = df_vals,
      BIC = bic_vals,
      row.names = all_names,
      stringsAsFactors = FALSE
    )

    return(result)
  }

  # Single object: use stored value if available, otherwise compute
  if (!is.null(object$bic)) {
    return(object$bic)
  }

  # Compute BIC from log-likelihood
  ll <- logLik(object)
  df <- attr(ll, "df")
  nobs <- attr(ll, "nobs")

  if (is.null(df) || is.na(df)) {
    stop("number of parameters not available", call. = FALSE)
  }

  if (is.null(nobs) || is.na(nobs) || nobs <= 0) {
    stop("number of observations not available", call. = FALSE)
  }

  return(-2 * as.numeric(ll) + df * log(nobs))
}
