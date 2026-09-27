#' Print a `glmELE` Object
#'
#' @description
#' Prints the function call and the estimated coefficient matrices from a fitted
#' `glmELE` object.
#'
#' @param x An object of class \code{"glmELE"}.
#' @param digits The number of significant digits to print. Defaults to
#'   \code{max(3L, getOption("digits") - 3L)}.
#' @param ... Additional arguments passed to methods.
#'
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' data(brfss, package = "postlink")
#'
#' adj_object <- adjELE(linked.data = brfss,
#'                      m.rate = unique(brfss$m.rate),
#'                      blocks = imonth,
#'                      weight.matrix = "BLUE")
#'
#' fit <- plglm(Weight ~ Height + Physhlth + Menthlth + Exerany,
#'              family = "gaussian", adjustment = adj_object)
#'
#' print(fit)
#'
#' @export
print.glmELE <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")

  cat("Coefficients:\n")
  print.default(format(x$coefficients, digits = digits), print.gap = 2, quote = FALSE)
  cat("\n")

  invisible(x)
}

#' Summarize a `glmELE` Object
#'
#' @description
#' Summarizes the results from a \code{glmELE} fit, providing coefficient estimates,
#' standard errors, test statistics, and p-values for each weighting method used.
#'
#' @param object An object of class \code{"glmELE"}.
#' @param ... Additional arguments passed to methods.
#'
#' @return An object of class \code{"summary.glmELE"}, which is a list containing:
#'   \item{call}{The matched call.}
#'   \item{family}{The family object used.}
#'   \item{coefficients}{A list of matrices, one per weighting method, containing estimates, SEs, t/z values, and p-values.}
#'   \item{dispersion}{The estimated dispersion parameter(s).}
#'   \item{deviance}{The deviance of the fitted model.}
#'   \item{df.residual}{The residual degrees of freedom.}
#'
#' @examples
#' data(brfss, package = "postlink")
#'
#' adj_object <- adjELE(linked.data = brfss,
#'                      m.rate = unique(brfss$m.rate),
#'                      blocks = imonth,
#'                      weight.matrix = "BLUE")
#'
#' fit <- plglm(Weight ~ Height + Physhlth + Menthlth + Exerany,
#'              family = "gaussian", adjustment = adj_object)
#'
#' summary(fit)
#'
#' @export
summary.glmELE <- function(object, ...) {

  # Extract components
  coef_mat <- object$coefficients
  var_list <- object$var
  fam <- object$family
  df_r <- object$df.residual

  # Identify methods (rows of coefficient matrix)
  methods <- rownames(coef_mat)
  coef_summaries <- list()

  # Loop over each method to build the summary table
  for (m in methods) {
    est <- coef_mat[m, ]

    # Extract variance for this method
    # Note: object$var is a named list.
    if (!is.null(var_list[[m]])) {
      se <- sqrt(diag(var_list[[m]]))
    } else {
      warning(paste("Variance matrix not found for method:", m))
      se <- rep(NA, length(est))
    }

    # Calculate t or z statistics
    t_val <- est / se

    # Calculate p-values
    if (fam$family %in% c("gaussian", "Gamma")) {
      p_val <- 2 * stats::pt(abs(t_val), df = df_r, lower.tail = FALSE)
      stat_name <- "t value"
      p_name <- "Pr(>|t|)"
    } else {
      p_val <- 2 * stats::pnorm(-abs(t_val))
      stat_name <- "z value"
      p_name <- "Pr(>|z|)"
    }

    # Combine into matrix
    tab <- cbind(Estimate = est,
                 `Std. Error` = se,
                 `Stat` = t_val,
                 `P` = p_val)
    colnames(tab)[3:4] <- c(stat_name, p_name)

    coef_summaries[[m]] <- tab
  }

  # Construct result
  res <- list(
    call = object$call,
    family = fam,
    coefficients = coef_summaries,
    deviance = object$deviance,
    df.residual = object$df.residual,
    dispersion = if (!is.null(object$dispersion)) object$dispersion else NULL
  )

  class(res) <- "summary.glmELE"
  return(res)
}

#' @noRd
#' @export
print.summary.glmELE <- function(x, digits = max(3L, getOption("digits") - 3L),
                                 signif.stars = getOption("show.signif.stars"), ...) {

  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n", sep = "")

  cat("\nFamily: ", x$family$family, "\n", sep = "")

  # Iterate over each method's summary
  for (m in names(x$coefficients)) {
    cat(paste0("\n--- Weighting Method: ", m, " ---\n"))
   stats::printCoefmat(x$coefficients[[m]], digits = digits,
                       signif.stars = signif.stars, has.Pvalue = TRUE,
                       na.print = "NA", ...)

    # Print dispersion if available for this method
    if (!is.null(x$dispersion) && m %in% names(x$dispersion)) {
      cat(paste0("\nDispersion parameter for ", x$family$family, " family: ",
                 format(x$dispersion[m], digits = digits), "\n"))
    }
  }

  cat("\n")
  invisible(x)
}

#' Extract Variance-Covariance Matrix from a `glmELE` Object
#'
#' @description
#' Extracts the variance-covariance matrix of the main parameters for a specific
#' weighting method.
#'
#' @param object An object of class \code{"glmELE"}.
#' @param weight.matrix Character string specifying which weighting method to return.
#'   Defaults to the first method found in the object.
#' @param ... Additional arguments passed to methods.
#'
#' @return A matrix of the estimated covariances between the parameter estimates.
#'
#' @examples
#' data(brfss, package = "postlink")
#'
#' adj_object <- adjELE(linked.data = brfss,
#'                      m.rate = unique(brfss$m.rate),
#'                      blocks = imonth,
#'                      weight.matrix = "BLUE")
#'
#' fit <- plglm(Weight ~ Height + Physhlth + Menthlth + Exerany,
#'              family = "gaussian", adjustment = adj_object)
#'
#' vcov(fit)
#'
#' @export
vcov.glmELE <- function(object, weight.matrix = NULL, ...) {

  methods <- names(object$var)

  if (is.null(weight.matrix)) {
    weight.matrix <- methods[1]
  }

  weight.matrix <- match.arg(weight.matrix, methods)

  return(object$var[[weight.matrix]])
}

#' Confidence Intervals for `glmELE` Objects
#'
#' @description
#' Computes Wald confidence intervals for one or more parameters in a \code{glmELE} object.
#'
#' @param object An object of class \code{"glmELE"}.
#' @param parm A specification of which parameters are to be given confidence intervals,
#'   either a vector of numbers or names. If missing, all parameters are considered.
#' @param level The confidence level required.
#' @param weight.matrix Character string specifying the weighting method to use.
#'   Defaults to the first method found.
#' @param ... Additional arguments passed to methods.
#'
#' @return A matrix (or vector) with columns giving lower and upper confidence limits for each parameter.
#'
#' @examples
#' data(brfss, package = "postlink")
#'
#' adj_object <- adjELE(linked.data = brfss,
#'                      m.rate = unique(brfss$m.rate),
#'                      blocks = imonth,
#'                      weight.matrix = "BLUE")
#'
#' fit <- plglm(Weight ~ Height + Physhlth + Menthlth + Exerany,
#'              family = "gaussian", adjustment = adj_object)
#'
#' confint(fit)
#'
#' @export
confint.glmELE <- function(object, parm, level = 0.95, weight.matrix = NULL, ...) {

  # Select weight matrix
  if (is.null(weight.matrix)) {
    weight.matrix <- rownames(object$coefficients)[1]
  }
  if (!weight.matrix %in% rownames(object$coefficients)) {
    stop(paste("Method", weight.matrix, "not found in object coefficients."))
  }

  # Extract estimates and SEs
  est <- object$coefficients[weight.matrix, ]
  vc <- object$var[[weight.matrix]]
  ses <- sqrt(diag(vc))

  # Handle 'parm' argument
  pnames <- names(est)
  if (missing(parm)) {
    parm <- pnames
  } else if (is.numeric(parm)) {
    parm <- pnames[parm]
  }

  # Calculate Critical Value
  alpha <- 1 - level
  if (object$family$family %in% c("gaussian", "Gamma")) {
    crit <- stats::qt(1 - alpha / 2, df = object$df.residual)
  } else {
    crit <- stats::qnorm(1 - alpha / 2)
  }

  # Calculate Intervals
  ci <- array(NA, dim = c(length(parm), 2),
              dimnames = list(parm, c(paste(100 * alpha / 2, "%"),
                                      paste(100 * (1 - alpha / 2), "%"))))

  ci[, 1] <- est[parm] - crit * ses[parm]
  ci[, 2] <- est[parm] + crit * ses[parm]

  return(ci)
}

#' Predictions for `glmELE` Objects
#'
#' @description
#' Obtains predictions and optionally standard errors from a fitted \code{glmELE} object.
#' This method handles new data, factor level consistency, offsets, and confidence intervals.
#'
#' @param object An object of class \code{"glmELE"}.
#' @param newdata An optional data frame in which to look for variables with which to predict.
#'   If omitted, the fitted linear predictors from the object are used.
#' @param weight.matrix Character string specifying the weighting method to use
#'   (e.g., "ratio", "LL", "BLUE"). Defaults to the first method found in the object.
#' @param type The type of prediction required. The default is on the scale of the linear predictors;
#'   the alternative "response" is on the scale of the response variable.
#' @param se.fit Logical; if \code{TRUE}, standard errors are returned.
#' @param interval Type of interval calculation. Can be "none" or "confidence".
#' @param level The confidence level required (default 0.95).
#' @param na.action A function determining what should be done with missing values in \code{newdata}.
#'   The default is to predict \code{NA}.
#' @param ... Additional arguments passed to methods.
#'
#' @return If \code{se.fit = FALSE} and \code{interval = "none"}, a vector of predictions.
#'   Otherwise, a list containing:
#'   \item{fit}{Predictions (or a matrix with columns \code{fit}, \code{lwr}, \code{upr} if intervals are requested).}
#'   \item{se.fit}{Estimated standard errors (if requested).}
#'   \item{residual.scale}{The dispersion parameter used.}
#'
#' @importFrom stats formula terms model.frame model.matrix delete.response na.pass napredict qt qnorm family
#'
#' @examples
#' data(brfss, package = "postlink")
#'
#' adj_object <- adjELE(linked.data = brfss,
#'                      m.rate = unique(brfss$m.rate),
#'                      blocks = imonth,
#'                      weight.matrix = "BLUE")
#'
#' fit <- plglm(Weight ~ Height + Physhlth + Menthlth + Exerany,
#'              family = "gaussian", adjustment = adj_object)
#'
#' predict(fit)
#'
#' @export
predict.glmELE <- function(object, newdata = NULL, weight.matrix = NULL,
                           type = c("link", "response"),
                           se.fit = FALSE,
                           interval = c("none", "confidence"), level = 0.95,
                           na.action = stats::na.pass, ...) {

  type <- match.arg(type)
  interval <- match.arg(interval)

  # Select Weight Matrix
  if (is.null(weight.matrix)) {
    weight.matrix <- rownames(object$coefficients)[1]
  }
  if (!weight.matrix %in% rownames(object$coefficients)) {
    stop(paste("Method", weight.matrix, "not found in object coefficients."))
  }

  # Extract Coefficients & Dispersion
  beta <- object$coefficients[weight.matrix, ]
  dispersion <- if (!is.null(object$dispersion)) object$dispersion[weight.matrix] else 1

  # Extract Model Information
  # We need Terms to handle factors and offsets correctly.
  # plglm() attaches the model frame to object$model.
  if (is.null(object$model)) {
    stop("The object does not contain the model frame component 'model'. ",
         "Ensure the model was fitted with 'model = TRUE'.")
  }

  Terms <- attr(object$model, "terms")

  # Handle Prediction Data (lp and X)
  if (is.null(newdata)) {
    # In-sample prediction-Use stored linear predictors for maximum precision and speed
    lp <- object$linear.predictors[, weight.matrix]

    # Reconstruct X only if SEs are needed
    if (se.fit || interval == "confidence") {
      # Use the stored model frame to recreate X exactly as used in fitting
      X <- stats::model.matrix(Terms, object$model, contrasts.arg = attr(object$model, "contrasts"))

      # Extract offset if present
      off_num <- attr(Terms, "offset")
    }

  } else {
    # New data prediction
    xlevels <- .getXlevels(Terms, object$model)
    Terms <- stats::delete.response(Terms)
    mf <- stats::model.frame(Terms, newdata, na.action = na.action, xlev = xlevels)

    offset <- rep(0, nrow(mf))
    if (!is.null(off.num <- attr(Terms, "offset"))) {
      for (i in off.num) offset <- offset + eval(attr(Terms, "variables")[[i + 1]], newdata)
    }

    # Create Design Matrix X
    contrasts.old <- attr(stats::model.matrix(Terms, object$model), "contrasts")
    X <- stats::model.matrix(Terms, mf, contrasts.arg = contrasts.old)

    # Compute Linear Predictor
    lp <- as.vector(X %*% beta)
    if (!is.null(offset)) lp <- lp + offset
  }

  # Calculate Standard Errors
  se_lp <- NULL
  if (se.fit || interval == "confidence") {
    # Get Variance-Covariance Matrix for the specific method
    V <- object$var[[weight.matrix]]

    if (is.null(V)) {
      warning("Variance matrix missing for this method. SEs set to NA.")
      se_lp <- rep(NA, length(lp))
    } else {
      var_lp <- rowSums((X %*% V) * X)
      se_lp <- sqrt(var_lp)
    }
  }

  # Calculate Confidence Intervals (on Link Scale)
  lower <- upper <- NULL
  if (interval == "confidence") {
    alpha <- 1 - level

    # Use t-distribution for Gaussian/Gamma (with estimated dispersion)
    # Use Normal for Binomial/Poisson (asymptotic)
    if (object$family$family %in% c("gaussian", "Gamma")) {
      crit <- stats::qt(1 - alpha / 2, df = object$df.residual)
    } else {
      crit <- stats::qnorm(1 - alpha / 2)
    }

    lower <- lp - crit * se_lp
    upper <- lp + crit * se_lp
  }

  # Transform to Response Scale
  fit_val <- lp

  if (type == "response") {
    fam <- object$family
    fit_val <- fam$linkinv(lp)

    # Transform intervals
    if (!is.null(lower)) {
      lower <- fam$linkinv(lower)
      upper <- fam$linkinv(upper)
    }
  }

  # Format Output & Handle NAs
  # If na.action padded the model frame (e.g. na.exclude), we pad the output.
  # Determine the na.action to apply
  na_act <- attr(if (is.null(newdata)) object$model else mf, "na.action")

  # Formatting logic matching standard R predict()
  if (!se.fit && interval == "none") {
    res <- fit_val
    if (!is.null(na_act)) res <- stats::napredict(na_act, res)
    return(res)
  }

  # Build return object
  res <- list(fit = fit_val)

  if (interval == "confidence") {
    mat <- cbind(fit = fit_val, lwr = lower, upr = upper)
    colnames(mat) <- c("fit", "lwr", "upr")
    res$fit <- mat
  }

  if (se.fit) {
    res$se.fit <- se_lp
    res$residual.scale <- sqrt(dispersion)
  }

  # Apply NA handling to all components
  if (!is.null(na_act)) {
    res$fit <- stats::napredict(na_act, res$fit)
    if (!is.null(res$se.fit)) res$se.fit <- stats::napredict(na_act, res$se.fit)
  }

  return(res)
}
