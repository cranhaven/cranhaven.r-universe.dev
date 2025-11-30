#' @title Analysis of Deviance for GKw Regression Models
#'
#' @description
#' Computes an analysis of deviance table for one or more fitted Generalized
#' Kumaraswamy (GKw) regression model objects. When multiple models are provided,
#' likelihood ratio tests are performed to compare nested models.
#'
#' @param object An object of class \code{"gkwreg"}, typically obtained from
#'   \code{\link{gkwreg}}.
#' @param ... Additional objects of class \code{"gkwreg"} for model comparison.
#'   Models must be nested and fitted to the same dataset.
#' @param test A character string specifying the test statistic to use. Currently
#'   only \code{"Chisq"} (default) is supported, which performs likelihood ratio
#'   tests. Can also be \code{"none"} for no tests.
#'
#' @details
#' When a single model is provided, the function returns a table showing the
#' residual degrees of freedom and deviance.
#'
#' When multiple models are provided, the function compares them using likelihood
#' ratio tests (LRT). Models are automatically ordered by their complexity
#' (degrees of freedom). The LRT statistic is computed as:
#' \deqn{LRT = 2(\ell_1 - \ell_0)}
#' where \eqn{\ell_1} is the log-likelihood of the more complex model and
#' \eqn{\ell_0} is the log-likelihood of the simpler (nested) model. Under the
#' null hypothesis that the simpler model is adequate, the LRT statistic follows
#' a chi-squared distribution with degrees of freedom equal to the difference in
#' the number of parameters between the models.
#'
#' \strong{Important}: This method assumes that the models being compared are
#' nested (i.e., one model is a special case of the other) and fitted to the
#' same data. Comparing non-nested models or models fitted to different datasets
#' will produce unreliable results. Use \code{\link{AIC}} or \code{\link{BIC}}
#' for comparing non-nested models.
#'
#' The deviance is defined as \eqn{-2 \times \text{log-likelihood}}. For models
#' fitted by maximum likelihood, smaller (more negative) deviances indicate
#' better fit. Note that deviance can be negative when the log-likelihood is
#' positive, which occurs when density values exceed 1 (common in continuous
#' distributions on bounded intervals). What matters for inference is the
#' \emph{change} in deviance between models, which should be positive when
#' the more complex model fits better.
#'
#' @return An object of class \code{c("anova.gkwreg", "anova", "data.frame")},
#'   with the following columns:
#'   \describe{
#'     \item{\code{Resid. Df}}{Residual degrees of freedom}
#'     \item{\code{Resid. Dev}}{Residual deviance (-2 × log-likelihood)}
#'     \item{\code{Df}}{Change in degrees of freedom (for model comparisons)}
#'     \item{\code{Deviance}}{Change in deviance (for model comparisons)}
#'     \item{\code{Pr(>Chi)}}{P-value from the chi-squared test (if \code{test = "Chisq"})}
#'   }
#'
#' @author Lopes, J. E.
#'
#' @references
#' Wilks, S. S. (1938). The large-sample distribution of the likelihood ratio
#' for testing composite hypotheses. \emph{The Annals of Mathematical Statistics},
#' \strong{9}(1), 60--62. \doi{10.1214/aoms/1177732360}
#'
#' Pawitan, Y. (2001). \emph{In All Likelihood: Statistical Modelling and
#' Inference Using Likelihood}. Oxford University Press.
#'
#' @seealso \code{\link{gkwreg}}, \code{\link{logLik.gkwreg}}, \code{\link{AIC.gkwreg}},
#'   \code{\link{BIC.gkwreg}}, \code{\link{lrtest}}
#'
#' @examples
#' \donttest{
#' # Load example data
#' data(GasolineYield)
#'
#' # Fit a series of nested models
#' fit1 <- gkwreg(yield ~ 1, data = GasolineYield, family = "kw")
#' fit2 <- gkwreg(yield ~ temp, data = GasolineYield, family = "kw")
#' fit3 <- gkwreg(yield ~ batch + temp, data = GasolineYield, family = "kw")
#'
#' # ANOVA table for single model
#' anova(fit3)
#'
#' # Compare nested models using likelihood ratio tests
#' anova(fit1, fit2, fit3)
#' #> Model 1 vs 2: Adding temperature is highly significant (p < 0.001)
#' #> Model 2 vs 3: Adding batch is highly significant (p < 0.001)
#'
#' # Compare two models
#' anova(fit2, fit3, test = "Chisq")
#'
#' # Suppress test statistics
#' anova(fit1, fit2, fit3, test = "none")
#' }
#'
#' @importFrom stats pchisq anova
#' @method anova gkwreg
#' @export
anova.gkwreg <- function(object, ..., test = c("Chisq", "none")) {
  # Match test argument
  test <- match.arg(test)

  # Check object class
  if (!inherits(object, "gkwreg")) {
    stop("'object' must be of class 'gkwreg'", call. = FALSE)
  }

  # Get additional objects
  dots <- list(...)

  # Single model case
  if (length(dots) == 0L) {
    return(.anova_single_gkwreg(object))
  }

  # Multiple models case
  # Get model names
  object_names <- as.character(match.call(expand.dots = FALSE)$...)
  if (length(object_names) == 0L || all(object_names == "")) {
    object_names <- paste0("Model", seq_along(dots))
  }

  first_name <- as.character(substitute(object))
  if (length(first_name) > 1L) {
    first_name <- deparse(substitute(object))
  }

  # Combine all models
  models <- c(list(object), dots)
  model_names <- c(first_name, object_names)

  # Check that all objects are gkwreg models
  is_gkwreg <- vapply(models, inherits, logical(1L), "gkwreg")
  if (!all(is_gkwreg)) {
    stop("all models must be of class 'gkwreg'", call. = FALSE)
  }

  # Perform comparison
  result <- .anova_multiple_gkwreg(models, model_names, test)

  return(result)
}


#' @keywords internal
#' @noRd
.anova_single_gkwreg <- function(object) {
  # Extract information
  ll <- logLik(object)
  df_model <- attr(ll, "df")
  nobs <- attr(ll, "nobs")
  df_resid <- nobs - df_model
  deviance <- -2 * as.numeric(ll)

  # Create data frame
  result <- data.frame(
    "Resid. Df" = df_resid,
    "Resid. Dev" = deviance,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  # Add model information as attributes
  attr(result, "heading") <- c(
    "Analysis of Deviance Table\n",
    paste("Model:", deparse(object$call))
  )

  # Set class - note the specific class first
  class(result) <- c("anova.gkwreg", "anova", "data.frame")

  return(result)
}


#' @keywords internal
#' @noRd
.anova_multiple_gkwreg <- function(models, model_names, test) {
  n_models <- length(models)

  # Extract log-likelihoods and df
  logliks <- lapply(models, logLik)

  # Get number of observations for each model (use numeric, not integer)
  nobs_vec <- vapply(logliks, function(ll) {
    as.numeric(attr(ll, "nobs"))
  }, FUN.VALUE = numeric(1L))

  # Check that all models use the same number of observations
  if (length(unique(nobs_vec)) > 1L) {
    warning("models are not all fitted to the same number of observations",
      call. = FALSE
    )
  }
  nobs <- nobs_vec[1L]

  # Get degrees of freedom (number of parameters) - use numeric
  df_model <- vapply(logliks, function(ll) {
    as.numeric(attr(ll, "df"))
  }, FUN.VALUE = numeric(1L))

  # Get log-likelihood values
  ll_vals <- vapply(logliks, as.numeric, FUN.VALUE = numeric(1L))

  # Calculate deviances
  deviances <- -2 * ll_vals

  # Calculate residual df
  df_resid <- nobs - df_model

  # Order models by complexity (df)
  ord <- order(df_model)
  model_names <- model_names[ord]
  df_model <- df_model[ord]
  df_resid <- df_resid[ord]
  deviances <- deviances[ord]
  ll_vals <- ll_vals[ord]

  # Calculate differences
  df_diff <- c(NA, diff(df_model))
  dev_diff <- c(NA, -diff(deviances)) # Negative because deviance decreases

  # Initialize result data frame
  result <- data.frame(
    "Resid. Df" = df_resid,
    "Resid. Dev" = deviances,
    "Df" = df_diff,
    "Deviance" = dev_diff,
    check.names = FALSE,
    row.names = model_names,
    stringsAsFactors = FALSE
  )

  # Add test statistics if requested
  if (test == "Chisq") {
    # Calculate p-values from chi-squared distribution
    pvals <- rep(NA_real_, n_models)

    for (i in 2:n_models) {
      if (!is.na(df_diff[i]) && df_diff[i] > 0 && !is.na(dev_diff[i])) {
        if (dev_diff[i] >= 0) {
          pvals[i] <- pchisq(dev_diff[i], df = df_diff[i], lower.tail = FALSE)
        } else {
          pvals[i] <- NA_real_
          warning("negative deviance change detected; models may not be nested",
            call. = FALSE
          )
        }
      }
    }

    result[["Pr(>Chi)"]] <- pvals
  }

  # Add heading
  heading <- c("Analysis of Deviance Table\n")

  # Add model formulas
  for (i in seq_along(models)) {
    formula_str <- if (!is.null(models[[ord[i]]]$formula)) {
      deparse(models[[ord[i]]]$formula)
    } else if (!is.null(models[[ord[i]]]$call)) {
      deparse(models[[ord[i]]]$call)
    } else {
      "Unknown"
    }
    heading <- c(heading, paste0("Model ", i, ": ", formula_str))
  }

  attr(result, "heading") <- heading

  # Set class - note the specific class first
  class(result) <- c("anova.gkwreg", "anova", "data.frame")

  return(result)
}

#' @title Print Method for ANOVA of GKw Models
#'
#' @description
#' Print method for analysis of deviance tables produced by \code{\link{anova.gkwreg}}.
#'
#' @param x An object of class \code{"anova.gkwreg"} from \code{\link{anova.gkwreg}}.
#' @param digits Minimum number of significant digits to print. Default is
#'   \code{max(getOption("digits") - 2, 3)}.
#' @param signif.stars Logical; if \code{TRUE} (default), significance stars are
#'   printed alongside p-values. Can be controlled globally via
#'   \code{options(show.signif.stars = FALSE)}.
#' @param dig.tst Number of digits for test statistics. Default is \code{digits}.
#' @param ... Additional arguments (currently ignored).
#'
#' @return The object \code{x}, invisibly.
#'
#' @author Lopes, J. E.
#'
#' @seealso \code{\link{anova.gkwreg}}
#'
#' @importFrom stats symnum
#' @method print anova.gkwreg
#' @export
print.anova.gkwreg <- function(x, digits = max(getOption("digits") - 2L, 3L),
                               signif.stars = getOption("show.signif.stars", TRUE),
                               dig.tst = digits,
                               ...) {
  # Print heading
  if (!is.null(attr(x, "heading"))) {
    cat(paste(attr(x, "heading"), collapse = "\n"), "\n\n", sep = "")
  }

  # Check if there are p-values
  has_pval <- "Pr(>Chi)" %in% names(x)

  # Create a matrix for printing (preserves formatting better)
  x_mat <- as.matrix(x)

  # Format each column
  for (i in seq_len(ncol(x))) {
    col_vals <- x[[i]]
    if (is.numeric(col_vals)) {
      # Format numeric columns
      formatted <- character(length(col_vals))
      for (j in seq_along(col_vals)) {
        if (is.na(col_vals[j])) {
          formatted[j] <- ""
        } else if (names(x)[i] == "Pr(>Chi)") {
          # Special formatting for p-values
          formatted[j] <- format.pval(col_vals[j], digits = dig.tst, eps = 1e-4)
        } else {
          # Regular numeric formatting
          formatted[j] <- format(round(col_vals[j], digits = digits),
            nsmall = ifelse(names(x)[i] %in% c("Df"), 0, digits)
          )
        }
      }
      x_mat[, i] <- formatted
    }
  }

  # Convert to data frame for printing
  x_print <- as.data.frame(x_mat, stringsAsFactors = FALSE)
  names(x_print) <- names(x)
  rownames(x_print) <- rownames(x)

  # Add significance stars if requested
  if (has_pval && signif.stars) {
    pval_col <- x[["Pr(>Chi)"]]
    stars <- symnum(pval_col,
      corr = FALSE,
      na = FALSE,
      cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
      symbols = c("***", "**", "*", ".", " ")
    )
    x_print[[ncol(x_print) + 1]] <- format(stars)
    names(x_print)[ncol(x_print)] <- ""
  }

  # Print the table
  print(x_print, quote = FALSE, right = TRUE, row.names = TRUE)

  # Print legend if stars are shown
  if (has_pval && signif.stars) {
    cat("---\n")
    cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
  }

  invisible(x)
}


#' @title Likelihood Ratio Test for Nested GKw Models
#'
#' @description
#' Performs a likelihood ratio test to compare two nested Generalized Kumaraswamy
#' regression models.
#'
#' @param object A fitted model object of class \code{"gkwreg"} (the restricted model).
#' @param object2 A fitted model object of class \code{"gkwreg"} (the full model).
#'
#' @details
#' This function performs a likelihood ratio test (LRT) to compare two nested
#' models. The test statistic is:
#' \deqn{LRT = 2(\ell_{\text{full}} - \ell_{\text{restricted}})}
#' which follows a chi-squared distribution with degrees of freedom equal to
#' the difference in the number of parameters.
#'
#' The models must be nested (one is a special case of the other) and fitted to
#' the same data for the test to be valid.
#'
#' @return A list with class \code{"htest"} containing:
#'   \describe{
#'     \item{\code{statistic}}{The LRT test statistic}
#'     \item{\code{parameter}}{Degrees of freedom for the test}
#'     \item{\code{p.value}}{P-value from the chi-squared distribution}
#'     \item{\code{method}}{Description of the test}
#'     \item{\code{data.name}}{Names of the compared models}
#'   }
#'
#' @author Lopes, J. E.
#'
#' @seealso \code{\link{anova.gkwreg}}
#'
#' @examples
#' \donttest{
#' data(GasolineYield)
#'
#' # Fit nested models
#' fit_restricted <- gkwreg(yield ~ temp, data = GasolineYield, family = "kw")
#' fit_full <- gkwreg(yield ~ batch + temp, data = GasolineYield, family = "kw")
#'
#' # Likelihood ratio test
#' lrtest(fit_restricted, fit_full)
#' }
#'
#' @importFrom stats pchisq
#' @export
lrtest <- function(object, object2) {
  # Check classes
  if (!inherits(object, "gkwreg") || !inherits(object2, "gkwreg")) {
    stop("both 'object' and 'object2' must be of class 'gkwreg'", call. = FALSE)
  }

  # Get log-likelihoods
  ll1 <- logLik(object)
  ll2 <- logLik(object2)

  # Get degrees of freedom
  df1 <- attr(ll1, "df")
  df2 <- attr(ll2, "df")

  # Check number of observations
  nobs1 <- attr(ll1, "nobs")
  nobs2 <- attr(ll2, "nobs")

  if (nobs1 != nobs2) {
    warning("models are not fitted to the same number of observations",
      call. = FALSE
    )
  }

  # Determine which is the restricted model
  if (df1 > df2) {
    # Swap models
    temp <- ll1
    ll1 <- ll2
    ll2 <- temp
    temp <- df1
    df1 <- df2
    df2 <- temp
    temp_name <- deparse(substitute(object))
    object_name <- deparse(substitute(object2))
    object2_name <- temp_name
  } else {
    object_name <- deparse(substitute(object))
    object2_name <- deparse(substitute(object2))
  }

  # Calculate test statistic
  lr_stat <- 2 * (as.numeric(ll2) - as.numeric(ll1))
  df_diff <- df2 - df1

  # Check for negative LRT
  if (lr_stat < 0) {
    warning("negative LRT statistic; models may not be nested or have convergence issues",
      call. = FALSE
    )
  }

  # Calculate p-value
  p_value <- if (lr_stat >= 0 && df_diff > 0) {
    pchisq(lr_stat, df = df_diff, lower.tail = FALSE)
  } else {
    NA_real_
  }

  # Create result
  result <- list(
    statistic = c(LRT = lr_stat),
    parameter = c(df = df_diff),
    p.value = p_value,
    method = "Likelihood Ratio Test for Nested GKw Models",
    data.name = paste(object_name, "vs", object2_name)
  )

  class(result) <- "htest"

  return(result)
}
