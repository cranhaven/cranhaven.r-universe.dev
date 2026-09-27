#' Methods for Bayesian Mixture GLM Fits
#'
#' @description
#' S3 methods for objects returned by \code{glmMixBayes()}, including printing,
#' summarizing fitted models, computing credible intervals and posterior
#' covariance matrices, generating predictions, and pooling regression results
#' across posterior match classifications.
#'
#' @name mixture_bayesglm_methods
#' @keywords internal
NULL

#' Pool Parameter Estimates Across Posterior Draws
#'
#' Generic function for pooling parameter estimates from Bayesian mixture
#' models using posterior draws of the latent component indicators.
#'
#' @param object A fitted Bayesian mixture model object.
#' @param ... Additional arguments passed to methods.
#'
#' @return A pooled model object combining parameter estimates across
#' posterior component-indicator draws.
#'
#' @examples
#' \donttest{
#' # mi_with() is a generic function for posterior allocation–based pooling.
#' # See ?mi_with.glmMixBayes for a complete example illustrating its use
#' # with Bayesian GLM mixture models.
#' }
#'
#' @export
mi_with <- function(object, ...) {
 UseMethod("mi_with", object)
}

#' Print a glmMixBayes Model Object
#'
#' @param x An object of class \code{glmMixBayes}.
#' @param digits Minimum number of significant digits to show.
#' @param ... Further arguments (unused).
#' @return The input \code{x}, invisibly.
#'
#' @examples
#' \donttest{
#' data(lifem)
#'
#' # lifem data preprocessing
#' # For computational efficiency in the example, we work with a subset of the lifem data.
#' lifem <- lifem[order(-(lifem$commf + lifem$comml)), ]
#' lifem_small <- rbind(
#'   head(subset(lifem, hndlnk == 1), 100),
#'   head(subset(lifem, hndlnk == 0), 20)
#' )
#'
#' x <- cbind(1, poly(lifem_small$unit_yob, 3, raw = TRUE))
#' y <- lifem_small$age_at_death
#'
#' adj <- adjMixBayes(
#'   linked.data = lifem_small,
#'   priors = list(theta = "beta(2, 2)")
#' )
#'
#' fit <- plglm(
#'   age_at_death ~ poly(unit_yob, 3, raw = TRUE),
#'   family = "gaussian",
#'   adjustment = adj,
#'   control = list(
#'     iterations = 200,
#'     burnin.iterations = 100,
#'     seed = 123
#'   )
#' )
#'
#' print(fit)
#' }
#'
#' @export
#' @method print glmMixBayes
print.glmMixBayes <- function(x, digits = max(3L, getOption("digits") - 3L),...){
  cat("Call:\n")
  print(x$call, quote = F, digits = digits)
  cat("\n")

  cat("Coefficients:", sep="\n")
  vals <- apply(x$estimates$coefficients, 2, mean)
  print(format(signif(vals, digits)), print.gap = 2, quote = F)
  cat("\n")

  invisible(x)
}

#' Summary Method for glmMixBayes Models
#'
#' @param object An object of class \code{glmMixBayes}.
#' @param ... Not used.
#' @return An object of class \code{"summary.glmMixBayes"}, which is printed with a custom method.
#'
#' @examples
#' \donttest{
#' data(lifem)
#'
#' # lifem data preprocessing
#' # For computational efficiency in the example, we work with a subset of the lifem data.
#' lifem <- lifem[order(-(lifem$commf + lifem$comml)), ]
#' lifem_small <- rbind(
#'   head(subset(lifem, hndlnk == 1), 100),
#'   head(subset(lifem, hndlnk == 0), 20)
#' )
#'
#' x <- cbind(1, poly(lifem_small$unit_yob, 3, raw = TRUE))
#' y <- lifem_small$age_at_death
#'
#' adj <- adjMixBayes(
#'   linked.data = lifem_small,
#'   priors = list(theta = "beta(2, 2)")
#' )
#'
#' fit <- plglm(
#'   age_at_death ~ poly(unit_yob, 3, raw = TRUE),
#'   family = "gaussian",
#'   adjustment = adj,
#'   control = list(
#'     iterations = 200,
#'     burnin.iterations = 100,
#'     seed = 123
#'   )
#' )
#'
#' summary(fit)
#' }
#'
#' @export
summary.glmMixBayes <- function(object, ...) {

 ci_beta1 <- t(apply(object$estimates$coefficients, 2,
                     function(x) stats::quantile(x, probs = c(0.025, 0.975))))
 ci_beta2 <- t(apply(object$estimates$m.coefficients, 2,
                     function(x) stats::quantile(x, probs = c(0.025, 0.975))))

 TAB1 <- cbind(
  Estimates   = apply(object$estimates$coefficients, 2, mean),
  `Std. Error`= apply(object$estimates$coefficients, 2, stats::sd),
  `2.5 %`     = ci_beta1[, 1],
  `97.5 %`    = ci_beta1[, 2]
 )

 TAB2 <- cbind(
  Estimates   = apply(object$estimates$m.coefficients, 2, mean),
  `Std. Error`= apply(object$estimates$m.coefficients, 2, stats::sd),
  `2.5 %`     = ci_beta2[, 1],
  `97.5 %`    = ci_beta2[, 2]
 )

 if (object$family %in% c("gaussian", "gamma")) {
  TAB3 <- cbind(
   Estimate    = mean(object$estimates$dispersion),
   `Std. Error`= stats::sd(object$estimates$dispersion)
  )
  TAB4 <- cbind(
   Estimate    = mean(object$estimates$m.dispersion),
   `Std. Error`= stats::sd(object$estimates$m.dispersion)
  )
  rownames(TAB3) <- ""
  rownames(TAB4) <- ""
 }

 out <- list(
  call          = object$call,
  family        = object$family,
  coefficients  = TAB1,
  m.coefficients= TAB2
 )

 if (object$family %in% c("gaussian", "gamma")) {
  out$dispersion    <- TAB3
  out$m.dispersion  <- TAB4
 }

 class(out) <- "summary.glmMixBayes"
 out
}

#' @noRd
#' @export
print.summary.glmMixBayes <- function(x, digits = max(3L, getOption("digits") - 3L),
                                      signif.stars = getOption("show.signif.stars"),...){
  cat("Call:", sep="\n")
  print(x$call,quote=F)
  cat(" ", sep="\n")
  cat("Family:", x$family, " ", sep="\n")

  print_mat <- function(mat) {
   print(format(round(mat, digits = digits), nsmall = digits),
         quote = FALSE, right = TRUE)
  }

  cat("(Component 1 = Correct-match):", sep = "\n")

  cat("Outcome Model Coefficients:", sep="\n")
  print_mat(x$coefficients)
  cat("\n")

  if (x$family %in% c("gamma", "gaussian")){
   cat("Dispersion:", sep="\n")
   print_mat(x$dispersion)
    cat("\n")
  }

  cat("(Component 2 = Incorrect-match):", sep = "\n")

  cat("Outcome Model Coefficients:", sep="\n")
  print_mat(x$m.coefficients)
  cat("\n")

  if (x$family %in% c("gamma", "gaussian")){
    cat("Dispersion:", sep="\n")
   print_mat(x$m.dispersion)
   cat("\n")
  }

  invisible(x)
}

#' Posterior Covariance Matrix for glmMixBayes Coefficients
#'
#' @param object A \code{glmMixBayes} model object.
#' @param ... Not used.
#' @return Posterior covariance matrix of the regression coefficients for
#' component 1 (the correct-match component).
#'
#' @examples
#' \donttest{
#' data(lifem)
#'
#' # lifem data preprocessing
#' # For computational efficiency in the example, we work with a subset of the lifem data.
#' lifem <- lifem[order(-(lifem$commf + lifem$comml)), ]
#' lifem_small <- rbind(
#'   head(subset(lifem, hndlnk == 1), 100),
#'   head(subset(lifem, hndlnk == 0), 20)
#' )
#'
#' x <- cbind(1, poly(lifem_small$unit_yob, 3, raw = TRUE))
#' y <- lifem_small$age_at_death
#'
#' adj <- adjMixBayes(
#'   linked.data = lifem_small,
#'   priors = list(theta = "beta(2, 2)")
#' )
#'
#' fit <- plglm(
#'   age_at_death ~ poly(unit_yob, 3, raw = TRUE),
#'   family = "gaussian",
#'   adjustment = adj,
#'   control = list(
#'     iterations = 200,
#'     burnin.iterations = 100,
#'     seed = 123
#'   )
#' )
#'
#' vcov(fit)
#' }
#'
#' @export
vcov.glmMixBayes <- function(object, ...) {
 stats::cov(object$estimates$coefficients)
}

#' Credible Intervals for Regression Coefficients from a glmMixBayes Fit
#'
#' Computes posterior credible intervals for the regression coefficients in a
#' fitted \code{glmMixBayes} model. By default, intervals are returned for all
#' coefficients in component 1 of the mixture model. A subset of
#' coefficients can be selected using \code{parm}.
#'
#' @param object A \code{glmMixBayes} model object.
#' @param parm Optional. Names or numeric indices of coefficients for which
#'   credible intervals should be returned. If \code{NULL}, intervals are
#'   returned for all coefficients.
#' @param level Probability level for the credible intervals. Defaults to
#'   \code{0.95}.
#' @param ... Not used.
#' @return
#' A matrix with one row per coefficient and two columns giving the lower and
#' upper credible interval bounds. Row names correspond to coefficient names.
#'
#' @examples
#' \donttest{
#' data(lifem)
#'
#' # lifem data preprocessing
#' # For computational efficiency in the example, we work with a subset of the lifem data.
#' lifem <- lifem[order(-(lifem$commf + lifem$comml)), ]
#' lifem_small <- rbind(
#'   head(subset(lifem, hndlnk == 1), 100),
#'   head(subset(lifem, hndlnk == 0), 20)
#' )
#'
#' x <- cbind(1, poly(lifem_small$unit_yob, 3, raw = TRUE))
#' y <- lifem_small$age_at_death
#'
#' adj <- adjMixBayes(
#'   linked.data = lifem_small,
#'   priors = list(theta = "beta(2, 2)")
#' )
#'
#' fit <- plglm(
#'   age_at_death ~ poly(unit_yob, 3, raw = TRUE),
#'   family = "gaussian",
#'   adjustment = adj,
#'   control = list(
#'     iterations = 200,
#'     burnin.iterations = 100,
#'     seed = 123
#'   )
#' )
#'
#' confint(fit)
#' }
#'
#' @export
#' @method confint glmMixBayes
confint.glmMixBayes <- function(object, parm = NULL, level = 0.95, ...) {
 alpha <- 1 - level
 vals <- t(apply(object$estimates$coefficients, 2,
                 function(x) stats::quantile(x, probs = c(alpha/2, 1 - alpha/2), na.rm = TRUE)))
 if (!is.null(parm)) {
  vals <- vals[parm, , drop = FALSE]
 }
 vals
}


#' Predictions from a glmMixBayes Model
#'
#' @param object A \code{glmMixBayes} model object.
#' @param newx A numeric matrix of new observations (n_new x K) with columns aligned
#'   to the design matrix \code{X} used for fitting.
#' @param type Either \code{"link"} or \code{"response"}, indicating the scale of predictions.
#' @param se.fit Logical; if \code{TRUE}, also return posterior SD of predictions.
#' @param interval Either \code{"none"} or \code{"credible"}, indicating whether to compute a credible interval.
#' @param level Probability level for the credible interval (default 0.95).
#' @param ... Not used.
#'
#' @return If \code{se.fit = FALSE} and \code{interval = "none"}, a numeric vector of predicted values.
#'   Otherwise, a matrix with columns for the fit, (optional) \code{se.fit}, and (optional)
#'   credible interval bounds.
#'
#' @examples
#' \donttest{
#' data(lifem)
#'
#' # lifem data preprocessing
#' # For computational efficiency in the example, we work with a subset of the lifem data.
#' lifem <- lifem[order(-(lifem$commf + lifem$comml)), ]
#' lifem_small <- rbind(
#'   head(subset(lifem, hndlnk == 1), 100),
#'   head(subset(lifem, hndlnk == 0), 20)
#' )
#'
#' x <- cbind(1, poly(lifem_small$unit_yob, 3, raw = TRUE))
#' y <- lifem_small$age_at_death
#'
#' adj <- adjMixBayes(
#'   linked.data = lifem_small,
#'   priors = list(theta = "beta(2, 2)")
#' )
#'
#' fit <- plglm(
#'   age_at_death ~ poly(unit_yob, 3, raw = TRUE),
#'   family = "gaussian",
#'   adjustment = adj,
#'   control = list(
#'     iterations = 200,
#'     burnin.iterations = 100,
#'     seed = 123
#'   )
#' )
#'
#' newx <- cbind(1, poly(c(0.2, 0.5, 0.8), 3, raw = TRUE))
#' predict(fit, newx = newx, type = "response")
#' }
#'
#' @export
predict.glmMixBayes <- function(object, newx,
                                type = c("link", "response"),
                                se.fit = FALSE,
                                interval = c("none", "credible"),
                                level = 0.95,
                                ...) {
  type <- match.arg(type)
  interval <- match.arg(interval)

  if (!is.matrix(newx) || !is.numeric(newx)) {
    stop("`newx` must be a numeric matrix with columns aligned to the fitted design matrix.")
  }

  mean_coef <- apply(object$estimates$coefficients, 2, mean)

  # point predictions
  if (type == "link") {
    predictions <- as.vector(newx %*% mean_coef)
  } else {
    eta <- as.vector(newx %*% mean_coef)
    predictions <- switch(object$family,
      gaussian = eta,
      gamma    = exp(eta),
      poisson  = exp(eta),
      binomial = stats::plogis(eta),
      stop("Unknown family in object.")
    )
  }

  if (!se.fit && interval == "none") {
    return(predictions)
  }

  # posterior predictive draws: rows are new data points, columns are draws
  if (type == "link") {
    all_predictions <- newx %*% t(object$estimates$coefficients)
  } else {
    eta_all <- newx %*% t(object$estimates$coefficients)
    all_predictions <- switch(object$family,
      gaussian = eta_all,
      gamma    = exp(eta_all),
      poisson  = exp(eta_all),
      binomial = stats::plogis(eta_all),
      stop("Unknown family in object.")
    )
  }

  se.predictions <- apply(all_predictions, 1, stats::sd)

  if (interval == "none") {
    vals <- cbind(fit = predictions, se.fit = se.predictions)
    colnames(vals) <- c("fit", "se.fit")
    return(vals)
  }

  alpha <- 1 - level
  ci <- t(apply(all_predictions, 1, stats::quantile, probs = c(alpha / 2, 1 - alpha / 2)))
  lower <- ci[, 1]
  upper <- ci[, 2]

  if (!se.fit && interval == "credible") {
    vals <- cbind(fit = predictions, lower = lower, upper = upper)
    colnames(vals) <- c("fit", paste0(alpha / 2 * 100, " %"), paste0((1 - alpha / 2) * 100, " %"))
    return(vals)
  }

  vals <- cbind(fit = predictions, se.fit = se.predictions, lower = lower, upper = upper)
  colnames(vals) <- c("fit", "se.fit", paste0(alpha / 2 * 100, " %"), paste0((1 - alpha / 2) * 100, " %"))
  vals
}


#' Pooling Regression Fits Across Posterior Draws of Correct-Match Classifications
#'
#' @description
#' Use posterior draws of the latent match indicators from \code{glmMixBayes()}
#' to repeatedly identify which records are treated as correct matches, refit the
#' requested regression model on those records, and pool the resulting estimates.
#'
#' Each retained posterior draw defines one subset of records classified as
#' correct matches. The function fits the specified \code{lm()} or \code{glm()}
#' model to that subset, extracts the estimated coefficients and their covariance
#' matrix, and combines the results across draws using multiple-imputation
#' pooling rules.
#'
#' @param object A \code{glmMixBayes} model object containing posterior draws of
#'   the latent match indicators.
#' @param data A data.frame with all candidate records in the same row order as used in the model.
#' @param formula Model formula for refitting on each draw (required).
#' @param family A \code{stats::family()} object for the refitted model. If not
#'   supplied, the function chooses a default family based on \code{object$family}.
#' @param min_n Minimum number of records required to fit the model for a given
#'   posterior draw. The default is \code{p + 1}, where \code{p} is the number
#'   of columns in the model matrix.
#' @param quietly If \code{TRUE}, draws that lead to fitting errors are skipped
#'   without printing the full error message.
#' @param ... Additional arguments passed through (currently unused).
#' @return An object of class \code{c("mi_link_pool_glm", "mi_link_pool")}
#'   containing pooled coefficient estimates, standard errors, confidence
#'   intervals, and related summary information.
#'
#' @examples
#' \donttest{
#' data(lifem)
#'
#' # lifem data preprocessing
#' # For computational efficiency in the example, we work with a subset of the lifem data.
#' lifem <- lifem[order(-(lifem$commf + lifem$comml)), ]
#' lifem_small <- rbind(
#'   head(subset(lifem, hndlnk == 1), 100),
#'   head(subset(lifem, hndlnk == 0), 20)
#' )
#'
#' x <- cbind(1, poly(lifem_small$unit_yob, 3, raw = TRUE))
#' y <- lifem_small$age_at_death
#'
#' adj <- adjMixBayes(
#'   linked.data = lifem_small,
#'   priors = list(theta = "beta(2, 2)")
#' )
#'
#' fit <- plglm(
#'   age_at_death ~ poly(unit_yob, 3, raw = TRUE),
#'   family = "gaussian",
#'   adjustment = adj,
#'   control = list(
#'     iterations = 200,
#'     burnin.iterations = 100,
#'     seed = 123
#'   )
#' )
#'
#' pooled_fit <- mi_with(
#'   object = fit,
#'   data = lifem_small,
#'   formula = age_at_death ~ poly(unit_yob, 3, raw = TRUE),
#'   family = gaussian()
#' )
#'
#' print(pooled_fit)
#' }
#'
#' @export
#' @importFrom stats coef vcov cov lm glm qt model.frame model.matrix gaussian poisson binomial Gamma
mi_with.glmMixBayes <- function(object, data, formula,
                            family = NULL, min_n = NULL, quietly = TRUE, ...) {

  # Resolve formula: allow inference from object$call$formula if not provided
  if (missing(formula) || is.null(formula)) {
   if (!is.null(object$call$formula)) {
    # Use parent.frame() so symbols referenced in the original call can be found
    formula <- eval(object$call$formula, envir = parent.frame())
   } else {
    stop("Formula not found: please provide it explicitly or ensure object$call$formula exists.",
         call. = FALSE)
   }
  }

  # Ensure we ended up with a valid formula
  if (!inherits(formula, "formula")) {
   stop("`formula` must be a valid formula object.", call. = FALSE)
  }

  if (is.null(family)) {
    family <- switch(object$family,
      gaussian = stats::gaussian(),
      poisson  = stats::poisson(),
      binomial = stats::binomial(),
      gamma    = stats::Gamma(),
      stats::gaussian()
    )
  }

  # Extract the matrix of component allocations (S x N)
  z_samples <- object$m_samples

  collapse_z_g <- function(z_samples, g) {
    if (is.list(z_samples)) {
      parts <- lapply(z_samples, function(Z) {
        if (is.null(dim(Z))) {
          stop("Each list element of `z_samples` must be a matrix-like object.")
        }
        Z[g, ]
      })
      as.integer(do.call(c, parts))
    } else {
      if (is.null(dim(z_samples)) || length(dim(z_samples)) != 2L) {
        stop("`z_samples` must be an S x N matrix, or a list of such matrices.")
      }
      as.integer(z_samples[g, ])
    }
  }

  fit_once <- function(df) {
    if (inherits(family, "family") && identical(family$family, "gaussian")) {
      fit <- stats::lm(formula, data = df)
    } else {
      fit <- stats::glm(formula, data = df, family = family)
    }
    list(coef = stats::coef(fit), vcov = stats::vcov(fit))
  }

  # Determine p and default min_n
  mf <- stats::model.frame(formula, data = data)
  Xtmp <- stats::model.matrix(formula, mf)
  p <- ncol(Xtmp)
  if (is.null(min_n)) min_n <- p + 1L

  # Number of posterior draws S
  S <- if (is.list(z_samples)) nrow(z_samples[[1]]) else nrow(z_samples)
  if (length(S) == 0L || is.null(S) || !is.finite(S)) {
    stop("Unable to infer the number of posterior draws (S).")
  }

  coefs_list <- list()
  vcovs_list <- list()
  kept <- logical(S)

  for (s in seq_len(S)) {
    zs <- collapse_z_g(z_samples, s)
    if (!all(zs %in% c(1L, 2L))) {
      stop("`m_samples` must contain only 1/2 indicators.")
    }

    if (sum(zs == 1L) < min_n) next
    dat_s <- data[which(zs == 1L), , drop = FALSE]

    res <- try(fit_once(dat_s), silent = quietly)
    if (!inherits(res, "try-error")) {
      coefs_list[[length(coefs_list) + 1L]] <- res$coef
      vcovs_list[[length(vcovs_list) + 1L]] <- res$vcov
      kept[s] <- TRUE
    }
  }

  m <- length(coefs_list)
  if (m == 0L) {
    stop("No valid imputations: all draws failed or had too few component-1 records.")
  }

  parnames <- names(coefs_list[[1L]])
  coefs_mat <- do.call(rbind, lapply(coefs_list, function(b) b[parnames]))

  Ubar <- Reduce("+", vcovs_list) / m
  B <- stats::cov(coefs_mat)
  Tmat <- Ubar + (1 + 1 / m) * B

  qbar <- colMeans(coefs_mat)
  se <- sqrt(diag(Tmat))
  lambda <- diag((1 + 1 / m) * B) / diag(Tmat)

  r <- diag((1 + 1 / m) * B) / diag(Ubar)
  dfold <- (m - 1) * (1 + 1 / r)^2
  df <- pmax(dfold, 3)

  tcrit <- stats::qt(0.975, df = df)
  lwr <- qbar - tcrit * se
  upr <- qbar + tcrit * se
  ci95 <- cbind(lwr = lwr, upr = upr)
  rownames(ci95) <- names(qbar)

  out <- list(
    m          = m,
    coef       = qbar,
    vcov       = Tmat,
    se         = se,
    ci95       = ci95,
    Ubar       = Ubar,
    B          = B,
    lambda     = lambda,
    df         = df,
    kept_draws = which(kept)
  )
  class(out) <- c("mi_link_pool_glm", "mi_link_pool")
  out
}

#' Print Pooled Regression Results
#'
#' @param x An object of class \code{mi_link_pool_glm}, typically returned by
#'   \code{mi_with()} for a \code{glmMixBayes} fit.
#' @param digits the number of significant digits to print.
#' @param ... further arguments (unused).
#' @return The input \code{x}, invisibly.
#'
#' @examples
#' \donttest{
#' data(lifem)
#'
#' # lifem data preprocessing
#' # For computational efficiency in the example, we work with a subset of the lifem data.
#' lifem <- lifem[order(-(lifem$commf + lifem$comml)), ]
#' lifem_small <- rbind(
#'   head(subset(lifem, hndlnk == 1), 100),
#'   head(subset(lifem, hndlnk == 0), 20)
#' )
#'
#' x <- cbind(1, poly(lifem_small$unit_yob, 3, raw = TRUE))
#' y <- lifem_small$age_at_death
#'
#' adj <- adjMixBayes(
#'   linked.data = lifem_small,
#'   priors = list(theta = "beta(2, 2)")
#' )
#'
#' fit <- plglm(
#'   age_at_death ~ poly(unit_yob, 3, raw = TRUE),
#'   family = "gaussian",
#'   adjustment = adj,
#'   control = list(
#'     iterations = 200,
#'     burnin.iterations = 100,
#'     seed = 123
#'   )
#' )
#'
#' pooled_fit <- mi_with(
#'   object = fit,
#'   data = lifem_small,
#'   formula = age_at_death ~ poly(unit_yob, 3, raw = TRUE),
#'   family = gaussian()
#' )
#'
#' print(pooled_fit, digits = 4)
#' }
#'
#' @export
print.mi_link_pool_glm <- function(x, digits = max(3L, getOption("digits") - 2L), ...) {
  cat("Pooled regression results across posterior match classifications:\n")
  cat("  Retained imputations (m):", x$m, "\n\n")

  est <- x$coef
  se  <- x$se
  ci  <- x$ci95
  df  <- x$df
  tab <- cbind(Estimate = est, Std.Error = se,
               CI.lwr = ci[, "lwr"], CI.upr = ci[, "upr"],
               df = df)

  stats::printCoefmat(tab, digits = digits, has.Pvalue = FALSE, ...)
  invisible(x)
}
