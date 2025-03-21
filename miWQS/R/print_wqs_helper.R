#' Prints the fitted WQS model along with the mean weights.
#'
#' @family wqs
#' @keywords wqs
#'
#' @description
#' Prints the fitted WQS model along with the mean weights. Adjusted from print.lm.
#'
#' @param x  An object of class WQS, from \code{\link{estimate.wqs}}.
#' @inheritParams base::print
#' @param ... Further arguments passed from other methods to print(). Currently has no effect.


# #'@inheritSection estimate.wqs examples
#' @examples
#' # See estimate.wqs().
#'
#' # As base package is always available, there is no need to ever import base
#' @export

print.wqs <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  ## Save Objects
  W <- x
  C <- W$C
  N.train <- nrow(W$q.train)
  N.valid <- nrow(W$q.valid)
  N <- ifelse(W$call$proportion.train == 1, N.train, N.train + N.valid)

  ## Summarize by family
  # Save coefficients
  WQS.regression <- summary(W$fit)$coefficients

  # If the family is the default, it is of form c("gaussian", "binomial", "poisson") which is in class call. I would want to extract gaussian.
  if (length(as.character(W$call$family)) > 1) {
    W$call$family <- as.character(W$call$family)[2]
  }

  ## Gaussian Print
  if (W$call$family == "gaussian") {
    cat("Overall Mixture Effect: (N.valid = ", N.valid, ") \n", sep = "")
    se <- WQS.regression[, 2]
    Ninety.Five.CI <- suppressWarnings(
      formatCI(
        cbind(
          mean = coef(W),
          lower.limit = coef(W) - 1.96 * se,
          upper.limit = coef(W) + 1.96 * se
        ),
        digits = 3)
    )
    d <- data.frame(WQS.regression[, 1:2], Ninety.Five.CI = Ninety.Five.CI,
      p.val = format.pval(WQS.regression[, 4], digits = 3,  eps = 0.001))
    print(d, digits = digits, print.gap = 2L, quote = FALSE)
  }

  ## Binomial Print
  if (W$call$family == "binomial") {
    # beta1hat = log(OR\hat) ~asystompically Normal (beta1, logit(p))
    cat("\n", "Odd Ratios & 95% CI (N.valid = ", N.valid, ") \n", sep = "")
    se <- exp(WQS.regression[, 2])
    Ninety.Five.CI <- suppressWarnings(
      formatCI(
        cbind(
          mean = exp(coef(W)),
          lower.limit = exp(coef(W) - 1.96 * WQS.regression[, 2]),
          upper.limit = exp(coef(W) + 1.96 * WQS.regression[, 2])
        ),
        digits = 3)
    )
    d <- data.frame(signif(exp(WQS.regression[, 1:2]), digits = 3), Ninety.Five.CI,
      p.val = format.pval(WQS.regression[, 4], digits = 3,  eps = 0.001))
    dimnames(d)[[2]] <- c("Odds Ratio", "SE.OR", "95% CI", "P-value")
    print(d, digits = digits, print.gap = 2L, quote = FALSE)
  }

  ## Poisson Print
  if (W$call$family == "poisson") {
    # beta1hat = log(RR\hat) ~asystompically Normal (beta1, logit(RR))
    cat("\n", "Relative Risk & 95% CI (N.valid = ", N.valid, ") \n", sep = "")
    # Actually calculated the same as if it was binary. The interpertation is different.
    se <- exp(WQS.regression[, 2])
    Ninety.Five.CI <-  suppressWarnings(
      formatCI(
        cbind(
          mean = exp(coef(W)),
          lower.limit = exp(coef(W) - 1.96 * WQS.regression[, 2]),
          upper.limit = exp(coef(W) + 1.96 * WQS.regression[, 2])
        ),
        digits = 3)
    )
    d <- data.frame(signif(exp(WQS.regression[, 1:2]), digits = 3), Ninety.Five.CI,
      p.val = format.pval(WQS.regression[, 4], digits = 3,  eps = 0.001))
    dimnames(d)[[2]] <- c("Rel.Risk", "SE.RR", "95% CI", "P-value")
    print(d, digits = digits, print.gap = 2L, quote = FALSE)
  }

  ## AIC
  cat("AIC: ", AIC(W$fit), "\n")

  ## Tell about convergence.
  d <- W$train.estimates
  B <- nrow(d)
  cat("\n")
  if (B ==  sum(d$convergence)) {
    cat("All (", B, ") bootstraps have converged. \n", sep = "")
  } else {
    cat(B - sum(d$convergence), " bootstrap(s) have failed to converged. Those are: \n", sep = "")
    print(which(!d$convergence), quote = FALSE)
  }

  ## Mean Weight Summary
  # Save Signal Function. When signal.fn is the default, it is a character vector of strings. Need to select first element.
  signal.fn <- W$call$signal.fn
  if (is(signal.fn, "call")) {signal.fn <- "signal.none"}

  cat ("\n Weights Adjusted by ", signal.fn, " using N.train = ", N.train, " observations: \n", sep = "")
  mean.weights <- W$processed.weights[, 1]
  mean.weights <- sort(mean.weights, decreasing = TRUE)
  f <- format.pval (mean.weights, digits = 4, eps = 0.0001)
  names(f) <- names(mean.weights)
  print(f,  print.gap = 2L,  quote = FALSE)
  cat("Important chemicals defined as mean weights > 1/", W$C, "~", round(1 / W$C, 3), ". \n", sep = "")
  # (No need: Can tell bad actors from weight estimates)
  # cat(paste(row.names(W$processed.weights [W$processed.weights[ , 1]  > 1/W$C, ]),
  #           collapse = ", "), "\n")


  ## Checking Constriants:  Already listed in function call (may keep later, but want to comment out.)
  show <- FALSE
  if (show) {
    cat("\n Check Constraints \n")
    cat("Minimum Beta1 Estimate: ",  min(W$train.estimates$beta_1), "\n")
    cat("Weight Constriants over all bootstraps and weights. \n")
    wts.matrix <- W$train.estimates[, -c(1:6)]
    #  print(summary(wts.matrix))
    w.sum <- apply(wts.matrix, 1, sum)
    print(data.frame (w.min = min(wts.matrix),  w.max =  max(wts.matrix),  min(w.sum), max(w.sum)))
  }

} # end print function





# ???Customary to put names
#  cat("\n")
# cat("WQS object contains: \n")
#  print.default(format(names(W)), quote = TRUE)
# cat("\n")


# Additional Things to Put first
# cat(" N = ", N, "\n")

# Call -- not working really (if covariates are not called, the "Z" is not refered as NULL.)
# cat("\nCall:\n")
# str(W$call)
# cat("\n")

# Coefficients
# cat("Coefficients:\n")
#  print (coef(W), digits = digits, print.gap = 2L, quote = FALSE)

# cat("Validation WQS Model:\n")
#   print (W$fit, digits = digits, print.gap = 2L, quote = FALSE)
#
#
#
#
