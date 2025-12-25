is.qris <- function(x) inherits(x, "qris")

#' @exportS3Method coef qris
#' @importFrom stats model.matrix na.omit printCoefmat
coef.qris <- function(object, ...) {
  coef <- as.numeric(object$coefficient)
  names(coef) <- object$varNames
  coef
}

#' @exportS3Method vcov qris
vcov.qris <- function(object, ...) {
  vcov <- object$vcov
  colnames(vcov) <- rownames(vcov) <- object$varNames
  vcov
}

#' @exportS3Method print qris
print.qris <- function(x, digits = max(1L, getOption("digits") - 3L), ...) {
  ## cat("Call: \n")
  ## dput(x$call)
  ## mat <- rbind(x$varNames, format(x$coefficient, digits = 5))
  ## prmatrix(mat, rowlab = rep("", nrow(mat)),
  ##          collab = rep("", ncol(mat)), quote = FALSE)
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
  cat("Coefficients:\n")
  print.default(format(coef(x), digits = digits), print.gap = 2L, quote = FALSE)
  cat("\n")
  invisible(x) 
}

#' @exportS3Method summary qris
summary.qris <- function(object, ...) {
  if (!is.qris(object))
    stop("Must be qris class")
  ans <- object["call"]
  est.qris <- object$coefficient
  if (is.null(object$stderr)) se.qris <- rep(NaN, length(est.qris))
  else se.qris <- object$stderr
  z.qris <- as.numeric(est.qris) / as.numeric(se.qris)
  TAB <- data.frame(estimate = round(drop(est.qris), 4),
                    std.Error = round(drop(se.qris), 4),
                    z.value = round(z.qris, 3),
                    p.value = round(2 * pnorm(-abs(z.qris)), 4))
  rownames(TAB) <- object$varNames
  out <- list(call = object$call, coefficients = TAB)
  class(out) <- "summary.qris"
  out
}


#' @exportS3Method print summary.qris
print.summary.qris <- function(x, ...){
  cat("Call:\n")
  print(x$call)
  cat("\n")
  cat("qris Estimator")
  cat("\n")
  printCoefmat(as.matrix(x$coefficients), P.values = TRUE, has.Pvalue = TRUE)
}

#' @exportS3Method confint qris
#' @importFrom stats qnorm
confint.qris <- function(object, parm, level = 0.95, ...) {
  cf <- coef(object)
  pnames <- names(cf)
  if (missing(parm)) 
    parm <- pnames
  else if (is.numeric(parm)) 
    parm <- pnames[parm]
  p <- (1 - level) / 2
  p <- c(p, 1 - p)
  prange <- qnorm(p)
  pct <- paste(format(100 * p, trim = TRUE, scientific = FALSE, digits = 3),"%")
  ci <- array(NA_real_, dim = c(length(parm), 2L), dimnames = list(parm, pct))
  if (!is.null(object$ci)) { ## intercept model
    cat("Confidence Interval for the ", round(object$para$Q, 2), " quantile residual time:\n")
    ci[1,] <- object$ci
    return(ci)
  } ## else
  ses <- object$stderr
  parm <- match(parm, pnames)
  ci[] <- cf[parm] + ses[parm] %o% prange
  ci
}

