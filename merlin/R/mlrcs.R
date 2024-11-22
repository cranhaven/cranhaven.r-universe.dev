#' Fit multi-level restricted cubic spline model
#'
#' @param formula A model formula for the fixed-effects in the model. Must include one restricted cubic spline term, specified as \code{rcs(variable, df = #)}.
#' @param random A formula for the random-effects in the model. Random-effects should be specified as a one-sided formula, e.g. \code{~ 1 + trt | id} for random effect on the intercept and treatment at the \code{id} level. Random-effects can be estimated at any number of nested random-effect levels by providing a list of one-sided formulas. When specifying random-effect at multiple levels. The one-sided formula should be given in order, starting with the highest level.
#' Only required when \code{rcs = TRUE}.
#' @param data A data frame containing all variables required for fitting the model.
#' @param ... Further arguments passed to \link{merlin}.
#'
#' @return An object of class \code{mlrcs}.
#' @export
#'
#' @examples
#' \dontrun{
#' # Two-level model
#' data("pbc.merlin", package = "merlin")
#' fit <- mlrcs(formula = logp ~ 1 + rcs(year, df = 4) + age + trt,
#'              random  = ~ 1 + trt | id,
#'              data = pbc.merlin
#' )
#' summary(fit)
#'
#' # Three-level model
#' fit <- mlrcs(formula = logp ~ 1 + rcs(year, df = 4) + age + trt,
#'              random  = list(~ 1 | region,
#'                             ~ 1 + trt | id),
#'              data = pbc.merlin
#' )
#' summary(fit)
#' }
#' 
#'
mlrcs <- function(formula, random = NULL, data, ...) {
### Check formula

  # get fixed part of formula 
  # if no rcs term
  if (grepl("rcs", toString(formula)) == F) {
    stop("No rcs() term specified", call. = FALSE)
  }
  
  formula_e    <- formula   
  formula_e[3] <- gsub("1", "", formula_e[3]) 
  formula_e[3] <- sub("\\+", "", formula_e[3])  
  formula_f    <- paste0(formula_e[2], " ~", formula_e[3])
  formula      <- formula_f
  
  # get timevar
  timevar_e <- unlist(strsplit(formula_f, split="rcs(", fixed = TRUE))[2]
  timevar_e <- unlist(strsplit(timevar_e, split=",", fixed = TRUE))[1]

  # get random part of formula
  if (is.null(random) == F) {
    
    #random = ~ 1 | id
    if (is.list(random) == F) random = list(random)
    Nrandom <- length(random)  
    count <- 1 # count for naming random-effect M#
    formula_r <- ""
    levels <- c()
    for (j in 1:Nrandom) {
      random_s <-  random[[j]]
      random_s <- toString(random_s)
      random_e <- unlist(strsplit(random_s, split=",", fixed = TRUE))[2]
      random_e <- unlist(strsplit(random_e, split="|", fixed = TRUE))[1]
      level_e  <- gsub(" ", "", unlist(strsplit(random_s, split="|", fixed = TRUE))[2])
      levels <- c(levels, level_e)
      
      random_e <- unlist(strsplit(toString(random_e), split="+", fixed = TRUE))
      random_e <- gsub(" ", "", random_e)
  
      for (i in 1:length(random_e)) {
        if (count > 1) v <- paste0(formula_r, " + ")
        if (random_e[i] == "1") {
          formula_r <- paste0( formula_r, "+", "M", count, "[", level_e, "] * 1")
        } else {
          formula_r <- paste0(formula_r, "+", random_e[i], ":M", count, "[", level_e, "] * 1 ")
        }
        count <- count + 1
      }
    } 
    formula <- paste0(formula, formula_r)
  } 
  
  # Full model specification
  formula <- stats::as.formula(formula)

  ## Fit model using merlin
  if (is.null(random) == F) {
    fit <- merlin(model = formula, family = "gaussian", data = data, levels = levels, timevar = timevar_e, ...) 
  } else {
    fit <- merlin(model = formula, family = "gaussian", data = data, timevar = timevar_e, ...) 
  }

  ### Return mlrcs object
  class(fit) <- "mlrcs"
  return(fit)
}

#' @title Print \code{mlrcs} Fits
#' @description Print the coefficients from a \code{mlrsc} fit.
#'
#' @param x An object of class \code{mlrcs}.
#' @param digits The number of significant digits to use when printing.
#' @param simplify Should non-interpretable coefficients be hidden (e.g. splines and flexible polynomials terms)? Defaults to TRUE.
#' @param ... Not used.
#'
#' @export
print.mlrcs <- function(x, digits = max(3L, getOption("digits") - 3L), simplify = TRUE, ...) {
  cat("Restricted cubic splines model\n")
  cat("Data:", x$data, "\n\n")
  cat("Coefficients:\n")
  # if (simplify) {
  #   if (x$distribution %in% c("rp", "loghazard", "logchazard")) x$coefficients <- x$coefficients[!grepl("^_cons|^rcs()|^fp()", names(x$coefficients))]
  # }
  print.default(format(stats::coef(x), digits = digits), print.gap = 2L, quote = FALSE)
  invisible(x)
}

#' @title Summarizing \code{mlrcs} Fits
#' @description These functions are all methods for class \code{mlrcs} or \code{summary.mlrcs} objects.
#' @param object An object of class \code{mlrcs}
#' @param sig Significancy level for confidence intervals. Defaults to 0.95.
#' @param ... Not used.
#'
#' @export
summary.mlrcs <- function(object, sig = 0.95, ...) {
  out <- summary.merlin(object = object, sig = sig, ...)
  class(out) <- c("summary.mlrcs", class(out))
  return(out)
}

#' @param x An object of class \code{summary.mlrcs}
#' @param digits The number of significant digits to use when printing.
#' @rdname summary.mlrcs
#' @export
print.summary.mlrcs <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat("Restricted cubic splines model\n")
  cat("Log likelihood =", stats::logLik(x))
  cat("\n\n")
  
  stats::printCoefmat(x$coefftable, digits = digits, na.print = "NA", cs.ind = c(1, 2, 5, 6), tst.ind = 3, zap.ind = 4)
  
  if (x$convergence != 0) warning("Model did not converge.", call. = FALSE)
}

#' @title Extract Model Coefficients
#' @description \code{coef} extracts model coefficients from a \code{mlrcs} model fit. \code{coefficients} is an alias for it.
#' @param object An object of class \code{mlrcs} or \code{summary.mlrcs}.
#' @param ... Not used.
#' @export
coef.mlrcs <- function(object, ...) object$coefficients

#' @rdname coef.mlrcs
#' @export
coef.summary.mlrcs <- function(object, ...) coef.merlin(object, ...)

#' @title Calculate Variance-Covariance Matrix for a \code{mlrcs} Model Object
#' @description Returns the variance-covariance matrix of all estimated parameters of a fitted \code{mlrcs} model.
#' @param object An object of class \code{mlrcs} or \code{summary.mlrcs}.
#' @param ... Not used.
#' @export
vcov.mlrcs <- function(object, ...) solve(object$hessian)

#' @rdname vcov.mlrcs
#' @export
vcov.summary.mlrcs <- function(object, ...) vcov.merlin(object$hessian)

#' @title Extract Log-Likelihood
#' @description Extract log-likelihood of a \code{mlrcs} model.
#' @param object An object of class \code{mlrcs} or \code{summary.mlrcs}.
#' @param ... Not used.
#' @export
logLik.mlrcs <- function(object, ...) object$loglikelihood

#' @rdname logLik.mlrcs
#' @export
logLik.summary.mlrcs <- function(object, ...) logLik.merlin(object, ...)
