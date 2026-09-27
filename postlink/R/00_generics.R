
#' @title Unsupported Generics for postlink Models
#' @description Intercepts standard likelihood and residual-based generics that
#' are not directly applicable to post-linkage models because the true match
#' status remains latent.
#' @param object A fitted model object.
#' @param fitted A fitted model object.
#' @param fit A fitted model object.
#' @param model A fitted model object.
#' @param scale Optional numeric specifying the scale parameter.
#' @param k Numeric specifying the "weight" of the equivalent degrees of freedom.
#' @param ... Additional arguments ignored by these methods.
#' @name unsupported_generics
NULL

.plda_generic_error <- function() {
 stop("This metric is not directly applicable for post-linkage adjusted models because the true match status remains latent.", call. = FALSE)
}

#
# Applies to all plmodel objects
#

#' @rdname unsupported_generics
#' @export
logLik.plmodel <- function(object, ...) .plda_generic_error()

#' @rdname unsupported_generics
#' @export
profile.plmodel <- function(fitted, ...) .plda_generic_error()

#' @rdname unsupported_generics
#' @export
anova.plmodel <- function(object, ...) .plda_generic_error()

#' @rdname unsupported_generics
#' @export
extractAIC.plmodel <- function(fit, scale, k = 2, ...) .plda_generic_error()

#
# GLM-Specific Residual Intercepts (Applies only to plglm objects)
#

#' @rdname unsupported_generics
#' @export
cooks.distance.plglm <- function(model, ...) .plda_generic_error()

#' @rdname unsupported_generics
#' @export
rstudent.plglm <- function(model, ...) .plda_generic_error()

#
# Base Methods for Adjustment Objects
#

#' Print Base Method for Adjustment Objects
#' @param x An object of class "adjustment".
#' @param ... Additional arguments.
#' @export
print.adjustment <- function(x, ...) {
 has_data <- FALSE
 n_obs <- 0

 if (!is.null(x$data_ref) && is.environment(x$data_ref)) {
  if (exists("data", envir = x$data_ref, inherits = FALSE)) {
   stored_data <- x$data_ref$data
   if (!is.null(stored_data) && is.data.frame(stored_data)) {
    has_data <- TRUE
    n_obs <- nrow(stored_data)
   }
  }
 }

 cat("\n* Linked Data:")
 if (has_data) {
  cat("\n    Observations:  ", format(n_obs, big.mark = ","))
 }

 invisible(x)
}

#
# Standard Generics for lmtest::coeftest Compatibility
#

#' Extract Coefficients for glmELE
#' @param object A fitted glmELE object.
#' @param weight.matrix Character specifying which weighting method to return.
#' @param ... Additional arguments.
#' @export
coef.glmELE <- function(object, weight.matrix = NULL, ...) {
 if (is.null(weight.matrix)) {
  weight.matrix <- rownames(object$coefficients)[1]
 }
 return(object$coefficients[weight.matrix, ])
}

#' Extract Coefficients for glmMixture
#' @param object A fitted glmMixture object.
#' @param ... Additional arguments.
#' @export
coef.glmMixture <- function(object, ...) {
 return(object$coefficients)
}

#' Extract Coefficients for glmMixBayes
#' @param object A fitted glmMixBayes object.
#' @param ... Additional arguments.
#' @export
coef.glmMixBayes <- function(object, ...) {
 return(colMeans(object$estimates$coefficients))
}

#' Extract Coefficients for survMixBayes
#' @param object A fitted survMixBayes object.
#' @param ... Additional arguments.
#' @export
coef.survMixBayes <- function(object, ...) {
 return(colMeans(object$estimates$coefficients))
}

#' Extract Residual Degrees of Freedom for plglm Models
#' @param object A fitted plglm object.
#' @param ... Additional arguments.
#' @export
df.residual.plglm <- function(object, ...) {
 return(object$df.residual)
}
