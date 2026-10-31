#' Elementwise standard error.
#'
#' Calculates the elementwise standard error for the object returned from \code{TRR.fit}. The standard error for the object returned from \code{TPR.fit} is unavailable.
#'
#' @param object an object of class \code{"Tenv"}, as the ones returned from \code{TRR.fit}.
#' @return The standard error tensor is returned.
#' @note The function only supports the object returned from \code{TRR.fit} since there is no standard error for the object returned from \code{TPR.fit}.
#'
#' @examples
#' data("bat")
#' x <- bat$x
#' y <- bat$y
#' fit <- TRR.fit(x, y, method="standard")
#' std_err(fit)
#' @export
#'
std_err <- function(object){
  cl <- object$call
  if(cl[1] == "TPR.fit()"){stop("there is no standard error for the object returned from TPR.fit().")}
  if(cl[1] == "TRR.fit()"){
    tmp <- Tenv_Pval(object$x, object$y, Bhat = coef(object))
    cat("\nThe standard error tensor:\n\n")
    tmp$se
  }
}
