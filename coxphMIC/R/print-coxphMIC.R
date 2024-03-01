#' The Generic \code{print} Function for Object of \code{coxphMIC} Class
#'
#' @name print.coxphMIC
#' @param x an object of \code{coxphMIC} class.
#' @param digits	the minimal number of significant digits. See \code{\link[base]{print.default}}.
#' @param ... further arguments passed to or from other methods.
#' @details
#' The (generic) print method for an \code{coxphMIC} object. The results include info on the estimated gamma and beta.
#' Depending on the options, significance testing and confidence intervals are also provided.
#' @return The table of estimated regression coefficients beta and the reparameterized gamma.
#' @references
#'\itemize{
#' \item Abdolyousefi, R. N. and Su, X. (2016). \bold{coxphMIC}: An R package for sparse estimation of Cox PH Models via approximated information criterion. Tentatively accepted, \emph{The R Journal}.
#' \item Su, X. (2015). Variable selection via subtle uprooting.
#' \emph{Journal of Computational and Graphical Statistics}, \bold{24}(4): 1092--1113.
#' URL \url{http://www.tandfonline.com/doi/pdf/10.1080/10618600.2014.955176}
#' \item Su, X., Wijayasinghe, C. S., Fan, J., and Zhang, Y. (2015). Sparse estimation of Cox proportional
#' hazards models via approximated information criteria. \emph{Biometrics}, \bold{72}(3): 751--759.
#' URL \url{http://onlinelibrary.wiley.com/doi/10.1111/biom.12484/epdf}
#' }
#' @seealso \code{\link[coxphMIC]{coxphMIC}}
#' @export

print.coxphMIC <- function(x, digits = max(3L, getOption("digits") - 3L), ...)
{
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"),
                "\n\n", sep = "")
  if (NROW(x$result)>=1){
    cat("Table of Estimated Coefficients via MIC:\n\n")
    print(x$result, digits=digits, ...)
  }
  else cat("No coefficients. Wasn't MIC successfully run? \n")
  cat("\n")
  invisible(x)
}
