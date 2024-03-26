#' @name print.summary.spjctest
#' @rdname print.summary.spjctest
#'
#' @title Print method for objects of class summary.spjctest.
#'
#' @param x object of class \emph{summary.spjctest}.
#' @param digits number of digits to show in printed tables.
#'   Default: max(3L, getOption("digits") - 3L).
#' @param ... further arguments passed to or from other methods.
#' @return No return value, called for side effects
#' @author
#'   \tabular{ll}{
#'   Fernando López  \tab \email{fernando.lopez@@upct.es} \cr
#'   Román Mínguez  \tab \email{roman.minguez@@uclm.es} \cr
#'   Antonio Páez \tab \email{paezha@@gmail.com} \cr
#'   Manuel Ruiz \tab \email{manuel.ruiz@@upct.es} \cr
#'   }
#'
#' @seealso
#' \code{\link{summary.spqtest}}.
#' @export
print.summary.spjctest <- function(x, digits = max(3L, getOption("digits") - 3L),
                                  ...) {
  class(x) <- c("gt_tbl") # prevent recursion...
  print(x)
  invisible(x)
}
