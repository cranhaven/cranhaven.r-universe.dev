#' Construct Design Matrices for OPSR Model Fits
#'
#' @param object an object of class `"opsr"`.
#' @param data a data frame containing the terms from `object$formula`. Passed to
#'   [`model.frame.opsr`]. Can be omitted.
#' @param .filter used internally in [`predict.opsr`] for counterfactual predictions.
#' @param ... further arguments passed to or from other methods.
#'
#' @return A list of lists with the design matrices `W` (selection process) and
#'   `X` (outcome process). Both of these lists have `object$nReg` elements (a
#'   separate design matrix for each regime).
#' @method model.matrix opsr
#' @seealso [`model.frame.opsr`], [`stats::model.matrix`]
#' @export
model.matrix.opsr <- function(object, data, .filter = NULL, ...) {
  mf <- if (missing(data)) model.frame(object) else model.frame(object, data = data)
  f <- object$formula
  nReg <- object$nReg
  nParts <- object$nParts
  z <- Formula::model.part(f, data = mf, lhs = 1, drop = TRUE)
  y <- Formula::model.part(f, data = mf, lhs = 2, drop = TRUE)

  w <- model.matrix(f, mf, rhs = 1)
  w <- w[, !(colnames(w) %in% "(Intercept)"), drop = FALSE]  # no intercept (identification threshold)!
  W <- lapply(seq_len(nReg), function(i) {
    j <- .filter %||% i
    z_not_present <- all(z != j)
    if (z_not_present) NULL else w[z == j, , drop = FALSE]
  })

  X <- lapply(seq_len(nReg), function(i) {
    rhs <- ifelse(nParts == 2, 2, i + 1)  # first is for selection process
    x <- model.matrix(f, mf, rhs = rhs)
    j <- .filter %||% i
    z_not_present <- all(z != j)
    if (z_not_present) NULL else x[z == j, , drop = FALSE]
  })

  list(W = W, X = X)
}
