#' Treatment Effect Computations for OPSR Model Fits
#'
#' @param object object an object of class `"opsr"`.
#' @param type see [`predict.opsr`] for details.
#' @param weights a vector of weights. If `NULL` then weights from `object` will
#'   be used.
#' @param ... additional arguments passed to [`predict.opsr`].
#'
#' @return An object of class `"opsr.te"`.
#'
#' @details
#' This function only prepares the input to a further call to [`summary.opsr.te`].
#'
#' @seealso [`summary.opsr.te`]
#' @example R/examples/ex-opsr_te.R
#' @export
opsr_te <- function(object, type = c("response", "unlog-response", "prob", "mills", "correction", "Xb"),
                    weights = NULL, ...) {
  type <- match.arg(type)
  if (is.null(weights)) {
    weights <- object$weights
  }

  ce.group <- function(object, group, type, ...) {
    p <- lapply(seq_len(object$nReg), function(x) {
      predict(object, group = group, counterfact = x, type = type, ...)
    })
    p <- Reduce(cbind, p)
    dimnames(p) <- NULL
    attr(p, "group") <- group
    class(p) <- c("ce.group", class(p))
    p
  }

  ce.treatment <- function(object, treatment, type, ...) {
    p <- lapply(seq_len(object$nReg), function(x) {
      predict(object, group = x, counterfact = treatment, type = type, ...)
    })
    p <- Reduce(cbind, p)
    dimnames(p) <- NULL
    attr(p, "treatment") <- treatment
    class(p) <- c("ce.treatment", class(p))
    p
  }

  ce.by.groups <- function(object, type, ...) {
    p <-
      lapply(seq_len(object$nReg), function(x) {
        ce.group(object, group = x, type = type, ...)
      })
    class(p) <- c("ce.by.groups", class(p))
    p
  }

  ce.by.treatments <- function(object, type, ...) {
    p <-
      lapply(seq_len(object$nReg), function(x) {
        ce.treatment(object, treatment = x, type = type, ...)
      })
    class(p) <- c("ce.by.treatments", class(p))
    p
  }

  average <- function(x, weights) {
    apply(x, 2, function(x) {
      stats::weighted.mean(x, w = weights, na.rm = TRUE)
    })
  }

  ## rows = group, cols = treatment
  ce.mean <- function(object, type, weights, ...) {
    nReg <- object$nReg
    ap <- lapply(seq_len(nReg), function(x) {
      pg <- ce.group(object, group = x, type = type, ...)
      average(pg, weights = weights)
    })
    ap <- Reduce(cbind, ap)
    dimnames(ap) <- NULL
    class(ap) <- c("ce.mean", class(ap))
    ap
  }

  ## apply funcs above
  pbg <- ce.by.groups(object, type = type, ...)
  pbt <- ce.by.treatments(object, type = type, ...)
  pm <- ce.mean(object, weights = weights, type = type, ...)

  ## structure output
  out <- list()
  out$fit <- object
  out$call <- match.call()
  out$ce.by.groups <- pbg
  out$ce.by.treatments <- pbt
  out$ce.mean <- pm
  out$nReg <- object$nReg
  out$weights <- weights

  class(out) <- c("opsr.te", class(out))
  out
}
