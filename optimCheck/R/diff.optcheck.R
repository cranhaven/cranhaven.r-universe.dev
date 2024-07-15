#' Elementwise difference between potential and optimal solutions.
#'
#' @name diff.optcheck
#' @aliases diff.summary.optcheck diff.optproj diff.summary.optproj diff.optrefit  diff.summary.optrefit
#' @param x Object of class \code{optcheck} or \code{summary.optcheck}, currently returned by \code{\link{optim_proj}}, \code{\link{optim_refit}}, or a summary of either of those calls.
#' @param ... Further arguments to be passed to or from other methods.
#' @return A two-column matrix consisting of the absolute and relative differences between the potential and optimal solutions (\code{xsol} and \code{xopt}).
#' @details This function is simply a wrapper to \code{summary(x)$xdiff} and \code{x$xdiff}, for \code{optcheck} and \code{summary.optcheck} objects respectively.
#' @export
diff.optcheck <- function(x, ...) {
  xdiff <- summary(x)$xdiff
  ## if(is.null(names(x$xsol))) {
  ##   # only add names if they are provided
  ##   rownames(xdiff) <- NULL
  ## }
  xdiff
}

#' @rdname diff.optcheck
#' @export
diff.summary.optcheck <- function(x, ...) {
  x$xdiff
}
