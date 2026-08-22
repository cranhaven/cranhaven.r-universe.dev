#' Print a brief summary of the \code{cglmm} model.
#'
#' @param x A \code{cglmm} object.
#' @param digits Controls the number of digits displayed in the summary output.
#' @param ... Additional, ignored arguments.
#'
#' @srrstats {RE4.17}
#' @srrstats {G1.4}
#'
#' @return \code{print(x)} returns \code{x} invisibly.
#'
#' @examples
#' # Single component cosinor model
#' cglmm(
#'   vit_d ~ amp_acro(time_col = time, group = "X", period = 12),
#'   data = vitamind
#' )
#' @export
print.cglmm <- function(x, digits = getOption("digits"), ...) {
  coef_list <- as.data.frame(x$raw_coefficients)
  colnames(coef_list) <- "Estimate"
  cat("\n Conditional Model \n")
  cat("\n Raw formula: \n")
  cat(deparse(x$formula), "\n")
  cat("\n Raw Coefficients: \n")
  stats::printCoefmat(coef_list, digits = digits)
  cat("\n Transformed Coefficients: \n")
  t.x <- x$coefficients

  if (x$group_check == TRUE) {
    names(t.x) <- update_covnames(names(t.x), group_stats = x$group_stats)
  }
  coef_list <- as.data.frame(t.x)
  colnames(coef_list) <- "Estimate"
  stats::printCoefmat(coef_list, digits = digits)

  if (x$dispformula_used) {
    coef_list <- as.data.frame(x$disp_list$raw_coefficients)
    colnames(coef_list) <- "Estimate"
    cat("\n***********************\n")
    cat("\n Dispersion Model \n")
    cat("\n Raw  Formula: \n")
    cat(deparse(x$disp_list$formula_disp), "\n")
    cat("\n Raw  Coefficients: \n")
    stats::printCoefmat(coef_list, digits = digits)

    if (x$dispformula_check) { # if there was an amp_acro() component
      cat("\n Transformed  Coefficients: \n")
      td.x <- x$disp_list$coefficients_disp
      if (is.null(x$disp_list$group_check_disp)) {
        x$disp_list$group_check_disp <- FALSE
      }
      if (x$disp_list$group_check_disp == TRUE) {
        names(td.x) <- update_covnames(
          names(td.x),
          group_stats = x$disp_list$group_stats_disp
        )
      }
      coef_list <- as.data.frame(td.x)
      colnames(coef_list) <- "Estimate"
      stats::printCoefmat(coef_list, digits = digits)
    }
  }

  if (x$ziformula_used) {
    coef_list <- as.data.frame(x$zi_list$raw_coefficients)
    colnames(coef_list) <- "Estimate"
    cat("\n***********************\n")
    cat("\n Zero-Inflation Model \n")
    cat("\n Raw  Formula: \n")
    cat(deparse(x$zi_list$formula_zi), "\n")
    cat("\n Raw  Coefficients: \n")
    stats::printCoefmat(coef_list, digits = digits)

    if (x$ziformula_check) {
      cat("\n Transformed  Coefficients: \n")
      tzi.x <- x$zi_list$coefficients_zi

      if (is.null(x$zi_list$group_check_zi)) {
        x$zi_list$group_check_zi <- FALSE
      }
      if (x$zi_list$group_check_zi == TRUE) {
        names(tzi.x) <- update_covnames(
          names(tzi.x),
          group_stats = x$zi_list$group_stats_zi
        )
      }
      coef_list <- as.data.frame(tzi.x)
      colnames(coef_list) <- "Estimate"
      stats::printCoefmat(coef_list, digits = digits)
    }
  }

  invisible(x)
}
