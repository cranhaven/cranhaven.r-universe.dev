# Package-level documentation and namespace setup

#' @keywords internal
#' @name matrixCorr-internal
#' @aliases RcppExports
"_PACKAGE"

## usethis namespace: start
#' @useDynLib matrixCorr, .registration = TRUE
## usethis namespace: end

# Silence NOTES about NSE vars in ggplot2 etc.
utils::globalVariables(c(
  "Var1", "Var2", "Tau", "Rho", "Pearson", "CCC",
  "label", "PCor", "r", "dCor", "bicor", "ci_label",
  "diffs", "hi_l", "hi_u", "lab", "lo_l", "lo_u", "loaL",
  "loaU", "md", "md_l", "md_u", "means", "j", "k"
))
