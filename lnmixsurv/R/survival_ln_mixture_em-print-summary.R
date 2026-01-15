#' @export
print.survival_ln_mixture_em <- function(x, digits = NULL, ...) {
  if (is.null(digits)) digits <- getOption("digits")
  fixed <- tidy.survival_ln_mixture_em(x, effects = "fixed")
  auxiliary <- tidy.survival_ln_mixture_em(x, effects = "auxiliary")
  
  cat("survival_ln_mixture_em")
  cat("\n formula:", extract_formula(x))
  cat("\n observations:", nobs(x))
  cat("\n iterations:", niterations(x))
  cat("\n predictors:", npredictors(x))
  cat("\n mixture groups:", length(x$mixture_groups))
  cat("\n------------------\n")
  cat("\n Fixed parameter(s):\n")
  print(data.frame(fixed[, -1], row.names = fixed$term), digits = digits)
  cat("\nAuxiliary parameter(s):\n")
  print(data.frame(auxiliary[, -1], row.names = auxiliary$term), digits = digits)
  cat("\n------------------\n")
  cat("\n AIC:", stats::AIC(x))
  cat("\n BIC:", stats::BIC(x))
}
