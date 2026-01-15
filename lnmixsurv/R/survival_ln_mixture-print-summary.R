#' @export
print.survival_ln_mixture <- function(x, digits = NULL, ...) {
  if (is.null(digits)) digits <- getOption("digits")
  fixed <- tidy.survival_ln_mixture(x, effects = "fixed", conf.int = TRUE)
  auxiliary <- tidy.survival_ln_mixture(x, effects = "auxiliary", conf.int = TRUE)
  cat("survival_ln_mixture")
  cat("\n formula:", extract_formula(x))
  cat("\n observations:", nobs(x))
  cat("\n predictors:", npredictors(x))
  cat("\n mixture groups:", length(x$mixture_groups))
  cat("\n------------------\n")
  print(data.frame(fixed[, -1], row.names = fixed$term), digits = digits)
  cat("\nAuxiliary parameter(s):\n")
  print(data.frame(auxiliary[, -1], row.names = auxiliary$term), digits = digits)
}
