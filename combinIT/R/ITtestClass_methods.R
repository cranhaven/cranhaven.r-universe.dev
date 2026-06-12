#' @export
print.ITtest <- function(x, ...) {
  msg1 <- paste(" Test:\t", x$test, "\n")
  msg2 <- paste("Data:\t", x$data_name, "\n")
  msg3 <- paste("Statistic = ", round(x$statistic, 3), "\n")
  msg4 <- paste("Exact Monte Carlo P-value = ", ifelse(is.numeric(x$pvalue_exact), round(x$pvalue_exact, 3), "NULL"), "\n")
  msg5 <- paste("Approximate P-value = ", ifelse(is.numeric(x$pvalue_appro), round(x$pvalue_appro, 3), "NULL"), "\n")
  msg6 <- paste("Nsim = ", x$nsim, "\n")
  msg9 <- paste("---------------------------------------", "\n")
  msg7 <- paste("A report on the test:", "\n")
  msg8 <- paste(x$Result, "\n")
  cat(msg1, msg2, msg3, msg4, msg5, msg6, msg9, justify(c(msg7, msg8)), "\n")
}
