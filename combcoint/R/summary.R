#' @export
summary.bhtest <- function(object, ...) {
  cat(c("----------------------------------------------------------",
      "Bayer Hanck Test for Non-Cointegration",
      "----------------------------------------------------------",
      paste(c("Formula:", object$formula),
            collapse = " "),
      paste(c("Lags:", object$lags),
            collapse = " "),
      paste(c("Trend:", object$trend),
            collapse = " "),
      " ",
      "Underlying Tests:"),
    sep = "\n")
  test.mat <- rbind(round(object$test.stat, 4),
                    object$pval.stat
                    )
  rownames(test.mat) <- c("Test Statistics", "p-Values")
  print(test.mat)
  cat(c(" ",
        paste(c("Value of the Bayer Hanck Test statistic:", round(object$bh.test, 4)),
              collapse = " ")),
      sep = "\n")
  cat(paste(c("p-Value:", object$bh.pval), collapse = " "),
      sep = "\n")
  out <- list(bh.test = object$bh.test)
  class(out) <- c("summary.bh.test")
  invisible(out)
}




