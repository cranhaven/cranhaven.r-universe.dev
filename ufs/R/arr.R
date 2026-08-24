#' Absolute Relative Risk and confidence interval
#'
#' This is a function to conveniently and quickly compute
#' the absolute relative risk (ARR) and its confidence interval.
#'
#' @param expPos Number of positive events in the experimental condition.
#' @param expN Total number of cases in the experimental condition.
#' @param conPos Number of positive events in the control condition.
#' @param conN Total number of cases in the control condition.
#' @param conf.level The confidence level for the confidence interval.
#' @param digits The number of digits to round to when printing the results.
#' @param printAsPercentage Whether to multiply with 100 when printing the results.
#' @param x The result of the call to `arr`.
#' @param ... Any additional arguments are neglected.
#'
#' @return An object with in `estimate`, the ARR, and in `conf.int`, the
#' confidence interval.
#' @rdname arr
#' @export
#'
#' @examples ufs::arr(10, 60, 20, 60);
arr <- function(expPos, expN,
                conPos, conN,
                conf.level = .95,
                digits = 2,
                printAsPercentage = TRUE) {

  res <- list(estimate = (conPos / conN) - (expPos / expN));

  res$se <-
    sqrt(

      (((expPos / expN) * (1 - (expPos / expN))) / expN) +
      (((conPos / conN) * (1 - (conPos / conN))) / conN)

    );

  res$conf.level <- conf.level;

  res$z <- stats::qnorm(1-(1-conf.level)/2);

  res$margin.of.error <- res$z * res$se;

  res$conf.int <-
    c(res$estimate - res$margin.of.error,
      res$estimate + res$margin.of.error);

  res$digits <- digits;
  res$printAsPercentage <- printAsPercentage;

  class(res) <- "ufsARR";

  return(res);

}

#' @method print ufsARR
#' @rdname arr
#' @export
print.ufsARR <- function(x,
                         digits = x$digits,
                         printAsPercentage = x$printAsPercentage,
                         ...) {
  if (printAsPercentage) {
    multiplier <- 100;
  } else {
    multiplier <- 1;
  }
  ufs::cat0(
    "Absolute Risk Reduction = ",
    ufs::formatCI(
      multiplier * x$conf.int,
      digits = digits,
      noZero = TRUE),
    " (point estimate: ",
    round(multiplier * x$estimate,
          digits = digits),
    ")");
  return(invisible(x));
}
