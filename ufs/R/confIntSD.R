#' Confidence interval for standard deviation
#'
#' This function is vectorized.
#'
#' @param x Either a standard deviation, in which case `n` must also be
#' provided, or a vector, in which case `n` must be NULL.
#' @param n The sample size is `x` is a standard deviation.
#' @param conf.level The confidence level
#'
#' @return A vector or matrix.
#' @export
#'
#' @examples ufs::confIntSD(mtcars$mpg);
#' ufs::confIntSD(c(6, 7), c(32, 32));
confIntSD <- function(x,
                      n = NULL,
                      conf.level=.95) {

  if (is.null(n)) {
    sd <- sd(x, na.rm=TRUE);
    n <- sum(!is.na(x));
  } else {
    if (length(n) > 1) {
      if (!(length(x) == length(n))) {
        stop("You provided multiple values for `n`, but not the same number ",
             "of values for `x`!");
      }
      res <-
        do.call(
          rbind,
          mapply(
            confIntSD,
            x,
            n,
            SIMPLIFY = FALSE
          )
        );

      row.names(res) <-
        paste0("sd=", x, ", n=", n);

      return(res);
    } else {
      sd <- x;
    }
  }

  res <-
    sort(
      sqrt(
        ((n-1) * sd^2) /
          stats::qchisq(
            p=c(((1 - conf.level) / 2),
                c(1 - (1 - conf.level) / 2)),
            df=(n - 1))
      )
    );

  names(res) <-
    c("sd.ci.lo", "sd.ci.hi");

  return(res);
}
