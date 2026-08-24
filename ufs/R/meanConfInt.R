#' A confidence interval for the mean
#'
#' @param vector A vector with raw data points - either specify this
#' or a mean and then either an sd and n or an se.
#' @param mean A mean.
#' @param sd,n A standard deviation and sample size; can be specified
#' to compute the standard error.
#' @param se The standard error (cna be specified instead of `sd` and `n`).
#' @param conf.level The confidence level of the interval.
#' @param x,digits,\dots Respectively the object to print, the number of digits to
#' round to, and any additonal arguments to pass on to the `print` function.
#'
#' @return And object with elements `input`, `intermediate`, and `output`,
#' where `output` holds the result in list `ci`.
#' @rdname meanConfInt
#'
#' @examples meanConfInt(mean=5, sd=2, n=20);
#' @export
meanConfInt <- function(vector=NULL, mean=NULL, sd=NULL, n=NULL, se=NULL, conf.level=.95) {
  if (is.null(mean) & is.null(sd) & is.null(n) & is.null(se)) {
    if (is.null(vector)) {
      stop("Please specify either a vector with datapoints, or a mean and then also either sd and n or se!");
    }
    mean <- mean(vector, na.rm=TRUE);
    sd <- sd(vector, na.rm=TRUE);
    n <- sum(!is.na(vector));
    se <- sd/sqrt(n);
  } else if (!is.null(mean) & !is.null(sd) & !is.null(n)) {
    se <- sd/sqrt(n);
  } else if (is.null(mean) | is.null(se)) {
    stop("Please specify either a vector with datapoints, or a mean and then also either sd and n or se!");
  }

  res <- list();
  res$input <- list(vector=vector, mean=mean, sd=sd, n=n, se=se, conf.level=conf.level);
  res$intermediate <- list(alpha = 1-conf.level);
  res$intermediate$t.bound.lo <- stats::qt(res$intermediate$alpha/2, df=n-1);
  res$intermediate$t.bound.hi <- stats::qt(1-res$intermediate$alpha/2, df=n-1);
  ci.lo <- mean + res$intermediate$t.bound.lo * se;
  ci.hi <- mean + res$intermediate$t.bound.hi * se;
  res$output <- list(ci = matrix(c(ci.lo, ci.hi), ncol=2));
  colnames(res$output$ci) <- c('ci.lo', 'ci.hi');
  rownames(res$output$ci) <- sprintf("(mean=%.2f)", mean);
  class(res) <- 'meanConfInt';
  return(res);
}

#' @method print meanConfInt
#' @rdname meanConfInt
#' @export
print.meanConfInt <- function(x, digits=2, ...) {
  print(round(x$output$ci, digits=digits), ...);
}
