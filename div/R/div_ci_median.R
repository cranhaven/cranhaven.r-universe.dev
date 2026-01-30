# (C) Philippe J.S. De Brouwer -- 2021
# licensed under GNU Affero General Public License v3.0

#' Function to calculate the confidence interval for the median
#'
#' @param x numeric, data from which the median is calcualted
#' @param conf numeric, the confidence interval as 1 - P(x < x0)
#' @returns ci (confidence interval object)
#' @export
#' @examples
#' x <- 1:100
#' div_ci_median(x)

div_ci_median <-function(x, conf = 0.95){

  #note: sort(x)[qbinom(c(.025,.975), length(x), 0.5)]
  # or: wilcox.test(x,conf.level=0.95,alternative="two.sided",correct=TRUE)

  n <- nrow(as.matrix(x))  # coerce to matrix and then use nrow (so it works for vectors and matrices)
  if(qbinom((1 - conf) / 2, n, 0.5) == 0) return(NA) # there is no confidence interval possible
  L <- qbinom((1 - conf) / 2, n, 0.5)  # index of lower limit of the confidence interval
  U <- n - L + 1                       # index of upper limit of the confidence interval
  if(L >= U)  return(NA)               # obviously
  x_ordered <- sort(x)
  the_ci <- list()
  the_ci$head <- paste(paste0(as.character(conf*100),"%"), c("Confidence interval for population median"))
  the_ci$ci <- c(median = median(x), lower = x_ordered[L], upper = x_ordered[n - L + 1])
  the_ci$ends <- c("Estimate", paste(as.character(c((1 - conf) / 2, 1 - ((1 - conf) / 2)) * 100),"%",sep=""))
  the_ci$coverage <- 1 - (2 * pbinom(q = L - 1, n, 0.5))
  class(the_ci) <- "ci"
  the_ci
}
