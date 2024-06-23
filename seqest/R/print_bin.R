#' @title Print the results by the binary logistic regression model
#'
#' @description
#' \code{print.seqbin} print the result of the binary logistic regression model
#' used by the method of adaptive shrinkage estimate.
#'
#' @details
#' This function is used to present results in a concise way. If we select
#' enough samples that satisfy the stopping criterion, then we show several
#' messages to report the conclusion including the length of fixed size
#' confidence set, the number of samples we choose, the value of coefficient and
#' the time have elapsed. Otherwise, the sample selection process is failed. We
#' need to reduce the length of fixed size confidence set because the smaller
#' the dlen, the larger the sample size we need.
#' @param x A variable of type \code{seqbin}
#' @param ... Additional variables to be transferred to functions
#' @method print seqbin
#' @export
#' @return print.seqbin returns several messages to show the conclusion.

print.seqbin <- function(x, ...){
  if (!inherits(x, "seqbin"))
    stop("Object must be of class 'seqbin'")
  if (x$is_stopped == 1){
    cat('The sample selection process is finished. \n')
    cat('The final result is shown below:','\n')
    cat("The length of fixed size confidence set (d):", x$d,'\n')
    cat("The number of samples at the end of iteration:",x$n,'\n')
    cat('The estimated coefficient at the end of iteration:',paste(round(x$beta_est,5),' '),'\n')
  }
  if (x$is_stopped == 0){
    cat("The sample selection process isn't finished. ")
    cat('Maybe you can reduce the length of d')
  }
}
