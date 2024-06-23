#' @title Print the results by the multi-logistic regression model
#'
#' @description
#' \code{print.seqmulti} print the result of the multi-logistic regression model
#'
#' @details
#' This function is used to present results in a concise way. If we select
#' enough samples that satisfy the stopping criterion, then we show several
#' messages to report the conclusion including the length of fixed size
#' confidence set, the number of samples we choose and the value of coefficient.
#' @param x A variable of type \code{seqmulti}
#' @param ... Additional variables to be transferred to functions
#' @method print seqmulti
#' @export
#' @return print.seqmulti returns several messages to show the conclusion.
#'
#'


print.seqmulti <- function(x, ...){
  if (!inherits(x, "seqmulti"))
    stop("Object must be of class 'seqmulti'")
  if (x$is_stopped == TRUE){
    cat('The sample selection process is finished. \n')
    cat('The final result is shown below:','\n')
    cat("The length of fixed size confidence set (d):", x$d,'\n')
    cat("The number of samples at the end of iteration:",x$n,'\n')
    cat('The estimated coefficient at the end of iteration:',paste(round(x$beta_est,5),' '),'\n')
  }
  if (x$is_stopped == FALSE){
    cat("The sample selection process isn't finished. ")
  }
}
