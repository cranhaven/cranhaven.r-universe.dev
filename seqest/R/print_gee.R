#' @title Print the results by the generalized estimating equations.
#'
#' @description
#' \code{print.seqGEE} print the result of the logistic regression model used by
#' the method of adaptive shrinkage estimate.
#'
#' @details
#' This function is used to present results in a concise way. If we select
#' enough samples that satisfy the stopping criterion, then we show several
#' messages to report the conclusion including the length of fixed size
#' confidence set, the number of samples we choose, the value of coefficient and
#' the index of the non zero coefficient
#' @param x A variable of type \code{seqGEE}
#' @param ... Additional variables to be transferred to functions
#' @method print seqGEE
#' @export
#' @return print.seqGEE returns several messages to show the conclusion.

print.seqGEE <- function(x, ...){
  if (!inherits(x, "seqGEE"))
    stop("Object must be of class 'seqGEE'")
  if (x$is_stopped$stop == TRUE){
    cat('The sample selection process is finished. \n')
    cat('The final result is shown below:','\n')
    cat("The length of fixed size confidence set (d):", x$d,'\n')
    cat("The number of samples at the end of iteration:",x$n,'\n')
    cat('The estimated coefficient at the end of iteration:',paste(round(x$beta_est,5),' '),'\n')
    cat('The index of the non zero coefficient:',x$nonZeroIdx, '\n')

  }
  if (x$is_stopped$stop == FALSE){
    cat("The sample selection process isn't finished. ")
  }
}
