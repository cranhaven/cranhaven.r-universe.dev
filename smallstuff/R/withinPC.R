#########1#########2#########3#########4#########5#########6#########7#########8
#' Calculate Row or Column Percentages
#'
#' Calculate percentages of values in a matrix or table with respect to the
#' row or column totals.
#'
#' @param X matrix or table
#' @param rows TRUE (default) to calculate by rows, or FALSE to calculate by
#' columns
#' @param rnd numbers of digits to round the result to
#' @return A matrix or table with percentages
#' @examples
#' (X=matrix(c(1:12),3))
#' withinPC(X)
#' @export
################################################################################
withinPC<-function(X,rows=TRUE,rnd=1) {
  if (!inherits(X,c("matrix","table"))) stop("X must be a matrix or table")
  rows=rows[1]
  if (!inherits(rows,"logical")) stop("rows must be either TRUE or FALSE")
  if (!isInt(rnd,FALSE)) stop("rnd must be a finite integer")
  if (rows) {
    Y=X/apply(X,1,sum)*100
  } else {
    Y=t(t(X)/apply(X,2,sum)*100)
  }
  round2(Y,digits=rnd)
}
