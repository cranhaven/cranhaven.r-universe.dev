#' @title Matrix multiplication
#'
#' @description Internal function to get the matrix product when missing data (NA) is found in matrix Y.
#'
#' @details The function ignore missing data when found in matrix Y. Before multiplication of matrices
#' the missing data in Y are replaced by 0 and multiplication is performed, and then, an adjustment is
#' performed. This adjustment is done by divide each cell of the product matrix by the sum of
#' proportions of nonzero at X with complete data in Y. In SYNCSA context this adjustment is done by
#' divide each cell of the product matrix by the sum of species proportion with trait data in Y.
#' Important, the matrix X must be standardized, in other words, row totals must be equal to 1.
#'
#' @encoding UTF-8
#' @param X A matrix, typically the standardized community matrix (W).
#' @param Y A matrix.
#' @return The matrix product.
#' @author Vanderlei Julio Debastiani <vanderleidebastiani@@yahoo.com.br>
#' @seealso \code{\link{syncsa}}, \code{\link{matmult}},
#' @keywords SYNCSA
#' @export
matmult.syncsa <- function(X, Y){
  y.NA <- is.na(Y)
  Y[y.NA] <- 0
  res <- X%*%Y
  if(any(y.NA)){
    X <- sweep(X, 1, rowSums(X, na.rm = TRUE), "/")
    adjustment <- X%*%ifelse(y.NA, 0, 1)
    res <- res/adjustment
  }
  return(res)
}
