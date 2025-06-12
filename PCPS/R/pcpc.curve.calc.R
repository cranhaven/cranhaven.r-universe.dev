#' @rdname pcps.curve
#' @include pcps.curve.R
#' @encoding UTF-8
#' @export
pcpc.curve.calc <- function(values, vectors, mt){
  n.col <- ncol(vectors)
  res <- matrix(NA, nrow = n.col, ncol=2)
  for(i in 1:n.col){
    mat <- cbind(1, vectors[, 1:i])
    res[i,2] <- suppressWarnings(summary(RcppArmadillo::fastLm(mat, mt[,1,drop = FALSE]))$r.squared)
  }
  res[,1] <- values[1:n.col, 3]
  colnames(res) <- c("Cum_PCPS_Eig","Coef_Deter")
  return(res)
}