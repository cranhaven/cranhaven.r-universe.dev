#' @rdname procrustes.syncsa
#' @include procrustes.syncsa.R
#' @encoding UTF-8
#' @export
procrustes.partial <- function(x, y, z)
{
  x <- as.matrix(x)
  y <- as.matrix(y)
  z <- as.matrix(z)
  if(!options("SYNCSA.speed")$SYNCSA.speed){
    pro.residuals <- function(Y, X){
      res <- matrix(NA, dim(Y)[1], dim(Y)[2])
      for(i in 1:ncol(Y)){
        mat <- cbind(1, X)
        fast_mod <- RcppArmadillo::fastLmPure(mat, Y[,i])
        coeffs <- fast_mod$coefficients
        res[,i] <- Y[,i] - mat %*% coeffs
      }
      return(res)
    }
  } else {
    pro.residuals <- function(Y, X){
      X <- sweep(X, 2, colMeans(X, na.rm = TRUE), check.margin = FALSE)
      Y <- sweep(Y, 2, colMeans(Y, na.rm = TRUE), check.margin = FALSE)
      Yfit <- X%*%solve(t(X)%*%X)%*%(t(X))%*%Y
      Yres <- Y-Yfit
      return(Yres)
    }
  }
  scoresofz <- stats::prcomp(z, scale = TRUE)$x
  nm <- round((nrow(x)-2)/2)
  if(nm<ncol(scoresofz)){
    scoresofz <- scoresofz[, 1:nm, drop = FALSE]
  }
  x.r <- pro.residuals(x, scoresofz)
  y.r <- pro.residuals(y, scoresofz)
  statistic <- procrustes.syncsa(x.r, y.r)
  return(statistic)
}
