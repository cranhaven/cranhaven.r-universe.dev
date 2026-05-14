#' Calculate the estimation of the covariance of estimated parameters in a MCD model, via the explicit formula.
#'
#' \code{covjmcm_mcd} gives an estimation of the covariance of estimated parameters in a MCD model using
#' the explicit formula, which is the inverse of the estimated Fisher's information matrix.
#'
#' @param object a fitted joint mean-covariance model of class "jmcmMod", returned by the function \code{jmcm}.
#' @return an estimated covariance matrix of the estimated parameters in a MCD model.
#' @examples
#' cattleA <- cattle[cattle$group=='A', ]
#' fit.mcd <- jmcm(weight|id|I(ceiling(day/14+1))~1|1,
#'                data = cattleA, cov.method = "mcd",
#'                triple = c(8,3,4))
#' cov.mcd <- covjmcm_mcd(fit.mcd)
#' @references [1] Pourahmadi, M., "Maximum likelihood estimation of generalised linear models for multivariate normal covariance matrix," Biometrika
#' 87(2), 425–435 (2000).
#' @seealso \code{\link{covjmcm}}, \code{\link{covjmcm_acd}}, and \code{\link{covjmcm_hpc}}
#' @importFrom MASS ginv
#' @export
covjmcm_mcd <- function(object){
  ##object is a fitted jmcm model
  if (missing(object)) stop("missing object.")
  if(object@call$cov.method!="mcd") stop("Method must be mcd")
  m <- getJMCM(object, name="m")
  n <- length(m)
  q <- length(getJMCM(object,"gamma"))
  p <- length(getJMCM(object,"beta"))
  d <- length(getJMCM(object,"lambda"))
  p1 <- p+d
  p2 <- p+q+d

  I <- matrix(0, p2, p2)

  for(i in 1:n){

    Xi <- getJMCM(object, name="X",sub.num=i)
    Zi <- getJMCM(object, name="Z",sub.num=i)
    Wi <- getJMCM(object, name="W",sub.num=i)
    Sigma <- getJMCM(object, name="Sigma", sub.num=i)
    T <- getJMCM(object, name="T", sub.num=i)
    D <- as.vector(diag(getJMCM(object, name="D", sub.num=i)))

    I11 <- t(Xi) %*% ginv(Sigma) %*% Xi

    I22 <- (1/2)*(t(Zi)%*%Zi)

    if(m[i]==1){I33 <- matrix(0,q,q)
    I23 <- matrix(0,d,q)}

    if(m[i]>1){
      wi <- array(0, dim=c(q , q,  m[i]))
      wi[,,1] <- matrix(0, q, q)
      for(j in 2:m[i]){
        for(k in 1:(j-1)){
          for(l in 1:(j-1))
            wi[,,j] <- wi[,,j] + (Sigma[k,l]/D[j])*
              (tcrossprod(Wi[sum(1:(j-1))-(j-1)+k, ],Wi[sum(1:(j-1))-(j-1)+l, ]))
        }
      }

      I33 <- apply(wi,c(1,2),sum)

      A <- Sigma %*% t(T)
      B <- matrix(0, m[i], q)

      for (j in 2:m[i]) {
        for (k in 1:(j-1))
          B[j,] <- B[j,] + A[k,j] * Wi[sum(1:(j-1))-(j-1)+k, ]
      }

      I23 <- t(Zi) %*% ((1/D)*B)
    }

    I[1:p,1:p] <- I[1:p,1:p] + I11
    I[(p+1):p1,(p+1):p1] <- I[(p+1):p1,(p+1):p1] + I22
    I[(p1+1):p2,(p1+1):p2] <- I[(p1+1):p2,(p1+1):p2] + I33
    I[(p+1):p1,(p1+1):p2] <- I[(p+1):p1,(p1+1):p2] + I23
  }

  I[(p1+1):p2,(p+1):p1] <- t(I[(p+1):p1,(p1+1):p2])
  cov <- ginv(I)
  cov
}
