#' Calculate the estimation of the covariance of estimated parameters in a ACD model, via the explicit formula.
#'
#' \code{covjmcm_acd} calculates the estimation of the covariance of estimated parameters in a ACD model using
#' the explicit formula, which is the inverse of the estimated Fisher's information matrix.
#'
#' @param object a fitted joint mean-covariance model of class "jmcmMod", returned by the function \code{jmcm}.
#' @return an estimated covariance matrix of the estimated parameters in a ACD model.
#' @references [1] M. Maadooliat, M. Pourahmadi and J. Z. Huang, "Robust estimation of the correlation
#' matrix of longitudinal data", Statistics and Computing 23, 17-28, (2013).
#' @examples
#' ##This may take more than 5s.
#' \donttest{
#' cattleA <- cattle[cattle$group=='A', ]
#' fit.acd <- jmcm(weight|id|I(ceiling(day/14+1))~1|1,
#'                data = cattleA, cov.method = "acd",
#'                triple = c(8,3,4))
#' cov.acd <- covjmcm_acd(fit.acd)}
#' @seealso \code{\link{covjmcm}}, \code{\link{covjmcm_mcd}}, and \code{\link{covjmcm_hpc}}
#' @importFrom MASS ginv
#' @export
covjmcm_acd <- function(object){
  ##object is a fitted jmcm model
  if (missing(object)) stop("missing object.")
  if(object@call$cov.method!="acd") stop("Method must be acd")
  m <- getJMCM(object, name="m")
  n <- length(m)
  q <- length(getJMCM(object,"gamma"))
  p <- length(getJMCM(object,"beta"))
  d <- length(getJMCM(object,"lambda"))
  p1 <- p+d
  p2 <- p+q+d
  I <- matrix(0, p2, p2)

  L1order<-function(Wi,r){
    #1order-derivative for gamma(s)
    l1 <- matrix(0,m[i],m[i])
    for(j in 2:m[i]){
      for(k in 1:(j-1))
        l1[j,k] <- Wi[,r][(sum(1:(j-1))-(j-1)+k)]
    }
    l1
  }

  for(i in 1:n){
    Xi <- getJMCM(object, name="X",sub.num=i)
    Zi <- getJMCM(object, name="Z",sub.num=i)
    Wi <- getJMCM(object, name="W",sub.num=i)
    Sigma <- getJMCM(object, name="Sigma", sub.num=i)
    Li <- getJMCM(object, name="T", sub.num=i)
    Ti <- ginv(Li)
    Di <- as.vector(diag(getJMCM(object, name="D", sub.num=i)))

    I11 <- t(Xi) %*% ginv(Sigma) %*% Xi

    I22 <- matrix(0, d, d)

    if(m[i]==1) {
      for(r in 1:d){
        for (s in 1:d){
          temp1 <- tcrossprod(Li)%*%(Zi[,r]*tcrossprod(Ti))*Zi[,s]
          I22[r,s] <-  (1/4)*(crossprod(Zi[,r],Zi[,s]) + temp1)
          }
        }
    I33 <- matrix(0,q, q)
    I32 <- matrix(0, q, d)
    }

    if(m[i]>1){
      for(r in 1:d){
        for (s in 1:d){
          temp1 <- tcrossprod(Li)%*%(Zi[,r]*tcrossprod(Ti))%*%diag(Zi[,s])
          I22[r,s] <-  (1/4)*(crossprod(Zi[,r],Zi[,s]) + sum(diag(temp1)))
        }
      }
      I33 <- matrix(0, q, q)

      for(r in 1:q){
        for(s in 1:q){
          temp2 <- tcrossprod(L1order(Wi, r), L1order(Wi,s))%*%crossprod(Ti)
          I33[r,s] <- sum(diag(temp2))
        }
      }

      ##alternative formula, not simplified, same result
      ##for(r in 1:q){
      ##for(s in 1:q){
      ##temp21 <- (tcrossprod(L1order(Wi, r), Li)+tcrossprod(Li,L1order(Wi, r)))%*%
      ##crossprod(Ti)%*%(tcrossprod(L1order(Wi, s), Li)+tcrossprod(Li,L1order(Wi, s)))
      ##temp22 <- tcrossprod(L1order(Wi, r), L1order(Wi,s))+tcrossprod(L1order(Wi, s), L1order(Wi,r))
      ##temp2 <- (2*temp21-temp22) %*% crossprod(Ti)
      ##I33[r,s] <- sum(diag(temp2))/2
      ##}
      ##}

      I32 <- matrix(0,q,d)
      for(r in 1:q){
        for(s in 1:d){
          temp3 <- Di*tcrossprod(L1order(Wi,r),Li)%*%(Zi[,s]*diag(Di))%*%ginv(Sigma)
          I32[r,s] <- sum(diag(temp3))/2
        }
      }

      ## alternative formula, not simplified, same result
      ##  for(r in 1:q){
      ##  for(s in 1:d){
      ## temp3 <- diag(Di)%*%(tcrossprod(L1order(Wi,r),Li)+tcrossprod(Li, L1order(Wi,r)))%*%
      ## diag(Zi[,s])%*%diag(Di)%*%ginv(Sigma)
      ## I32[r,s] <- sum(diag(temp3))/2}}

    }

    I[1:p,1:p] <- I[1:p,1:p] + I11
    I[(p+1):p1,(p+1):p1] <- I[(p+1):p1,(p+1):p1] + I22
    I[(p1+1):p2,(p1+1):p2] <- I[(p1+1):p2,(p1+1):p2] + I33
    I[(p1+1):p2,(p+1):p1] <- I[(p1+1):p2,(p+1):p1] + I32
  }
  I[(p+1):p1,(p1+1):p2] <- t(I[(p1+1):p2,(p+1):p1])
  cov <- ginv(I)
  cov
}
