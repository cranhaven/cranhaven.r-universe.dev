#' Calculate the estimation of the covariance of estimated parameters in a HPC model, via the explicit formula.
#'
#' \code{covjmcm_hpc} gives the estimation of the covariance of estimated parameters in a HPC model using
#' the explicit formula, which is the inverse of the estimated Fisher's information matrix.
#'
#' @param object a fitted joint mean-covariance model of class "jmcmMod", returned by the function \code{jmcm}.
#' @return an estimated covariance matrix of the estimated parameters in a HPC model.
#' @references [1] W. Zhang, C. Leng, and C. Y. Tang(2015), "A joint modelling approach for longitudinal studies,"
#' Journal of the Royal Statistical Society. Series B. 77, 219-238.
#' @examples
#' ##This may take more than 1 min.
#' \donttest{
#' cattleA <- cattle[cattle$group=='A', ]
#' fit.hpc <- jmcm(weight|id|I(ceiling(day/14+1))~1|1,
#'                data = cattleA, cov.method = "hpc",
#'                triple = c(8,3,4))
#' cov.hpc <- covjmcm_hpc(fit.hpc)}
#' @seealso \code{\link{covjmcm}}, \code{\link{covjmcm_mcd}}, and \code{\link{covjmcm_acd}}
#' @export
covjmcm_hpc <- function(object){
  ##object is a fitted jmcm model
  if (missing(object)) stop("missing object.")
  if(object@call$cov.method!="hpc") stop("Method must be hpc")
  m <- getJMCM(object, name="m")
  n <- length(m)
  gamma <- getJMCM(object,"gamma")
  q <- length(gamma)
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

    Ti <- getJMCM(object, name="T", sub.num=i)
    D <- as.vector(diag(getJMCM(object, name="D", sub.num=i)))
    R <- Ti %*% t(Ti)

    I11 <- t(Xi) %*% ginv(Sigma) %*% Xi

    I22 <- (1/4)*(t(Zi)%*%(diag(rep(1,m[i]))+ginv(R)*R)%*%Zi)

    if(m[i]==1){I33 <- matrix(0,q,q)
    I23 <- matrix(0,q,d)}

    phi <- Wi %*% gamma
    Ai <- ginv(Ti)
    Ei <- (1/tan(as.vector(phi))) * Wi
    ##dTijj/dgamma
    Gi <- matrix(0, m[i], q)
    ##dTijk/dgamma
    Fi <- matrix(0, sum(1:(m[i]-1)) ,q)
    ##b_{ijk}
    Bi <- matrix(0, sum(1:(m[i]-1)) ,q)

    if(m[i]>1){

      for(j in 2:m[i]){

        temp <-  Ei[(sum(1:(j-1))-(j-1)+1):sum(1:(j-1)), ]
        Gi[j,] <- switch(class(temp)[1],
                         "numeric" = Ti[j,j] * temp,
                         "matrix" = Ti[j,j] * colSums(temp))
        for(k in 1:(j-1)){
          k1 <- sum(1:(j-1))-(j-1)+k
          if(k==1) Fi[k1, ] <- -Ti[j,k] * tan(phi[k1,]) * Wi[k1,]
          temp0 <- Ei[(k1-k+1):(k1-1), ]
          if(k>1)  Fi[k1, ] <- switch(class(temp0)[1],
                                      "numeric" = Ti[j,k]*(-tan(phi[k1,])*Wi[k1,] + temp0),
                                      "matrix" = Ti[j,k]*(-tan(phi[k1,])*Wi[k1,] + colSums(temp0)))

        }
      }

      for(j in 2:m[i]){
        for(k in 1:(j-1)){
          k1 <- sum(1:(j-1))-(j-1)+k
          for(l in (k+1):j)
            Bi[k1, ] <- Bi[k1, ] + Ai[j,l]*Fi[(sum(1:(l-1))-(l-1)+k), ]
          Bi[k1,] <- Bi[k1,] + Ai[j, k]*Gi[k, ]
        }
      }

      I33 <- matrix(0, q, q)
      for(j in 2:m[i]){
        temp1 <- Bi[(sum(1:(j-1))-(j-1)+1):sum(1:(j-1)),]
        temp2 <- switch(class(temp1)[1],
                        "numeric" = tcrossprod(Bi[(sum(1:(j-1))-(j-1)+1):sum(1:(j-1)),]),
                        "matrix" = crossprod(Bi[(sum(1:(j-1))-(j-1)+1):sum(1:(j-1)),]))
        I33 <- I33 + 2*tcrossprod(Gi[j, ]/Ti[j,j])+temp2
      }

      I32 <- matrix(0, q, d)
      for(j in 2:m[i]){
        b <- matrix(0, q, d)
        for(k in 1:(j-1)){
          a <- rep(0,d)
          for(l in k:j) a <- a + Ti[l,k]*Ai[j,k]*Zi[l,]
          b <- b + as.matrix(Bi[sum(1:(j-1))-(j-1)+k,]) %*% t(as.matrix(a))
        }
        I32 <- I32 + as.matrix(Gi[j, ]/Ti[j,j]) %*% Zi[j, ]+(1/2)*b
      }

    }

    I[1:p,1:p] <- I[1:p,1:p] + I11
    I[(p+1):p1,(p+1):p1] <- I[(p+1):p1,(p+1):p1] + I22
    I[(p1+1):p2,(p1+1):p2] <- I[(p1+1):p2,(p1+1):p2] + I33
    I[(p+1):p1,(p1+1):p2] <- I[(p+1):p1,(p1+1):p2] + t(I32)
  }
  I[(p1+1):p2,(p+1):p1] <- t(I[(p+1):p1,(p1+1):p2])
  cov <- ginv(I)
  cov
}
