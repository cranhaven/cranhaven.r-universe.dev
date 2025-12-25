#' @title Somers' D and its asymptotic standard errors
#' @description Computes Somers' D on a given cartesian product Y x f(X), where Y consists of the components of \code{y} and f(X) consists of the components of \code{fx}. Furthermore, the asymptotic standard error as well as the modified asymptotic standard error to test the null hypothesis that the measure is zero are provided as defined in Goktas and Oznur (2011).
#' @param y a vector of realized categories.
#' @param fx a vector of predicted values of the ranking function f.
#' @return A list of length three is returned, containing the following components:
#' \item{val}{Somers' D}
#' \item{ASE}{the asymptotic standard error of Somers' D}
#' \item{ASE0}{the modified asymptotic error of Somers' D under the null hypothesis.}
#' @examples SomersD(rep(1:5,each=3),c(3,3,3,rep(2:5,each=3)))
#' @references Goktas, A., Oznur, I., 2011. A Comparison of the Most Commonly Used Measures of Association for Doubly Ordered Square Contingency Tables via Simulation. Metodoloski zvezki 8 (1), 17-37



SomersD <- function(y,fx){

  if (any(is.na(fx)) | any(is.na(y))) {
    stop("\n both 'fx' and 'y' must not contain NA values")
  }
  
  if (length(fx)!=length(y)) {
    stop("\n both 'fx' and 'y' must be of the same length")
  }

  CT  <- table(y,fx)
  b  <- ncol(CT)
  a  <- nrow(CT)
  r  <- rowSums(CT)
  RR <- cumsum(r)+(1-r)/2
  W <- length(y)

  C1 <- D1 <- matrix(0,ncol=b,nrow=a)
  if(a>2 && b>2){
    C1 <- funC(CT)
    D1 <- funD(CT)
  }

  C2 <- D2 <- matrix(0,ncol=b,nrow=a)
  if (b>2) {
    C2 <- funC1(CT)
    D2 <- funD1(CT)
  }

  C3 <- D3 <- matrix(0,ncol=b,nrow=a)
  if (a>2) {
    C3 <- funC2(CT)
    D3 <- funD2(CT)
  }
  C <- C1 + C2 + C3
  D <- D1 + D2 + D3

  C[1,1] <- sum(CT[2:a,2:b])
  C[a,b] <- sum(CT[1:(a-1),1:(b-1)])
  C[a,1] <- C[1,b] <- 0

  D[1,b] <- sum(CT[2:a,1:(b-1)])
  D[a,1] <- sum(CT[1:(a-1),2:b])
  D[1,1] <- D[a,b] <- 0

  P  <- sum(CT*C)
  Q  <- sum(CT*D)
  Dr <- W^2-sum(r^2)

  SomersD      <- list()
  SomersD$val  <- (P-Q)/Dr
  SomersD$ASE0 <- 2/Dr*sqrt(sum(CT*(C-D)^2)-(P-Q)^2/W)
  SomersD$ASE  <- 2/Dr^2*sqrt(sum(CT*(Dr*(C-D)-(P-Q)*(W-matrix(rep(RR,b),byrow=FALSE,ncol=b)))^2))

  return(SomersD)
}