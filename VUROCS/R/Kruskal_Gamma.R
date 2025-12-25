#' @title Kruskal's Gamma and its asymptotic standard errors
#' @description Computes Kruskal's Gamma on a given cartesian product Y x f(X), where Y consists of the components of \code{y} and f(X) consists of the components of \code{fx}. Furthermore, the asymptotic standard error as well as the modified asymptotic standard error to test the null hypothesis that the measure is zero are provided as defined in Brown and Benedetti (1977).
#' @param y a vector of realized categories.
#' @param fx a vector of predicted values of the ranking function f.
#' @return A list of length three is returned, containing the following components:
#' \item{val}{Kruskal's Gamma}
#' \item{ASE}{the asymptotic standard error of Kruskal's Gamma}
#' \item{ASE0}{the modified asymptotic error of Kruskal's Gamma under the null hypothesis}
#' @examples Kruskal_Gamma(rep(1:5,each=3),c(3,3,3,rep(2:5,each=3)))
#' @references Brown, M.B., Benedetti, J.K., 1977. Sampling Behavior of Tests for Correlation in Two-Way Contingency Tables. Journal of the American Statistical Association 72(358), 309-315

Kruskal_Gamma <- function(y,fx){

  if (any(is.na(fx)) | any(is.na(y))) {
    stop("\n both 'fx' and 'y' must not contain NA values")
  }
  
  if (length(fx)!=length(y)) {
    stop("\n both 'fx' and 'y' must be of the same length")
  }

  CT  <- table(y,fx)
  b  <- ncol(CT)
  a  <- nrow(CT)
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

  Kruskal_Gamma <- list()
  Kruskal_Gamma$val  <- (P-Q)/(P+Q)
  Kruskal_Gamma$ASE0 <- 2/(P+Q)*sqrt(sum(CT*(C-D)^2)-(P-Q)^2/W)
  Kruskal_Gamma$ASE  <- 4/(P+Q)^2*sqrt(sum(CT*(Q*C-P*D)^2))

  return(Kruskal_Gamma)
}
