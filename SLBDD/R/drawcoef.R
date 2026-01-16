#' Random Draw of Coefficients for AR Models and MA Models
#'
#' Random draw of polynomial coefficients for stationary AR models or invertible MA models.
#' The resulting polynomial has solutions outside the unit circle.
#'
#' @param deg Degree of the polynomial. Maximum degree is 5.
#' @param delta The minimum distance of a polynomial root from the boundary 1 or -1. The default is 0.02.
#'
#' @return \eqn{c = (c1,c2,...)} denotes the coefficients of \eqn{1-c1*x-c2*x^2-...}.
#'
#' @examples
#' draw.coef(2)
#' @export
"draw.coef" <- function(deg, delta = 0.02){
  draw1 <- function(n){
    c <- runif(n,min=-1+delta,max=1-delta)
    c
  }

  draw2 <- function(n){
    c1 <- runif(n,min=-2+delta,max=2-delta)
    c2 <- NULL
    for (i in 1:n){
      if(c1[i] < 0){
        d1 <- -1+delta; d2 <- 1+c1[i]-delta
        if(d2 <= d1)d2 <- 1+c1[i]
        r <- runif(1,min=d1,max=d2)
      }else{
        d1 <- -1+delta; d2 <- 1-c1[i]-delta
        if(d2 <= d1)d2 <- 1-c1[i]
        r <- runif(1,min=d1,max=d2)
      }
      c2 <- rbind(c2,c(c1[i],r))
    }
    c2
  }

  if(deg > 5){
    message("Maximum degree is set at 5: ","\n")
    deg <- 5
  }
  if(deg==1)c <- draw1(1)
  if(deg==2)c <- draw2(1)
  if(deg==3){c1 <- draw1(1); c2 <- draw2(1)
  c <- polyprod(c1,c2)
  }
  if(deg==4){c1 <- draw2(1); c2 <- draw2(1)
  c <- polyprod(c1,c2)
  }
  if(deg==5){c1 <- draw2(1); c2 <- draw2(1); c3 <- draw1(1)
  c4 <- polyprod(c1,c2)
  c <- polyprod(c4,c3)
  }
  c
}

"polyprod" <- function(a,b){
  d1 <- length(a)
  d2 <- length(b)
  if(d1 < 1)c=b
  if(d2 < 1)c=a
  if(min(d1,d2) > 0){
    c=rep(0,d1+d2)
    c[1:d1] <- a
    A <- c(-1,a)
    for (i in 1:d2){
      ii <- i-1
      c[(ii+1):(ii+d1+1)] <- c[(ii+1):(ii+d1+1)]-b[i]*A
    }
  }
  c
}
