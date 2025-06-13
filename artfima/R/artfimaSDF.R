#Source: artfimaSDF.R
#spectral density function at Fourier frequencies given n
#area under sdf (0, pi) is half the time series variance assuming innovation
#  variance equals 1.0.
#
artfimaSDF <-function(n=100, d=0, lambda=0, phi=numeric(0), theta=numeric(0), 
                     obj=NULL, plot=c("loglog", "log", "none")) {
  plot <-  match.arg(plot)
  if (!is.null(obj)) {
    if ("artfima" == class(obj)) {
      d <- obj$dHat
      lambda <- obj$lambdaHat
      phi <- obj$phiHat
      theta <- obj$thetaHat
    }
  }  
  lams <- 2*pi*seq(from=1/n, to=1/2, by=1/n)
  if (length(lambda)==0) {
    s <- sdfarfima(n, d=d, phi=phi, theta=theta)
    if (plot=="none") {
      return(s)
    }
    if (plot=="log") {
      plot(lams, log(s), type="l", xlab="frequency", ylab="log sdf")
    } else {
      plot(log(lams), log(s), type="l", xlab="log frequency", ylab="log sdf")
    }
    return(s)
  }
  nf <- length(lams)
  a <- outer(lams, 1:length(theta))
  if (length(theta)>0) {
    C <- cbind(1, cos(a))%*% c(1, -theta)
    S <- sin(a) %*% theta
  } else {
    C <- 1
    S <- 0
  }
  num <- as.vector(C*C + S*S)
  a <- outer(lams, 1:length(phi))
  if (length(phi)>0) {
    C <- cbind(1, cos(a))%*% c(1, -phi)
    S <- sin(a) %*% phi
  } else {
    C <- 1
    S <- 0
  }
  den <- as.vector(C*C+S*S)
  s1 <- num/den
  s2 <- (1 + exp(-2*lambda) - (2*cos(lams))/exp(lambda))^(-d)
  s <- s1*s2
  if (plot=="none") {
    return(s)
  }
  if (plot=="log") {
    plot(lams, log(s), type="l", xlab="frequency", ylab="log sdf")
  } else {
    plot(log(lams), log(s), type="l", xlab="log frequency", ylab="log sdf")
  }
  s
}

sdffi <- function(n, d, lambda){
  w <- 2*pi*seq(from=1/n, to=1/2, by=1/n)
  (1 + exp(-2*lambda) - (2*cos(w))/exp(lambda))^(-d)
}

sdfarfima<- function(n, d=0, phi=numeric(0), theta=numeric(0)) {
  #model assumed stationary and invertible
  lams <- 2*pi*seq(from=1/n, to=1/2, by=1/n)
  nf <- length(lams)
  a <- outer(lams, 1:length(theta))
  if (length(theta)>0) {
    C <- cbind(1, cos(a))%*% c(1, -theta)
    S <- sin(a) %*% theta
  } else {
    C <- 1
    S <- 0
  }
  #num <- as.vector(C*C + S*S)/(2*pi)#adjust area
  num <- as.vector(C*C + S*S)
  a <- outer(lams, 1:length(phi))
  if (length(phi)>0) {
    C <- cbind(1, cos(a))%*% c(1, -phi)
    S <- sin(a) %*% phi
  } else {
    C <- 1
    S <- 0
  }
  den <- as.vector(C*C+S*S)
  s1 <- num/den
  if (length(d)==0) {
    s2 <- 1
  } else {
    s2 <- (2*sin(lams/2))^(-2*d)
  }
  s1*s2
}
