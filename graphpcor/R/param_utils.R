#' Internal functions to map between Euclidean
#' and spherical coordinates
#' @name param-utils
#' @details
#' For details, please see the wikipedia entry on 'N-sphere' at
#' [N-sphere](https://en.wikipedia.org/wiki/N-sphere)
NULL
#> NULL

#' @describeIn param-utils
#' Map between spherical to Euclidean coordinates
#' @param rphi numeric vector where the first element
#' is the radius and the remaining are the angles
rphi2x <- function(rphi) {
  ### to convert from \{r, \phi_1, ..., \phi_{m-1} \} into x_i \in \Re
  ### see https://en.wikipedia.org/wiki/N-sphere
  co <- cos(rphi[-1])
  si <- cumprod(sin(rphi[-1]))
  x <- rphi[1] * c(co, 1) * c(1, si)
  return(x)
}
#' @describeIn param-utils
#' Transform from Euclidean coordinates to spherical
#' @param x parameters in the Euclidean space to be converted
x2rphi <- function(x) {
  ### to convert from x_i \in \Re into \{r, \phi_1, ..., \phi_{m-1} \}
  ### see https://en.wikipedia.org/wiki/N-sphere
  ### NOTE: from x to phi it may give phi[m-1]<0, if so add 2*pi
  m <- length(x)
  if(m>1) {
    phi <- numeric(m-1)
    phi[m-1] <- atan2(x[m], x[m-1])
    if(phi[m-1]<0)
      phi[m-1] <- phi[m-1] + 2*pi
    r2 <- x[m]^2 + x[m-1]^2
    if(m>2) {
      for(i in (m-2):1) {
        phi[i] <- atan2(sqrt(r2), x[i])
        r2 <- r2 + x[i]^2
      }
    }
  } else {
    r2 <- x^2
    phi <- NULL
  }
  return(c(sqrt(r2), phi))
}
#' @describeIn param-utils
#' Drawn samples from the PC-prior for correlation
#' @param n integer to define the size of the correlation matrix
#' @param lambda numeric as the parameter for the
#' Exponential distribution of the radius
#' @param R scaling matrix (square root of the Hessian
#' around the base model)
#' @param theta.base numeric vector of the base model
#' @importFrom stats runif
#' @importFrom stats rexp
rtheta <- function(n, lambda=1, R, theta.base) {
  m <- n*(n-1)/2
  r <- rexp(1, lambda) ## radial coordinate
  if(m>1) {
    phi <- numeric(m-1)
    if(m>2)
      phi[1:(m-2)] <- runif(m-2, 0, pi)  ## m-2 angles
    phi[m-1] <- runif(1, 0, 2*pi)  ## last angle

  } else {
    phi <- NULL
  }
  if(missing(R)) {
    R <- diag(x = rep(1, m), nrow = m, ncol = m)
  }
  out <- rphi2x(c(r, phi)) %*% R
  if(!missing(theta.base)){
    out <- out + theta.base
  }
  return(drop(out))
}
#' @describeIn param-utils
#' PC-prior density for the correlation matrix
#' @param theta numeric vector of length `m`.
#' @param H.elements list output of theta2H
dtheta <- function(theta, lambda, theta.base, H.elements) {
  ## log(\lambda)-r\lamda -log(2)-(m-1)log(\pi) +log(|J1|) +log(|J2|)
  ## log(|J1|) = {m-1}*log(r) + \sum_{i=1:m-1} (m-i-1)*log(sin(\phi_i))
  ## log(|J2|) is numerically evaluated as log(abs(det(H)))
  stopifnot(length(theta)==length(theta.base))
  m <- length(theta)
  ld2 <- 2*sum(log(abs(H.elements$svd$d))) ## log(abs(det(H)))
  x <- drop(H.elements$hneg.5 %*%theta) - theta.base
  rphi <- x2rphi(x)
  stopifnot(all(rphi>0))
  ld1 <- (m-1) * log(rphi[1])
  if(m>2) {
    ii <- 1:(m-2) ### do not include the last one!
    ld1 <- ld1 + sum(log(sin(rphi[ii+1])) * (m-ii-1))
  }
  cr <- log(2)+log(pi)+log(rphi[1])
  ld <- log(lambda) -rphi[1]*lambda -log(2) -(m-1)*log(pi)
  return(ld + ld1 + ld2 +cr)
}
