#' kernel.function 
#' 
#' \code{kernel.function} calculates several kernel functions (uniform, triangle, epanechnikov, biweight, triweight, gaussian).
#' 
#'
#' slightly modified version of the kernel.function from the gplm package. The kernel parameter is a text string specifying the univariate kernel function which is either the gaussian pdf or proportional to (1-|u|^p)^q. Possible text strings are "triangle" (p=q=1), "uniform" (p=1, q=0), "epanechnikov" (p=2, q=1), "biweight" or "quartic" (p=q=2), "triweight" (p=2, q=3), "gaussian" or "normal" (gaussian pdf).
#' The multivariate kernels are obtained by a product of unvariate kernels K(u_1)...K(u_d) or by a spherical (radially symmetric) kernel proportional to K(||u||). (The resulting kernel is a density, i.e. integrates to 1.)
#' @param u n x d matrix
#' @param kernel text string
#' @param product or spherical kernel if d>1
#' @return matrix with diagonal elements set to \code{x}
#' @keywords kernel


kernel.function <-  function (u, kernel = "normal", product = TRUE) {
  if (kernel == "triangular") {
    kernel <- "triangle"
  }
  if (kernel == "rectangle" || kernel == "rectangular") {
    kernel <- "uniform"
  }
  if (kernel == "quartic") {
    kernel <- "biweight"
  }
  if (kernel == "normal") {
    kernel <- "gaussian"
  }
  kernel.names <- c("triangle", "uniform", "epanechnikov", 
                    "biweight", "triweight", "gaussian")
  c1 <- c(1, 0.5, 0.75, 0.9375, 1.09375, NA)
  pp <- c(1, 2, 2, 2, 2, 0)
  qq <- c(1, 0, 1, 2, 3, NA)
  names(c1) <- names(pp) <- names(qq) <- kernel.names
  if (is.null(dim(u))) {
    d <- 1
    u <- matrix(u, length(u), 1)
  }
  else {
    u <- as.matrix(u)
    d <- ncol(u)
  }
  p <- pp[kernel]
  q <- qq[kernel]
  volume.d <- pi^(d/2)/gamma(d/2 + 1)
  r1 <- c(d + 1, 1, (d + 2), (d + 2) * (d + 4), (d + 2) * (d + 
                                                             4) * (d + 6), NA)
  r2 <- c(1, 1, 2, 8, 48, NA)
  names(r1) <- names(r2) <- kernel.names
  if (p > 0) {
    if (product) {
      x <- 1 - sqrt(u * u)^p
      c <- c1[kernel]
      k <- (c^d) * apply(x^q, 1, prod) * apply(x >= 0, 
                                               1, prod)
    }
    else {
      x <- 1 - sqrt(rowSums(u * u))^p
      c <- r1[kernel]/(r2[kernel] * volume.d)
      k <- c * x^q * (x >= 0)
    }
  }
  else {
    k <- apply(dnorm(u), 1, prod)
  }
  return(k)
}





