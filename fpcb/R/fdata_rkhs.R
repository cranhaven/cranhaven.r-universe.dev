#' functional data in rkhs
#' 
#' Representing functinal data using Reproducing Kernel Hilbert Spaces.
#' Approximate each curve with a smooth function using a kernel function.
#' 
#' With this function each function can be represented with a vector in R^d.
#' 
#' @param curves a data matrix with observations (curves) in rows and the
#' discretizations points in columns.
#' @param rk kernek function rk object.
#' @param gamma regularization parameter. Defaoult value = 1e-5.
#' @return \item{data}{input curves.} \item{fdata}{smoothed curves.}
#' \item{lambda}{coefficients of the (stable) and d dimensional RKHS
#' representation.} \item{alpha}{coefficients of the RKHS expansion.}
#' \item{gamma}{regularization parameter.}
#' @author N. Hernández and J. Cugliari
#' @references A. Muñoz, J. González, Representing functional data using
#' support vector machines, Pattern Recognition Letters 31 (2010) 511–516. <doi:10.1016/j.patrec.2009.07.014>.
#' @export
#' @examples
#' 
#' t = 1:50
#' curves = matrix(sin(t)+rnorm(length(t)),nrow=1)
#' f.data <- fdata_rkhs(curves, rk = rk(t,sigma = 0.01))
#' plot(t,curves, xlab='time', ylab='PM10 dataset', col='gray', lty=1, type='b')
#' lines(t,f.data$fdata, col='blue', lty=1)
#' 
fdata_rkhs <-
function(curves, rk, gamma = 1e-5) {  # this will be called as 'fdata'
  p      <- ncol(curves)
  alpha  <- curves %*% solve(diag(gamma, p) + rk$K)
  lambda <- alpha %*% rk$U %*% sqrt(rk$D)
  fdata  <- lambda %*% t(rk$U %*% sqrt(rk$D))
  return(list(data   = curves,
              rk     = rk,
              alpha  = alpha,
              lambda = lambda,
              gamma  = gamma,
              fdata  = fdata))
}
