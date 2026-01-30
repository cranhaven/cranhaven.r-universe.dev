#' Global minimum and maximum of a real-valued continuous function over a closed interval
#' 
#' Gets global minimum and maximum of a given function expression
#' on an interval using basic calculus criteria
#' 
#' @param     f function expression
#' @param f1der function expression of first derivative of \code{f}
#' @param f2der function expression of second derivative of \code{f}
#' @param     D numeric vector specifying the interval over which \code{f}
#'              is optimized
#' 
#' @export
#' 
#' @details This function uses \code{\link[rootSolve]{uniroot.all}}
#' to get all roots of \code{f1der} over \code{D}, additionally,
#' the second derivative criterion is used to determine the global minimum and
#' maximum.
#' 
#' @importFrom rootSolve uniroot.all
#' 
#' @return A list containing:
#' \item{min}{numeric giving critical point where global minimum is achieved}
#' \item{max}{numeric giving critical point where global maximum is achieved}
#' \item{mins}{numeric vector giving all critical points satisfying second 
#' derivative criterion for minimum}
##' \item{maxs}{numeric vector giving all critical points satisfying second 
#' derivative criterion for maximum}
#' 
#' @seealso \code{\link{phenopar}}, \code{\link[rootSolve]{uniroot.all}}
#' 
global_min_max <- function(f, f1der, f2der, D){
  
  crtPts <- uniroot.all(f=f1der, interval= c(D[1],D[length(D)]),
                        tol = .Machine$double.eps)
  
  maxPts <- which(f2der(crtPts)<0)
  
  minPts <- which(f2der(crtPts)>0)
  
  MAX <- crtPts[maxPts[which.max(f(crtPts[maxPts]))]]
  
  MIN <- crtPts[minPts[which.min(f(crtPts[minPts]))]]
  
  list(min=MIN, max=MAX, mins=crtPts[minPts], maxs=crtPts[maxPts])
}
