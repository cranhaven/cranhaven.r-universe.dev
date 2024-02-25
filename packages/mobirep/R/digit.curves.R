#' Creates homogeneous level curve for every model
#'
#' Creates homogeneous level curve for every model (adapted from the function digit.curves
#' of the geomorph R package)
#'
#' @param start A numeric vector of x,y, coordinates for the landmark defining the start of the curve
#' @param curve A matrix (p x k) of 2D coordinates for a set of ordered points defining a curve
#' @param nPoints Numeric how many semilandmarks to place equidistantly along the curve (not counting beginning and end points)
#' @param closed Logical Whether the curve is closed (TRUE) or open (FALSE)
#' @export
#' @seealso \code{\link[geomorph]{digit.curves}}
#' @return A matrix of coordinates for nPoints equally spaced semilandmarks sampled along the curve
#' @references Bookstein, F. J. 1997 Landmark Methods for Forms without Landmarks: Morphometrics of
#' Group Differences in Outline Shape. Medical Image Analysis 1(3):225-243.
#' @references Rohlf, F.J., 2015. The tps series of software. Hystrix 26(1):9-12.
#' @examples
#' # Curve  creation
#' x<-seq(0,30, length=200)
#' y=90-x^2
#' curve=data.frame(x,y)
#'
#' # Homogenization of the number of points
#' ltl<-digit.curves.p(start=curve[1,], curve=as.matrix(curve), nPoints=98, closed = FALSE)
#' plot(ltl)

digit.curves.p <- function(start, curve, nPoints, closed=TRUE){
  nPoints <- nPoints+2
  if(!is.matrix(curve)) stop("Input must be a p-x-k matrix of curve coordinates")
  nCurvePoints = NROW(curve)
  if(nCurvePoints < 2) stop("curve matrix does not have enough points to estimate any interior points")
  if(nPoints > (nCurvePoints - 1)) {
    if((nCurvePoints - 1) == 1) nPoints = 1
    if((nCurvePoints - 1) > 1) nPoints = nCurvePoints - 2
    message("\nWarning: because the number of desired points exceeds the number of curve points,")
    message("\nthe number of points will be truncated to", nPoints, "\n\n")
  }
  start <- as.numeric(start)
  if(!setequal(start, curve[1,])) curve <- rbind(start, curve)
  if(closed) curve <- rbind(curve, curve[1,])
  res <- evenPts.p(curve, nPoints)
  if(closed) res <- res[-NROW(res),]
  res
}


#' Spaces out curve points via linear interpolation
#'
#' Basic function for spacing out curve points via linear interpolation (adapted from the function digit.curves
#' of the geomorph package). The main different is that curves are normalized to allow an intercomaprison of confidence scores
#' regardless of the input data.
#' used in digit.curves.p
#' @param x,n numeric vectors
#' @return A matrix of coordinates for nPoints equally spaced
#' semilandmarks sampled along the curve in a normalized space
evenPts.p <- function(x, n){
  x <- as.matrix(na.omit(x))
  at1<-scale(x[,1])
  at2<-scale(x[,2])
  x[,1]<-as.vector(scale(x[,1]))
  x[,2]<-as.vector(scale(x[,2]))
  N <- NROW(x); p <- NCOL(x)
  if(N == 1) stop("x must be a matrix")
  if(n < 3) {
    n <- 2
    nn <- 3 # so lapply function works
  } else nn <- n

  if(N == 2) {
    x <- rbind(x, x[2,])
    N <- 3 # third row cut off later
  }
  xx <- x[2:N, ] - x[1:(N - 1), ]
  ds <- as.numeric(sqrt(xx[,1]^2+xx[,2]^2))
  cds <- c(0, cumsum(ds))
  cuts <- cumsum(rep(cds[N]/(n-1), n-1))
  targets <- lapply(1:(nn-2), function(j){
    dtar <- cuts[j]
    ll <- which.max(cds[cds < dtar])
    ul <- ll + 1
    adj <- (dtar- cds[ll])/(cds[[ul]] - cds[ll])
    x[ll,] + adj * (x[ul,] - x[ll,])
  })

  out <- matrix(c(x[1,], unlist(targets), x[N,]), n, p, byrow = TRUE)
  out[,1]<- out[,1]*attr(at1,'scaled:scale')+attr(at1, 'scaled:center')
  out[,2]<-out[,2]*attr(at2,'scaled:scale')+attr(at2, 'scaled:center')
  out
}
