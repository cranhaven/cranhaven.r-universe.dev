\encoding{latin1}
\name{dval}
\alias{dval}
\alias{print.dval}
\alias{summary.dval}
\alias{print.summary.dval}
\title{Multiscale local density of a spatial point pattern}
\description{
  Computes local density estimates of a spatial point pattern, i.e. the number of points per unit area,
  within sample circles of regularly increasing radii \eqn{r}, centred at the nodes of
  a grid covering a simple (rectangular or circular) or complex sampling window (see Details). 
}
\usage{
dval(p, upto, by, nx, ny)
}
\arguments{
  \item{p }{a \code{"spp"} object defining a spatial point pattern in a given sampling window (see \code{\link{spp}}).}
  \item{upto }{maximum radius of the sample circles (see Details).}
  \item{by }{interval length between successive sample circles radii (see Details).}
  \item{nx,ny }{number of sample circles regularly spaced out in \eqn{x} and \eqn{y} directions.}
  }
\details{
 The local density is estimated for a regular sequence of sample circles radii given by \code{seq(by,upto,by)} (see \code{\link{seq}}).
 The sample circles are centred at the nodes of a regular grid with size \eqn{nx} by \eqn{ny}. Ripley's edge effect correction is applied when 
 the sample circles overlap boundary of the sampling window (see Ripley (1977) or Goreaud & P?Pelissier (1999) for an extension to circular and complex 
 sampling windows). Due to edge effect correction, \code{upto}, the maximum radius of the sample circles, is half the longer side for a rectangle sampling
 window (i.e. \eqn{0.5*max((xmax-xmin),(ymax-ymin))}) and the radius \eqn{r0} for a circular sampling window (see \code{\link{swin}}).
}
\value{
 A list of class \code{c("vads","dval")} with essentially the following components:
 \item{r }{a vector of regularly spaced out distances (\code{seq(by,upto,by)}).}
 \item{xy }{a data frame of \eqn{(nx*ny)} observations giving \eqn{(x,y)} coordinates of the centres of the sample circles (the grid nodes).}
 \item{cval }{a matrix of size \eqn{(nx*ny,length(r))} giving the estimated number of points of the pattern per sample circle with radius \eqn{r}.}
 \item{dval }{a matrix of size \eqn{(nx*ny,length(r))} giving the estimated number of points of the pattern per unit area per sample circle with radius \eqn{r}.}
 }
\references{
  Goreaud, F. and P?Pelissier, R. 1999. On explicit formula of edge effect correction for Ripley's \emph{K}-function. \emph{Journal of Vegetation Science}, 10:433-438.\cr\cr
  P?Pelissier, R. and Goreaud, F. 2001. A practical approach to the study of spatial structure in simple cases of heterogeneous vegetation. \emph{Journal of Vegetation Science}, 12:99-108.\cr\cr
  Ripley, B.D. 1977. Modelling spatial patterns. \emph{Journal of the Royal Statistical Society B}, 39:172-212.
}
\author{\email{Raphael.Pelissier@ird.fr}}
\note{
  There are printing, summary and plotting methods for \code{"vads"} objects.
}
 \section{Warning }{
  In its current version, function \code{dval} ignores the marks of multivariate and marked point patterns (they are all considered to be univariate patterns).
}
\seealso{
  \code{\link{plot.vads}},
  \code{\link{spp}}.}
\examples{
  data(BPoirier)
  BP <- BPoirier
  \dontrun{spatial point pattern in a rectangle sampling window of size [0,110] x [0,90]}
  swr <- spp(BP$trees, win=BP$rect)
  dswr <- dval(swr,25,1,11,9)
  summary(dswr)
  plot(dswr)
  
  \dontrun{spatial point pattern in a circle with radius 50 centred on (55,45)}
  swc <- spp(BP$trees, win=c(55,45,45))
  dswc <- dval(swc,25,1,9,9)
  summary(dswc)
  plot(dswc)
  
  \dontrun{spatial point pattern in a complex sampling window}
  swrt <- spp(BP$trees, win=BP$rect, tri=BP$tri1)
  dswrt <- dval(swrt,25,1,11,9)
  summary(dswrt)
  plot(dswrt)
}
\keyword{spatial}
