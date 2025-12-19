\encoding{latin1}
\name{spp}
\alias{spp}
\alias{print.spp}
\alias{summary.spp}
\alias{print.summary.spp}
\alias{ppp2spp}
\title{Creating a spatial point pattern}
\description{
  Function \code{spp} creates an object of class \code{"spp"}, which represents a
  spatial point pattern observed in a finite sampling window (or study region).
  The \code{ads} library supports univariate, multivariate and marked point patterns
  observed in simple (rectangular or circular) or complex sampling windows. 
}
\usage{
spp(x, y=NULL, window, triangles, marks, int2fac=TRUE)
ppp2spp(p)
}
\arguments{
  \item{x,y}{if \code{y=NULL}, \eqn{x} is a list of two vectors of point coordinates, else both \eqn{x} and \eqn{y} are atomic vectors of point coordinates.}
  \item{window}{a \code{"swin"} object or a vector defining the limits of a simple sampling 
  window: \code{c(xmin,ymin,xmax,ymax)} for a rectangle ; \code{c(x0,y0,r0)} for a circle.}
  \item{triangles}{(optional) a list of triangles removed from a simple initial window to define a 
  complex sampling window (see \code{\link{swin}}).}
  \item{marks}{(optional) a vector of mark values, which may be factor levels or numerical values (see Details).}
  \item{int2fac}{if TRUE, integer marks are automatically coerced into factor levels.}
  \item{p}{a \code{"ppp"} object from package \code{spatstat.geom}.}
}
\details{
A spatial point pattern is assumed to have been observed within a specific
  sampling window (a finite study region) defined by the \code{window} argument. If \code{window} is a simple \code{"swin"} object, 
  it may be coerced into a complex type by adding a \code{triangles} argument (see \code{\link{swin}}). A spatial point pattern may be of 3 different types.
	\itemize{
	\item
	\bold{univariate pattern:}
    by default when argument \code{marks} is not given.
	\item
	\bold{multivariate pattern:}
    \code{marks} is a factor, which levels are interpreted as categorical marks (e.g. colours, species, etc.) attached to points of the pattern.
	Integer marks may be automatically coerced into factor levels when argument \code{int2fac = TRUE}.
	\item
	\bold{marked pattern:}
    \code{marks} is a vector of real numbers attached to points of the pattern. Integer values may also be considered as numerical values 
	if argument \code{int2fac = FALSE}.
	}
}
\value{
	An object of class \code{"spp"} describing a spatial point pattern observed in a given sampling window.
  \item{\code{$type}}{a character string indicating if the spatial point pattern is \code{"univariate"}, \code{"multivariate"} or \code{"marked"}.}
  \item{\code{$window}}{an \code{swin} object describing the sampling window (see \code{\link{swin}}).}
  \item{\code{$n}}{an integer value giving the number of points of the pattern located inside the sampling window (points on the boundary are considered to be inside).}
  \item{\code{$x}}{a vector of \eqn{x} coordinates of points located inside the sampling window.}
  \item{\code{$y}}{a vector of \eqn{y} coordinates of points located inside the sampling window.}
  \item{\code{$nout}}{(optional) an integer value giving the number of points of the pattern located outside the sampling window.}
  \item{\code{$xout}}{(optional) a vector of \eqn{x} coordinates of points located outside the sampling window.}
  \item{\code{$yout}}{(optional) a vector of \eqn{y} coordinates of points located outside the sampling window.}
  \item{\code{$marks}}{(optional) a vector of the marks attached to points located inside the sampling window.}
  \item{\code{$marksout}}{(optional) a vector of the marks attached to points located outside the sampling window.}
}
\references{
 Goreaud, F. and P?Pelissier, R. 1999. On explicit formula of edge effect correction for Ripley's \emph{K}-function. \emph{Journal of Vegetation Science}, 10:433-438.
}
\note{
There are printing, summary and plotting methods for \code{"spp"} objects.\cr
Function \code{ppp2spp} converts an \code{\link[spatstat.geom]{ppp.object}} from package \code{spatstat.geom} into an \code{"spp"} object.
}
\seealso{
  \code{\link{plot.spp}},
  \code{\link{swin}}
  }
\author{
 \email{Raphael.Pelissier@ird.fr}
}
\examples{
	data(BPoirier)
	BP <- BPoirier
	\dontrun{univariate pattern in a rectangle of size [0,110] x [0,90]}
	swr <- spp(BP$trees, win=BP$rect)
	\dontrun{an alternative using atomic vectors of point coordinates}
	swr <- spp(BP$trees, win=BP$rect) 
	summary(swr)
	plot(swr)
	
	\dontrun{univariate pattern in a circle with radius 50 centred on (55,45)}
	swc <- spp(BP$trees, win=c(55,45,50))
	summary(swc)
	plot(swc)
	plot(swc, out=TRUE) # plot points outside the circle

	\dontrun{multivariate pattern in a rectangle of size [0,110] x [0,90]}
	swrm <- spp(BP$trees, win=BP$rect, marks=BP$species)
	summary(swrm)
	plot(swrm)
	plot(swrm, chars=c("b","h","o")) # replace symbols by letters
	
	\dontrun{marked pattern in a rectangle of size [0,110] x [0,90]}
	swrn <- spp(BP$trees, win=BP$rect, marks=BP$dbh)
	summary(swrn)
	plot(swrn)
	
	\dontrun{multivariate pattern in a complex sampling window}
	swrt <- spp(BP$trees, win=BP$rect, tri=BP$tri1, marks=BP$species)
	summary(swrt)
	plot(swrt)
	plot(swrt, out=TRUE) # plot points outside the sampling window
	
	
	\dontrun{converting a ppp object from spatstat.geom}
	data(demopat)
	demo.spp<-ppp2spp(demopat)
	plot(demo.spp)
}
\keyword{spatial}
