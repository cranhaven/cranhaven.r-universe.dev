\encoding{latin1}
\name{kval}
\alias{kval}
\alias{print.kval}
\alias{summary.kval}
\alias{print.summary.kval}
\title{Multiscale local second-order neighbour density of a spatial point pattern}
\description{
 Computes local second-order neighbour density estimates for an univariate spatial point pattern, i.e. the number of neighbours per unit area
  within sample circles of regularly increasing radii \eqn{r}, centred at each point of the pattern (see Details). 
}
\usage{
  kval(p, upto, by)
}
\arguments{
  \item{p}{a \code{"spp"} object defining a spatial point pattern in a given sampling window (see \code{\link{spp}}).}
  \item{upto }{maximum radius of the sample circles (see Details).}
  \item{by }{interval length between successive sample circles radii (see Details).}
}
\details{
 Function \code{kval} returns individual values of \emph{K(r)} and associated functions (see \code{\link{kfun}})
 estimated for each point of the pattern. For a given distance \emph{r}, these values can be mapped within the sampling window 
 (Getis & Franklin 1987, P?Pelissier & Goreaud 2001).  
}
\value{
A list of class \code{c("vads","kval")} with essentially the following components:
 \item{r }{a vector of regularly spaced out distances (\code{seq(by,upto,by)}).}
 \item{xy }{a data frame with 2 components giving \eqn{(x,y)} coordinates of points of the pattern.}
 \item{gval }{a matrix of size \eqn{(length(xy),length(r))} giving individual values of the pair density function \eqn{g(r)}.}
 \item{nval }{a matrix of size \eqn{(length(xy),length(r))} giving individual values of the neighbour density function \eqn{n(r)}.}
 \item{kval }{a matrix of size \eqn{(length(xy),length(r))} giving individual values of Ripley's function \eqn{K(r)}.}
 \item{lval }{a matrix of size \eqn{(length(xy),length(r))} giving individual values the modified Ripley's function \eqn{L(r)}.}
 }
\references{ 
  Getis, A. and Franklin, J. 1987. Second-order neighborhood analysis of mapped point patterns. \emph{Ecology}, 68:473-477.\cr\cr
  P?Pelissier, R. and Goreaud, F. 2001. A practical approach to the study of spatial structure in simple cases of heterogeneous vegetation. \emph{Journal of Vegetation Science}, 12:99-108.
}
\author{
	\email{Raphael.Pelissier@ird.fr}
}
\note{
 There are printing, summary and plotting methods for \code{"vads"} objects.
}
\section{Warning }{
  Function \code{kval} ignores the marks of multivariate and marked point patterns (they are all considered to be univariate patterns).
}
\seealso{
	\code{\link{plot.vads}},
	\code{\link{kfun}},
	\code{\link{dval}},
	\code{\link{k12val}}.
}
\examples{
  data(BPoirier)
  BP <- BPoirier
  \dontrun{spatial point pattern in a rectangle sampling window of size [0,110] x [0,90]}
  swr <- spp(BP$trees, win=BP$rect)
  kvswr <- kval(swr, 25, 1)
  summary(kvswr)
  plot(kvswr)

  \dontrun{spatial point pattern in a circle with radius 50 centred on (55,45)}
  swc <- spp(BP$trees, win=c(55,45,45))
  kvswc <- kval(swc, 25, 1)
  summary(kvswc)
  plot(kvswc)
  
  \dontrun{spatial point pattern in a complex sampling window}
  swrt <- spp(BP$trees, win=BP$rect, tri=BP$tri1)
  kvswrt <- kval(swrt, 25, 1)
  summary(kvswrt)
  plot(kvswrt)
}
\keyword{spatial}
