\encoding{latin1}
\name{k12val}
\alias{k12val}
\alias{print.k12val}
\alias{summary.k12val}
\alias{print.summary.k12val}
\title{Multiscale local second-order neighbour density of a bivariate spatial point pattern}
\description{
 Computes local second-order neighbour density estimates for a bivariate spatial point pattern, i.e. the number of neighbours of type 2 per unit area
  within sample circles of regularly increasing radii \eqn{r}, centred at each type 1 point of the pattern (see Details). 
}
\usage{
 k12val(p, upto, by, marks)
}
\arguments{
  \item{p}{a \code{"spp"} object defining a multivariate spatial point pattern in a given sampling window (see \code{\link{spp}}).}
  \item{upto }{maximum radius of the sample circles (see Details).}
  \item{by }{interval length between successive sample circles radii (see Details).}
  \item{marks}{by default \code{c(1,2)}, otherwise a vector of two numbers or character strings identifying the types (the \code{p$marks} levels)
  of points of type 1 and 2, respectively.}
}
\details{
  Function \code{K12val} returns individual values of \emph{K12(r)} and associated functions (see \code{\link{k12fun}})
 estimated at each type 1 point of the pattern. For a given distance \emph{r}, these values can be mapped within the sampling window, as in
 Getis & Franklin 1987 or P?Pelissier & Goreaud 2001. 
}
\value{
 A list of class \code{c("vads","k12val")} with essentially the following components:
 \item{r }{a vector of regularly spaced distances (\code{seq(by,upto,by)}).}
 \item{xy }{a data frame with 2 components giving \eqn{(x,y)} coordinates of type 1 points of the pattern.}
 \item{g12val }{a matrix of size \eqn{(length(xy),length(r))} giving individual values of the bivariate pair density function \eqn{g12(r)}.}
 \item{n12val }{a matrix of size \eqn{(length(xy),length(r))} giving individual values of the bivariate neighbour density function \eqn{n12(r)}.}
 \item{k12val }{a matrix of size \eqn{(length(xy),length(r))} giving individual values of the intertype function \eqn{K12(r)}.}
 \item{l12val }{a matrix of size \eqn{(length(xy),length(r))} giving individual values the modified intertype function \eqn{L12(r)}.}
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
\seealso{ 
	\code{\link{plot.vads}},
	\code{\link{k12fun}},
	\code{\link{dval}},
	\code{\link{kval}}.
}
\examples{
  data(BPoirier)
  BP <- BPoirier
  \dontrun{spatial point pattern in a rectangle sampling window of size [0,110] x [0,90]}
  swrm <- spp(BP$trees, win=BP$rect, marks=BP$species)
  k12vswrm <- k12val(swrm, 25, 1, marks=c("beech","oak"))
  summary(k12vswrm)
  plot(k12vswrm)
 
  \dontrun{spatial point pattern in a circle with radius 50 centred on (55,45)}
  swc <- spp(BP$trees, win=c(55,45,45), marks=BP$species)
  k12vswc <- k12val(swc, 25, 1, marks=c("beech","oak"))
  summary(k12vswc)
  plot(k12vswc)
  
  \dontrun{spatial point pattern in a complex sampling window}
  swrt <- spp(BP$trees, win=BP$rect, tri=BP$tri2, marks=BP$species)
  k12vswrt <- k12val(swrt, 25, 1, marks=c("beech","oak"))
  summary(k12vswrt)
  plot(k12vswrt)
}
\keyword{spatial}
