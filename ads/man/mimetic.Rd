\encoding{latin1}
\name{mimetic}
\alias{mimetic}
\alias{plot.mimetic}
\title{Univariate point pattern simulation by mimetic point process}
\description{
  Simulates replicates of an observed univariate point pattern by stochastic optimization of its L-function properties.
}
\usage{
mimetic(x,upto=NULL,by=NULL,prec=NULL,nsimax=3000,conv=50)
}
\arguments{
  \item{x }{either a \code{("fads", "kfun")} object or a \code{"spp"} object of type "univariate" defining a spatial point pattern in a given sampling window (see \code{\link{kfun}} or \code{\link{spp}}).}
  \item{upto }{(optional) maximum radius of the sample circles when \code{x} is a \code{"spp"} object.}
  \item{by }{(optional) interval length between successive sample circles radii when \code{x} is a \code{"spp"} object.}
  \item{prec }{precision of point coordinates generated during simulations when \code{x} is a \code{"spp"} object. By default prec=0.01 or the value used in function \code{kfun} when \code{x} is a \code{("fads", "kfun")} object.}
  \item{nsimax }{maximum number of simulations allowed. By default the process stops after \code{nsimax=3000} if convergence is not reached.}
  \item{conv }{maximum number of simulations without optimization gain (convergence criterion).}
}
\details{
 Function \code{mimetic} uses a stepwise depletion-replacement algorithm to generate a point pattern whose L-function is optimized with regards to an observed one, following the mimetic point process principle (Goreaud et al. 2004).  
 Four points are randomly deleted at each step of the process and replaced by new points that minimize the following cost function:||\eqn{Lobs(r) - Lsim (r)}||)^2. The simulation stops as soon as the cost function doesn't decrease 
 after \code{conv} simulations or after a maximum of \code{nsimax} simulations. The process apply to rectangular, circular or complex sampling windows (see \code{\link{spp}}). There exist a \code{plot} method that displays diagnostic 
 plots, i.e. the observed and simulated L-function, the simulated point pattern and the successive values of the cost function. 
}
\value{
 A list of class \code{"mimetic"} with essentially the following components:
  \item{call }{the function call.}
  \item{fads }{an object of class \code{("fads", "mimetic")} with 2 components:\cr\cr}
  \item{..r }{a vector of regularly spaced out distances corresponding to seq(by,upto,by).}
	\item{..l }{a dataframe with 2 components:\cr\cr}
	\item{.. ..obs}{a vector of values of the L-function estimated for the initial observed pattern}
	\item{.. ..sim}{a vector of values of the L-function estimated for the simulated pattern}
\item{spp }{a object of class \code{"spp"} corresponding to the simulated point pattern (see \code{\link{spp}}).}
 \item{theo }{a vector of theoretical values, i.e. Simpson \eqn{D} for all the points.}
 \item{cost }{a vector of the successive values of the cost function.}
}
\references{
  Goreaud F., Loussier, B., Ngo Bieng, M.-A. & Allain R. 2004. Simulating realistic spatial structure for forest stands: a mimetic point process. In Proceedings of Interdisciplinary Spatial Statistics Workshop, 2-3 December, 2004. Paris, France.}
\author{\email{Raphael.Pelissier@ird.fr}}
\note{
  There are printing and plotting methods for \code{"mimetic"} objects.
}
\seealso{
 \code{\link{spp}},
 \code{\link{kfun}},
}
\examples{
  data(BPoirier)
  BP<-BPoirier
  \dontrun{performing point pattern analysis in a rectangle sampling window}
  swr <- spp(BP$trees, win=BP$rect)
  plot(swr)
  
  \dontrun{performing the mimetic point process from "spp" object}
  mimswr <- mimetic(swr, 20, 2)
  plot(mimswr)

  \dontrun{performing the mimetic point process from "fads" object}
  mimkswr <- mimetic(kfun(swr, 20, 2))
  plot(mimkswr)
  
  }
\keyword{spatial}
