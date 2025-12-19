\encoding{latin1}
\name{ksfun}
\alias{ksfun}
\title{Multiscale second-order neighbourhood analysis of a multivariate spatial point pattern using Simpson diversity}
\description{
  Computes estimates of Shimatani \emph{alpha} and \emph{beta} functions of Simpson diversity from a multivariate spatial point pattern 
  in a simple (rectangular or circular) or complex sampling window. Computes optionally local confidence limits of the functions
  under the null hypothesis of a random allocation of species labels (see Details).
}
\usage{
ksfun(p, upto, by, nsim=0, alpha=0.01)
}
\arguments{
  \item{p }{a \code{"spp"} object defining a spatial point pattern in a given sampling window (see \code{\link{spp}}).}
  \item{upto }{maximum radius of the sample circles (see Details).}
  \item{by }{interval length between successive sample circles radii (see Details).}
  \item{nsim }{number of Monte Carlo simulations to estimate local confidence limits of the null hypothesis of a random allocation of species labels (see Details).
  By default \code{nsim=0}, so that no confidence limits are computed.}
  \item{alpha }{if \code{nsim>0}, significant level of the confidence limits. By default \eqn{\alpha=0.01}.}
}
\details{
 Function \code{ksfun} computes Shimatani \eqn{\alpha(r)} and \eqn{\beta(r)} functions of Simpson diversity, called here \eqn{Ks(r)} and \eqn{gs(r)}, respectively.\cr\cr
 For a multivariate point pattern consisting of \eqn{S} species with intensity \eqn{\lambda}p, Shimatani (2001) showed that
  a distance-dependent measure of Simpson (1949) diversity can be estimated from Ripley (1977) \eqn{K}-function computed for each species separately and for all the points grouped together (see also Eckel et al. 2008).
  Function \code{ksfun} is thus a simple wrapper function of \code{\link{kfun}}, standardized by Simpson diversity coefficient:
  
  
  \eqn{Ks(r) = 1 - sum(\lambda p * \lambda p * Kp(r)) / (\lambda * \lambda * K(r) * D)} which is a standardized estimator of \eqn{\alpha(r)} in Shimatani (2001).\cr\cr
  \eqn{gs(r) = 1 - sum(\lambda p * \lambda p * gp(r)) / (\lambda * \lambda * g(r) * D)} corresponding to a standardized version of \eqn{\beta(r)} in Shimatani (2001).\cr\cr
  
  \eqn{Kp(r)} and \eqn{K(r)} (resp. \eqn{gp(r)} and \eqn{g(r)}) are univariate K-functions computed for species \eqn{p} and for all species together; \eqn{D = 1 - sum(Np * (Np - 1) / (N*(N - 1)))} is the unbiased version of Simpson diversity,
  with \eqn{Np} the number of individuals of species \eqn{p} in the sample and \eqn{N = sum(Np)}.

  The program introduces an edge effect correction term according to the method proposed by Ripley (1977)
  and extended to circular and complex sampling windows by Goreaud & P?Pelissier (1999).\cr\cr
 
  The theoretical values of \eqn{gr(r)} and \eqn{Kr(r)} under the null hypothesis of random labelling is 1 for all \eqn{r}.
  Local Monte Carlo confidence limits and p-values of departure from this hypothesis are estimated at each distance \eqn{r} by reallocating at random the species labels among points of the pattern, keeping the point locations unchanged.
}
\value{
 A list of class \code{"fads"} with essentially the following components:
  \item{r }{a vector of regularly spaced out distances (\code{seq(by,upto,by)}).}
  \item{gs }{a data frame containing values of the function \eqn{gs(r)}.}
  \item{ks }{a data frame containing values of the function \eqn{Ks(r)}.\cr}
  Each component except \code{r} is a data frame with the following variables:\cr
\item{obs }{a vector of estimated values for the observed point pattern.}
 \item{theo }{a vector of theoretical values expected under the null hypothesis of random labelling, i.e. 1 for all \eqn{r}.}
 \item{sup }{(optional) if \code{nsim>0} a vector of the upper local confidence limits of a random distribution of species labels at a significant level \eqn{\alpha}.}
 \item{inf }{(optional) if \code{nsim>0} a vector of the lower local confidence limits of a random distribution of species labels at a significant level \eqn{\alpha}.}
 \item{pval }{(optional) if \code{nsim>0} a vector of local p-values of departure from a random distribution of species labels.}
}
\references{
 Shimatani K. 2001. Multivariate point processes and spatial variation in species diversity. \emph{Forest Ecology and Management}, 142:215-229.

 Eckel, S., Fleisher, F., Grabarnik, P. and Schmidt V. 2008. An investigation of the spatial correlations for relative purchasing power in Baden-Wurttemberg. \emph{AstA - Advances in Statistical Analysis}, 92:135-152.
 
 Simpson, E.H. 1949. Measurement of diversity. \emph{Nature}, 688:163.
 
 Goreaud F. & P?Pelissier R. 1999. On explicit formulas of edge effect correction for Ripley's K-function. \emph{Journal of Vegetation Science}, 10:433-438.

 Ripley B.D. 1977. Modelling spatial patterns. \emph{Journal of the Royal Statistical Society B}, 39:172-192.
}
\author{\email{Raphael.Pelissier@ird.fr}}
\note{
  There are printing and plotting methods for \code{"fads"} objects.
}
\seealso{
\code{\link{plot.fads}},
 \code{\link{spp}},
 \code{\link{kfun}},
  \code{\link{kpqfun}},
 \code{\link{kp.fun}},
 \code{\link{krfun}}.
}
\examples{
  data(Paracou15)
  P15<-Paracou15
  \dontrun{spatial point pattern in a rectangle sampling window of size 125 x 125}
  swmr <- spp(P15$trees, win = c(125, 125, 250, 250), marks = P15$species)
  kswmr <- ksfun(swmr, 50, 5, 500)
  plot(kswmr)

  \dontrun{spatial point pattern in a circle with radius 50 centred on (125,125)}
  swmc <- spp(P15$trees, win = c(125, 125, 50), marks = P15$species)
  kswmc <- ksfun(swmc, 50, 5, 500)
  plot(kswmc)
  
  \dontrun{spatial point pattern in a complex sampling window}
  swrt <- spp(P15$trees, win = c(125, 125, 250, 250), tri=P15$tri, marks=P15$species)
  kswrt <- ksfun(swrt, 50, 5, 500)
  plot(kswrt)
}
\keyword{spatial}
