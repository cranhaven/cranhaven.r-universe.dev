\encoding{latin1}
\name{kdfun}
\alias{kdfun}
\title{Multiscale second-order neighbourhood analysis of a spatial phylogenetic or functional community pattern from fully mapped data}
\description{
  Computes distance-dependent estimates of Shen et al. (2014) phylogenetic or functional mark correlation functions from a multivariate spatial point pattern 
  in a simple (rectangular or circular) or complex sampling window. Computes optionally local confidence limits of the functions
  under the null hypothesis of species equivalence (see Details).
}
\usage{
kdfun(p, upto, by, dis, nsim=0, alpha = 0.01)
}
\arguments{
  \item{p }{a \code{"spp"} object defining a spatial point pattern in a given sampling window (see \code{\link{spp}}).}
  \item{upto }{maximum radius of the sample circles (see Details).}
  \item{by }{interval length between successive sample circles radii (see Details).}
  \item{dis }{a \code{"dist"} object defining Euclidean distances between species.}
  \item{nsim }{number of Monte Carlo simulations to estimate local confidence limits of the null hypothesis of a random allocation of species distances (species equivalence; see Details).
  By default \code{nsim = 0}, so that no confidence limits are computed.}
  \item{alpha }{if \code{nsim>0}, significant level of the confidence limits. By default \eqn{\alpha = 0.01}.}
}
\details{
 Function \code{kdfun} computes Shen et al. (2014) \eqn{Kd} and \emph{gd}-functions. For a multivariate point pattern consisting of \eqn{S} species with intensity \eqn{\lambda}p, such functions can be estimated from the bivariate \eqn{Kpq}-functions between each pair of different species \eqn{p} and \eqn{q}.
 Function \code{kdfun} is thus a simple wrapper of \code{\link{k12fun}} (P?Pelissier & Goreaud 2014):

  \eqn{Kd(r) = D * Kr(r) / HD * Ks(r) = D * sum(\lambda p * \lambda q * Kpq(r) * dpq) / HD * sum(\lambda p * \lambda q * Kpq(r))}.\cr
  \eqn{gd(r) = D * g(r) / HD * gs(r) = D * sum(\lambda p * \lambda q * gpq(r) * dpq) / HD * sum(\lambda p * \lambda q * gpq(r))}.\cr\cr

  where \eqn{Ks(r)} and \eqn{gs(r)} are distance-dependent versions of Simpson's diversity index, \eqn{D} (see \code{\link{ksfun}}), \eqn{Kr(r)} and \eqn{gr(r)} are distance-dependent versions of Rao's diversity coefficient (see \code{\link{krfun}}); 
  \eqn{dpq} is the distance between species \eqn{p} and \eqn{q} defined by matrix \code{dis}, typically a taxonomic, phylogenetic or functional distance. The advantage here is that as the edge effects vanish between \eqn{Kr(r)} and \eqn{Ks(r)},
   implementation is fast for a sampling window of any shape. \eqn{Kd(r)} provides the expected phylogenetic or functional distance of two heterospecific individuals a distance less than \emph{r} apart (Shen et al. 2014), while \eqn{gd(r)} 
   provides the same within an annuli between two consecutive distances of \emph{r} and \emph{r-by}.
   
	Theoretical values under the null hypothesis of species equivalence as well as local Monte Carlo confidence limits and p-values of departure from the null hypothesis (Besag & Diggle 1977) are estimated at each distance \eqn{r}, 
	by randomizing the between-species distances, keeping the point locations and distribution of species labels unchanged. The theoretical expectations of \eqn{gd(r)} and \eqn{Kd(r)} are thus \eqn{1}.

}
\value{
 A list of class \code{"fads"} with essentially the following components:
  \item{r }{a vector of regularly spaced out distances (\code{seq(by,upto,by)}).}
  \item{gd }{a data frame containing values of the function \eqn{gd(r)}.}
  \item{kd }{a data frame containing values of the function \eqn{Kd(r)}.\cr}
  Each component except \code{r} is a data frame with the following variables:\cr
\item{obs }{a vector of estimated values for the observed point pattern.}
 \item{theo }{a vector of theoretical values expected under the null hypothesis of species equivalence.}
 \item{sup }{(optional) if \code{nsim>0} a vector of the upper local confidence limits of a random distribution of the null hypothesis at a significant level \eqn{\alpha}.}
 \item{inf }{(optional) if \code{nsim>0} a vector of the lower local confidence limits of a random distribution of the null hypothesis at a significant level \eqn{\alpha}.}
 \item{pval }{(optional) if \code{nsim>0} a vector of local p-values of departure from the null hypothesis.}
}
\references{
 Shen, G., Wiegand, T., Mi, X. & He, F. (2014). Quantifying spatial phylogenetic structures of fully stem-mapped plant communities. \emph{Methods in Ecology and Evolution}, 4, 1132-1141.
 
 P?Pelissier, R. & Goreaud, F. ads package for R: A fast unbiased implementation of the K-function family for studying spatial point patterns in irregular-shaped sampling windows. \emph{Journal of Statistical Software}, in press.
 
}
\author{\email{Raphael.Pelissier@ird.fr}}
\note{
  There are printing and plotting methods for \code{"fads"} objects.
}
\seealso{
\code{\link{plot.fads}},
 \code{\link{spp}},
 \code{\link{ksfun}},
 \code{\link{krfun}},
 \code{\link[ade4]{divc}}.
}
\examples{
  data(Paracou15)
  P15<-Paracou15
  \dontrun{spatial point pattern in a rectangle sampling window of size 125 x 125}
  swmr <- spp(P15$trees, win = c(175, 175, 250, 250), marks = P15$species)
  \dontrun{testing the species equivalence hypothesis}
  kdswmr <- kdfun(swmr, dis = P15$spdist, 50, 2, 100)
  \dontrun{running more simulations is slow}
  \donttest{kdswmr <- kdfun(swmr, dis = P15$spdist, 50, 2, 500)}
  plot(kdswmr)

 \dontrun{spatial point pattern in a circle with radius 50 centred on (125,125)}
  swmc <- spp(P15$trees, win = c(125,125,50), marks = P15$species)
  kdswmc <- kdfun(swmc, dis = P15$spdist, 50, 2, 100)
  \dontrun{running more simulations is slow}
  \donttest{kdswmc <- kdfun(swmc, dis = P15$spdist, 50, 2, 500)}
  plot(kdswmc)
  
 \dontrun{spatial point pattern in a complex sampling window}
  swrt <- spp(P15$trees, win = c(125,125,250,250), tri = P15$tri, marks = P15$species)
  kdswrt <- kdfun(swrt, dis = P15$spdist, 50, 2, 100)
  \dontrun{running simulations is slow}
  \donttest{kdswrt <- kdfun(swrt, dis = P15$spdist, 50, 2, 500)}
  plot(kdswrt)
}
\keyword{spatial}
