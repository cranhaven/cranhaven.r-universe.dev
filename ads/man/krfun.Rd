\encoding{latin1}
\name{krfun}
\alias{krfun}
\title{Multiscale second-order neighbourhood analysis of a multivariate spatial point pattern using Rao quadratic entropy}
\description{
  Computes distance-dependent estimates of Rao's quadratic entropy from a multivariate spatial point pattern 
  in a simple (rectangular or circular) or complex sampling window. Computes optionally local confidence limits of the functions
  under the null hypothesis of either a random labelling or a species equivalence (see Details).
}
\usage{
krfun(p, upto, by, nsim=0, dis = NULL, H0 = c("rl", "se"), alpha = 0.01)
}
\arguments{
  \item{p }{a \code{"spp"} object defining a spatial point pattern in a given sampling window (see \code{\link{spp}}).}
  \item{upto }{maximum radius of the sample circles (see Details).}
  \item{by }{interval length between successive sample circles radii (see Details).}
  \item{nsim }{number of Monte Carlo simulations to estimate local confidence limits of the null hypothesis of a random allocation of species labels (see Details).
  By default \code{nsim = 0}, so that no confidence limits are computed.}
  \item{dis }{(optional) a \code{"dist"} object defining Euclidean distances between species. By default \eqn{dis = NULL} so that species are considered equidistant.}
\item{H0}{one of c("rl","se") to select either the null hypothesis of random labelling (H0 = "rl") or species equivalence (H0 = "se") (see Details). By default, the null hypothesis is random labelling.}
  \item{alpha }{if \code{nsim>0}, significant level of the confidence limits. By default \eqn{\alpha = 0.01}.}
}
\details{
 Function \code{krfun} computes distance-dependent functions of Rao (1982) quadratic entropy (see \code{\link[ade4]{divc}} in package \code{ade4}).\cr\cr
 For a multivariate point pattern consisting of \eqn{S} species with intensity \eqn{\lambda}p, such functions can be estimated from the bivariate \eqn{Kpq}-functions between each pair of different species \eqn{p} and \eqn{q}.
 Function \code{krfun} is thus a simple wrapper function of \code{\link{k12fun}} and \code{\link{kfun}}, standardized by Rao diversity coefficient (Pelissier & Goreaud 2014):

  \eqn{Kr(r) = sum(\lambda p * \lambda q * Kpq(r)*dpq) / (\lambda * \lambda * K(r) * HD)}.\cr
  \eqn{gr(r) = sum(\lambda p * \lambda q * gpq(r)*dpq) / (\lambda * \lambda * g(r) * HD)}.\cr\cr

  where \eqn{dpq} is the distance between species \eqn{p} and \eqn{q} defined by matrix \code{dis}, typically a taxonomic, phylogenetic or functional distance, and \eqn{HD=sum(Np*Nq*dpq/(N(N - 1)))} is the unbiased version of Rao diversity coefficient (see Shimatani 2001). When \code{dis = NULL}, species are considered each other equidistant and \code{krfun} returns the same results than \code{\link{ksfun}}. 

The program introduces an edge effect correction term according to the method proposed by Ripley (1977)
  and extended to circular and complex sampling windows by Goreaud & Pelissier (1999).\cr\cr
 
Theoretical values under the null hypothesis of either random labelling or species equivalence as well as local Monte Carlo confidence limits and p-values of departure from the null hypothesis (Besag & Diggle 1977) are estimated at each distance \eqn{r}.

The random labelling hypothesis (H0 = "rl") is tested by reallocating species labels at random among all points of the pattern, keeping the point locations unchanged, so that expectations of \eqn{gr(r)} and \eqn{Kr(r)} are 1 for all \eqn{r}.
The species equivalence hypothesis (H0 = "se") is tested by randomizing the between-species distances, keeping the point locations and distribution of species labels unchanged. The theoretical expectations of \eqn{gr(r)} and \eqn{Kr(r)} are thus \eqn{gs(r)} and \eqn{Ks(r)}, respectively (see \code{\link{ksfun}}).

}
\value{
 A list of class \code{"fads"} with essentially the following components:
  \item{r }{a vector of regularly spaced out distances (\code{seq(by,upto,by)}).}
  \item{gr }{a data frame containing values of the function \eqn{gr(r)}.}
  \item{kr }{a data frame containing values of the function \eqn{Kr(r)}.\cr}
  Each component except \code{r} is a data frame with the following variables:\cr
\item{obs }{a vector of estimated values for the observed point pattern.}
 \item{theo }{a vector of theoretical values expected under the selected null hypothesis.}
 \item{sup }{(optional) if \code{nsim>0} a vector of the upper local confidence limits of a random distribution of the selected null hypothesis at a significant level \eqn{\alpha}.}
 \item{inf }{(optional) if \code{nsim>0} a vector of the lower local confidence limits of a random distribution of the selected null hypothesis at a significant level \eqn{\alpha}.}
 \item{pval }{(optional) if \code{nsim>0} a vector of local p-values of departure from the selected null hypothesis.}
}
\references{
 Rao, C.R. 1982. Diversity and dissimilarity coefficient: a unified approach. \emph{Theoretical Population Biology}, 21:24-43.

 Shimatani, K. 2001. On the measurement of species diversity incorporating species differences. \emph{Oikos}, 93, 135-147.
 
 Goreaud F. & Pelissier R. 1999. On explicit formulas of edge effect correction for Ripley's K-function. \emph{Journal of Vegetation Science}, 10:433-438.

 Ripley B.D. 1977. Modelling spatial patterns. \emph{Journal of the Royal Statistical Society B}, 39:172-192.
 
 Pelissier, R. & Goreaud, F. 2014. ads package for R: A fast unbiased implementation of the k-function family for studying spatial point patterns in irregular-shaped sampling windows. \emph{Journal of Statistical Software}, in press.
 
}
\author{\email{Raphael.Pelissier@ird.fr}}
\note{
  There are printing and plotting methods for \code{"fads"} objects.
}
\seealso{
\code{\link{plot.fads}},
 \code{\link{spp}},
 \code{\link{ksfun}},
 \code{\link{kdfun}},
 \code{\link[ade4]{divc}}.
}
\examples{
  data(Paracou15)
  P15<-Paracou15
  \dontrun{spatial point pattern in a rectangle sampling window of size 125 x 125}
  swmr <- spp(P15$trees, win = c(175, 175, 250, 250), marks = P15$species)
  \dontrun{testing the random labeling hypothesis}
  krwmr.rl <- krfun(swmr, dis = P15$spdist, H0 = "rl", 25, 2, 50)
  \dontrun{running more simulations is slow}
  \donttest{krwmr.rl <- krfun(swmr, dis = P15$spdist, H0 = "rl", 25, 2, 500)}
  plot(krwmr.rl)
  \dontrun{testing the species equivalence hypothesis}
  krwmr.se <- krfun(swmr, dis = P15$spdist, H0 = "se", 25, 2, 50)
  \dontrun{running more simulations is slow}
  \donttest{krwmr.se <- krfun(swmr, dis = P15$spdist, H0 = "se", 25, 2, 500)}
  plot(krwmr.se)

  \dontrun{spatial point pattern in a circle with radius 50 centred on (125,125)}
  swmc <- spp(P15$trees, win = c(125,125,50), marks = P15$species)
  krwmc <- krfun(swmc, dis = P15$spdist, H0 = "rl", 25, 2, 100)
  \dontrun{running more simulations is slow}
  \donttest{krwmc <- krfun(swmc, dis = P15$spdist, H0 = "rl", 25, 2, 500)}
  plot(krwmc)
  
  \dontrun{spatial point pattern in a complex sampling window}
  swrt <- spp(P15$trees, win = c(125,125,250,250), tri = P15$tri, marks = P15$species)
  krwrt <- krfun(swrt, dis = P15$spdist, H0 = "rl", 25, 2)
  \dontrun{running simulations is slow}
  \donttest{krwrt <- krfun(swrt, dis = P15$spdist, H0 = "rl", 25, 2, 500)}
  plot(krwrt)
}
\keyword{spatial}
