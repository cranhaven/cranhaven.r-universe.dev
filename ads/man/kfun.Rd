\encoding{latin1}
\name{kfun}
\alias{kfun}
\title{Multiscale second-order neighbourhood analysis of an univariate spatial point pattern}
\description{
  Computes estimates of Ripley's \emph{K}-function and associated neighbourhood functions from an univariate spatial point pattern 
  in a simple (rectangular or circular) or complex sampling window. Computes optionally local confidence limits of the functions
  under the null hypothesis of Complete Spatial Randomness (see Details).
}
\usage{
kfun(p, upto, by, nsim=0, prec=0.01, alpha=0.01)
}
\arguments{
  \item{p }{a \code{"spp"} object defining a spatial point pattern in a given sampling window (see \code{\link{spp}}).}
  \item{upto }{maximum radius of the sample circles (see Details).}
  \item{by }{interval length between successive sample circles radii (see Details).}
  \item{nsim }{number of Monte Carlo simulations to estimate local confidence limits of the null hypothesis of complete spatial randomness (CSR) (see Details).
  By default \code{nsim=0}, so that no confidence limits are computed.}
  \item{prec }{if \code{nsim>0}, precision of points' coordinates generated during simulations. By default \code{prec=0.01}.}
  \item{alpha }{if \code{nsim>0}, significant level of the confidence limits. By default \eqn{\alpha=0.01}.}
}
\details{
 Function \code{kfun} computes Ripley's \eqn{K(r)} function of second-order neighbourhood analysis and the associated functions \eqn{g(r)}, \eqn{n(r)} and \eqn{L(r)}.\cr\cr
 For a homogeneous isotropic point process of intensity \eqn{\lambda}, Ripley (1977) showed that
  the second-order property could be characterized by a function \eqn{K(r)}, so that the expected
  number of neighbours within a distance \eqn{r} of an arbitrary point of the pattern is:
  \eqn{N(r) = \lambda*K(r)}.\cr\cr
  \eqn{K(r)} is a intensity standardization of \eqn{N(r)}, which has an expectation of \eqn{\pi*r^2} under the null hypothesis of CSR: \eqn{K(r) = N(r)/\lambda}.\cr\cr
  \eqn{n(r)} is an area standardization of \eqn{N(r)}, which has an expectation of \eqn{\lambda} under the null hypothesis of CSR: \eqn{n(r) = N(r)/(\pi*r^2)}, where \eqn{\pi*r^2} is the area of the disc of radius \eqn{r}.\cr\cr
  \eqn{L(r)} is a linearized version of \eqn{K(r)} (Besag 1977), which has an expectation of 0 under the null hypothesis of CSR: \eqn{L(r) = \sqrt(K(r)/\pi)-r}. \emph{L(r)} becomes positive when the pattern tends to clustering and negative when it tends to regularity.\cr\cr
  \eqn{g(r)} is the derivative of \eqn{K(r)} or pair density function (Stoyan et al. 1987), so that the expected
  number of neighbours at a distance \eqn{r} of an arbitrary point of the pattern (i.e. within an annuli between two successive circles with radii \eqn{r} and \eqn{r-by}) is:
  \eqn{O(r) = \lambda*g(r)}.\cr\cr
  
  The program introduces an edge effect correction term according to the method proposed by Ripley (1977)
  and extended to circular and complex sampling windows by Goreaud & P?Pelissier (1999).\cr\cr
 
  Theoretical values under the null hypothesis of CSR as well as
  local Monte Carlo confidence limits and p-values of departure from CSR (Besag & Diggle 1977) are estimated at each distance \eqn{r}.
}
\value{
 A list of class \code{"fads"} with essentially the following components:
  \item{r }{a vector of regularly spaced out distances (\code{seq(by,upto,by)}).}
  \item{g }{a data frame containing values of the pair density function \eqn{g(r)}.}
  \item{n }{a data frame containing values of the local neighbour density function \eqn{n(r)}.}
  \item{k }{a data frame containing values of Ripley's function \eqn{K(r)}.}
  \item{l }{a data frame containing values of the modified Ripley's function \eqn{L(r)}.\cr}
  Each component except \code{r} is a data frame with the following variables:\cr
\item{obs }{a vector of estimated values for the observed point pattern.}
 \item{theo }{a vector of theoretical values expected for a Poisson pattern.}
 \item{sup }{(optional) if \code{nsim>0} a vector of the upper local confidence limits of a Poisson pattern at a significant level \eqn{\alpha}.}
 \item{inf }{(optional) if \code{nsim>0} a vector of the lower local confidence limits of a Poisson pattern at a significant level \eqn{\alpha}.}
 \item{pval }{(optional) if \code{nsim>0} a vector of local p-values of departure from a Poisson pattern.}
}
\references{
 Besag J.E. 1977. Discussion on Dr Ripley's paper. \emph{Journal of the Royal Statistical Society B}, 39:193-195.
  
 Besag J.E. & Diggle P.J. 1977. Simple Monte Carlo tests spatial patterns. \emph{Applied Statistics}, 26:327-333.
  
 Goreaud F. & P?Pelissier R. 1999. On explicit formulas of edge effect correction for Ripley's K-function. \emph{Journal of Vegetation Science}, 10:433-438.

 Ripley B.D. 1977. Modelling spatial patterns. \emph{Journal of the Royal Statistical Society B}, 39:172-192.
  
 Stoyan D., Kendall W.S. & Mecke J. 1987. \emph{Stochastic geometry and its applications}. Wiley, New-York.
}
\author{\email{Raphael.Pelissier@ird.fr}}
\note{
  There are printing and plotting methods for \code{"fads"} objects.
}
 \section{Warning }{
  Function \code{kfun} ignores the marks of multivariate and marked point patterns, which are analysed as univariate patterns.
}
\seealso{
\code{\link{plot.fads}},
 \code{\link{spp}},
 \code{\link{kval}},
 \code{\link{k12fun}},
 \code{\link{kijfun}},
 \code{\link{ki.fun}},
 \code{\link{kmfun}}.
}
\examples{
  data(BPoirier)
  BP <- BPoirier
  \dontrun{spatial point pattern in a rectangle sampling window of size [0,110] x [0,90]}
  swr <- spp(BP$trees, win=BP$rect)
  kswr <- kfun(swr,25,1,500)
  plot(kswr)

  \dontrun{spatial point pattern in a circle with radius 50 centred on (55,45)}
  swc <- spp(BP$trees, win=c(55,45,45))
  kswc <- kfun(swc, 25, 1, 500)
  plot(kswc)
  
  \dontrun{spatial point pattern in a complex sampling window}
  swrt <- spp(BP$trees, win=BP$rect, tri=BP$tri1)
  kswrt <- kfun(swrt, 25, 1, 500)
  plot(kswrt)
}
\keyword{spatial}
