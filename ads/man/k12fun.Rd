\encoding{latin1}
\name{k12fun}
\alias{k12fun}
\title{Multiscale second-order neighbourhood analysis of a bivariate spatial point pattern}
\description{
  Computes estimates of the intertype \emph{K12}-function and associated neighbourhood functions from a bivariate spatial point pattern 
  in a simple (rectangular or circular) or complex sampling window. Computes optionally local confidence limits of the functions
  under the null hypotheses of population independence or random labelling (see Details).
}
\usage{
k12fun(p, upto, by, nsim=0, H0=c("pitor","pimim","rl"), prec=0.01, nsimax=3000, conv=50,
 rep=10, alpha=0.01,  marks)
}
\arguments{
  \item{p}{a \code{"spp"} object defining a multivariate spatial point pattern in a given sampling window (see \code{\link{spp}}).}
  \item{upto}{maximum radius of the sample circles (see Details).}
  \item{by}{interval length between successive sample circles radii (see Details).}
  \item{nsim}{number of Monte Carlo simulations to estimate local confidence limits of the selected null hypothesis (see Details).
  By default \code{nsim=0}, so that no confidence limits are computed.}
  \item{H0}{one of \code{c("pitor","pimim","rl")} to select either the null hypothesis of population independence using toroidal shift (\code{H0="pitor"}) or mimetic point process (\code{H0="pimim"}), or of random labelling (\code{H0="rl"}) (see Details).
  By default, the null hypothesis is population independence using toroidal shift.}
  \item{prec}{if \code{nsim>0} and \code{H0="pitor"} or \code{H0="pimim"}, precision of the random vector or point coordinates generated during simulations. By default \code{prec=0.01}.}
  \item{nsimax}{if \code{nsim>0} and \code{H0="pimim"}, maximum number of simulations allowed (see \code{\link{mimetic}}. By default \code{nsimax=3000}.}
  \item{conv}{if \code{nsim>0} and \code{H0="pimim"}, convergence criterion (see \code{\link{mimetic}}. By default \code{conv=50}.}
	\item{rep}{if \code{nsim>0} and \code{H0="pimim"}, controls for convergence failure of the mimetic point process (see details). By default \code{rep=10} so that the function aborts after 10 consecutive failures in mimetic point process convergence.}
  \item{alpha}{if \code{nsim>0}, significant level of the confidence limits. By default \eqn{\alpha=0.01}.}
  \item{marks}{by default c(1,2), otherwise a vector of two numbers or character strings identifying the types (the \code{p$marks} levels)
   of points of type 1 and 2, respectively.}
}
\details{
  Function \code{k12fun} computes the intertype \eqn{K12(r)} function of second-order neighbourhood analysis and the associated functions \eqn{g12(r)},
   \eqn{n12(r)} and \eqn{L12(r)}.\cr\cr
  For a homogeneous isotropic bivariate point process of intensities \eqn{\lambda1} and \eqn{\lambda2},
  the second-order property could be characterized by a function \eqn{K12(r)} (Lotwick & Silverman 1982), so that the expected
  number of neighbours of type 2 within a distance \eqn{r} of an arbitrary point of type 1 is:
  \eqn{N12(r) = \lambda2*K12(r)}.\cr\cr
  \eqn{K12(r)} is an intensity standardization of \eqn{N12(r)}: \eqn{K12(r) = N12(r)/\lambda2}.\cr\cr
  \eqn{n12(r)} is an area standardization of of \eqn{N12(r)}: \eqn{n12(r) = N12(r)/(\pi*r^2)}, where \eqn{\pi*r^2} is the area of the disc of radius \eqn{r}.\cr\cr
  \eqn{L12(r)} is a linearized version of \eqn{K12(r)}, which has an expectation of 0 under population independence: \eqn{L12(r) = \sqrt(K12(r)/\pi)-r}. \eqn{L12(r)} becomes positive when the two population show attraction and negative when they show repulsion.
   Under the null hypothesis of random labelling, the expectation of \eqn{L12(r)} is \eqn{L(r)}. It becomes greater than \eqn{L(r)} when the types tend to be positively correlated and lower when they tend to be negatively correlated.\cr\cr
  \eqn{g12(r)} is the derivative of \eqn{K12(r)} or bivariate pair density function, so that the expected
  number of points of type 2 at a distance \eqn{r} of an arbitrary point of type 1 (i.e. within an annuli between two successive circles with radii \eqn{r} and \eqn{r-by}) is:
  \eqn{O12(r) = \lambda2*g12(r)} (Wiegand & Moloney 2004).\cr\cr
  
  The program introduces an edge effect correction term according to the method proposed by Ripley (1977)
  and extended to circular and complex sampling windows by Goreaud & Pelissier (1999).\cr\cr
 
  Theoretical values under the null hypothesis of either population independence or random labelling as well as
  local Monte Carlo confidence limits and p-values of departure from the null hypothesis (Besag & Diggle 1977) are estimated at each distance \eqn{r}.\cr
  
  The population independence hypothesis assumes that the location of points of a given population is independent from the location 
  of points of the other. It is therefore tested conditionally to the intrinsic spatial pattern of each population. Two different procedures are available:
  \code{H0="pitor"} just shifts the pattern of type 1 points around a torus following Lotwick & Silverman (1982); \code{H0="pimim"} uses a mimetic point process (Goreaud et al. 2004)
  to mimic the pattern of type 1 points (see \code{\link{mimetic}}.\cr
  The random labelling hypothesis \code{"rl"} assumes that the probability to bear a given mark is the same for all points of the pattern and
   doesn't depends on neighbours. It is therefore tested conditionally to the whole spatial pattern, by randomizing the marks over the points'
  locations kept unchanged (see Goreaud & Pelissier 2003 for further details). 
}
\value{
  A list of class \code{"fads"} with essentially the following components:
  \item{r }{a vector of regularly spaced out distances (\code{seq(by,upto,by)}).}
  \item{g12 }{a data frame containing values of the bivariate pair density function \eqn{g12(r)}.}
  \item{n12 }{a data frame containing values of the bivariate local neighbour density function \eqn{n12(r)}.}
  \item{k12 }{a data frame containing values of the intertype function \eqn{K12(r)}.}
  \item{l12 }{a data frame containing values of the modified intertype function \eqn{L12(r)}.\cr}
  Each component except \code{r} is a data frame with the following variables:\cr
 \item{obs }{a vector of estimated values for the observed point pattern.}
 \item{theo }{a vector of theoretical values expected under the selected null hypothesis.}
 \item{sup }{(optional) if \code{nsim>0} a vector of the upper local confidence limits of the selected null hypothesis at a significant level \eqn{\alpha}.}
 \item{inf }{(optional) if \code{nsim>0} a vector of the lower local confidence limits of the selected null hypothesis at a significant level \eqn{\alpha}.}
 \item{pval }{(optional) if \code{nsim>0} a vector of local p-values of departure from the selected null hypothesis.}
}
\references{
 Besag J.E. & Diggle P.J. 1977. Simple Monte Carlo tests spatial patterns. \emph{Applied Statistics}, 26:327-333.\cr\cr
 Goreaud F. & Pelissier R. 1999. On explicit formulas of edge effect correction for Ripley's K-function. \emph{Journal of Vegetation Science}, 10:433-438.\cr\cr
 Goreaud, F. & Pelissier, R. 2003. Avoiding misinterpretation of biotic interactions with the intertype \emph{K12}-function: population independence vs. random labelling hypotheses. \emph{Journal of Vegetation Science}, 14: 681-692.\cr\cr
 Lotwick, H.W. & Silverman, B.W. 1982. Methods for analysing spatial processes of several types of points. \emph{Journal of the Royal Statistical Society B}, 44:403-413.\cr\cr
 Ripley B.D. 1977. Modelling spatial patterns. \emph{Journal of the Royal Statistical Society B}, 39:172-192.\cr\cr
 Wiegand, T. & Moloney, K.A. 2004. Rings, circles, and null-models for point pattern analysis in ecology. \emph{Oikos}, 104:209-229.
 Goreaud F., Loussier, B., Ngo Bieng, M.-A. & Allain R. 2004. Simulating realistic spatial structure for forest stands: a mimetic point process. In \emph{Proceedings of Interdisciplinary Spatial Statistics Workshop}, 2-3 December, 2004. Paris, France.
}
\author{\email{Raphael.Pelissier@ird.fr}}
\note{
  There are printing and plotting methods for \code{"fads"} objects.
}
\seealso{
\code{\link{plot.fads}},
 \code{\link{spp}},
 \code{\link{k12val}},
 \code{\link{kfun}},
 \code{\link{kijfun}},
 \code{\link{ki.fun}},
  \code{\link{mimetic}},
 \code{\link{kmfun}}.
}
\examples{
  data(BPoirier)
  BP <- BPoirier
  \dontrun{spatial point pattern in a rectangle sampling window of size [0,110] x [0,90]}
  swrm <- spp(BP$trees, win=BP$rect, marks=BP$species)
  #testing population independence hypothesis
  k12swrm.pi <- k12fun(swrm, 25, 1, 500, marks=c("beech","oak"))
  plot(k12swrm.pi)
  #testing random labelling hypothesis
  k12swrm.rl <- k12fun(swrm, 25, 1, 500, H0="rl", marks=c("beech","oak"))
  plot(k12swrm.rl)

  \dontrun{spatial point pattern in a circle with radius 50 centred on (55,45)}
  swc <- spp(BP$trees, win=c(55,45,45), marks=BP$species)
  k12swc.pi <- k12fun(swc, 25, 1, 500, marks=c("beech","oak"))
  plot(k12swc.pi)
  
  \dontrun{spatial point pattern in a complex sampling window}
  swrt.rl <- spp(BP$trees, win=BP$rect, tri=BP$tri2, marks=BP$species)
  k12swrt.rl <- k12fun(swrt.rl, 25, 1, 500, H0="rl",marks=c("beech","oak"))
  plot(k12swrt.rl)
  \dontrun{testing population independence hypothesis requires minimizing the outer polygon}
  xr<-range(BP$tri3$ax,BP$tri3$bx,BP$tri3$cx)
  yr<-range(BP$tri3$ay,BP$tri3$by,BP$tri3$cy)
  rect.min<-swin(c(xr[1], yr[1], xr[2], yr[2]))
  swrt.pi <- spp(BP$trees, window = rect.min, triangles = BP$tri3, marks=BP$species)
  k12swrt.pi <- k12fun(swrt.pi, 25, 1, nsim = 500, marks = c("beech", "oak"))
  plot(k12swrt.pi)
}
\keyword{spatial}
