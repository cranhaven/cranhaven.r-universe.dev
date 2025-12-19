\encoding{latin1}
\name{kmfun}
\alias{kmfun}
\title{Multiscale second-order neighbourhood analysis of a marked spatial point pattern}
\description{
   Computes estimates of the mark correlation \emph{Km}-function and associated neighbourhood functions from a marked spatial point pattern 
  in a simple (rectangular or circular) or complex sampling window. Computes optionally local confidence limits of the functions
  under the null hypothesis of no correlation between marks (see Details).
}
\usage{
kmfun(p, upto, by, nsim=0, alpha=0.01)
}
\arguments{
  \item{p}{a \code{"spp"} object defining a marked spatial point pattern in a given sampling window (see \code{\link{spp}}).}
  \item{upto }{maximum radius of the sample circles (see Details).}
  \item{by }{interval length between successive sample circles radii (see Details).}
  \item{nsim }{number of Monte Carlo simulations to estimate local confidence limits of the null hypothesis of no correlation between marks (see Details).
  By default \code{nsim=0}, so that no confidence limits are computed.}
  \item{alpha }{if \code{nsim>0}, significant level of the confidence limits. By default \eqn{\alpha=0.01}.}
}
\details{
  Function \code{kmfun} computes the mark correlation function \eqn{Km(r)} and the associated function \eqn{gm(r)}.\cr\cr
 It is defined from a general definition of spatial autocorrelation (Goreaud 2000) as:\cr
 
 \eqn{Km(r) = (COV(Xi,Xj)|d(i,j)<r) / VAR(X)}\cr
 
 where \eqn{X} is a quantitative random variable attached to each point of the pattern.
 
 \emph{Km(r)} has a very similar interpretation than more classical correlation functions, such as Moran's \emph{I}: it takes values between -1 and 1, with an expectation of 0 under the null hypothesis of no spatial correlation between the values of \emph{X}, becomes positive when values of \eqn{X} at distance \emph{r} are positively correlated and negative when values of \eqn{X} at distance \emph{r} are negatively correlated.
 
 \eqn{gm(r)} is the derivative of \eqn{Km(r)} or pair mark correlation function, which gives the correlation of marks within an annuli between two successive circles with radii \eqn{r} and \eqn{r-by}).\cr\cr
  
  The program introduces an edge effect correction term according to the method proposed by Ripley (1977) and extended to circular and complex sampling windows by Goreaud & P?Pelissier (1999).
  
  Local Monte Carlo confidence limits and p-values of departure from the null hypothesis of no correlation are estimated at each distance \eqn{r}, after reallocating at random the values of \emph{X} over all points of the pattern, the location of trees being kept unchanged.
}
\value{
 A list of class \code{"fads"} with essentially the following components:
  \item{r }{a vector of regularly spaced out distances (\code{seq(by,upto,by)}).}
  \item{gm }{a data frame containing values of the pair mark correlation function \eqn{gm(r)}.}
  \item{km }{a data frame containing values of the mark correlation function \eqn{Km(r)}.\cr}
 Each component except \code{r} is a data frame with the following variables:\cr
 \item{obs }{a vector of estimated values for the observed point pattern.}
 \item{theo }{a vector of theoretical values expected for the null hypothesis of no correlation between marks.}
 \item{sup }{(optional) if \code{nsim>0} a vector of the upper local confidence limits of the null hypothesis at a significant level \eqn{\alpha}.}
 \item{inf }{(optional) if \code{nsim>0} a vector of the lower local confidence limits of the null hypothesis at a significant level \eqn{\alpha}.}
 \item{pval }{(optional) if \code{nsim>0} a vector of local p-values of departure from the null hypothesis.}
}
\note{
Applications of this function can be found in Oddou-Muratorio \emph{et al.} (2004) and Madelaine \emph{et al.} (submitted).
}

  \references{Goreaud, F. 2000. \emph{Apports de l'analyse de la structure spatiale en foret tempere a l'etude et la modelisation des peuplements complexes}. These de doctorat, ENGREF, Nancy, France.\cr\cr
Goreaud F. & P?Pelissier R. 1999. On explicit formulas of edge effect correction for Ripley's K-function. \emph{Journal of Vegetation Science}, 10:433-438.\cr\cr
Madelaine, C., Pelissier, R., Vincent, G., Molino, J.-F., Sabatier, D., Prevost, M.-F. & de Namur, C. 2007. Mortality and recruitment in a lowland tropical rainforest of French Guiana: effects of soil type and species guild. \emph{Journal of Tropical Ecology}, 23:277-287.

Oddou-Muratorio, S., Demesure-Musch, B., Pelissier, R. & Gouyon, P.-H. 2004. Impacts of gene flow and logging history on the local genetic structure of a scattered tree species, Sorbus torminalis L. \emph{Molecular Ecology}, 13:3689-3702.

Ripley B.D. 1977. Modelling spatial patterns. \emph{Journal of the Royal Statistical Society B}, 39:172-192.
}
\author{\email{Raphael.Pelissier@ird.fr}}
\seealso{
\code{\link{plot.fads}},
 \code{\link{spp}},
 \code{\link{kfun}},
 \code{\link{k12fun}},
 \code{\link{kijfun}},
 \code{\link{ki.fun}}.
 }
\examples{
  data(BPoirier)
  BP <- BPoirier
  \dontrun{spatial point pattern in a rectangle sampling window of size [0,110] x [0,90]}
  swrm <- spp(BP$trees, win=BP$rect, marks=BP$dbh)
  kmswrm <- kmfun(swrm, 25, 2, 500)
  plot(kmswrm)
  
  \dontrun{spatial point pattern in a circle with radius 50 centred on (55,45)}
  swc <- spp(BP$trees, win=c(55,45,45), marks=BP$dbh)
  kmswc <- kmfun(swc, 25, 2, 500)
  plot(kmswc)

  \dontrun{spatial point pattern in a complex sampling window}
  swrt <- spp(BP$trees, win=BP$rect, tri=BP$tri2, marks=BP$dbh)
  kmswrt <- kmfun(swrt, 25, 2, 500)
  plot(kmswrt)

}
\keyword{spatial}
