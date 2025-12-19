\encoding{latin1}
\name{kp.fun}
\alias{kp.fun}
\alias{ki.fun}
\title{ Multiscale second-order neighbourhood analysis of a multivariate spatial point pattern}
\description{
(Formerly \code{ki.fun}) Computes a set of \emph{K12}-functions between all possible marks \eqn{p} and the other marks in
 a multivariate spatial point pattern defined in a simple (rectangular or circular) 
  or complex sampling window (see Details).
}
\usage{
kp.fun(p, upto, by)
}
\arguments{
  \item{p    }{a \code{"spp"} object defining a multivariate spatial point pattern in a given sampling window (see \code{\link{spp}}).}
  \item{upto }{maximum radius of the sample circles (see Details).}
  \item{by   }{interval length between successive sample circles radii (see Details).}
}
\details{
   Function \code{kp.fun} is simply a wrapper to \code{\link{k12fun}}, which computes \emph{K12(r)} between each mark \eqn{p} of the pattern
   and all other marks grouped together (the \eqn{j} points).
}
\value{
  A list of class \code{"fads"} with essentially the following components:
  \item{r }{a vector of regularly spaced distances (\code{seq(by,upto,by)}).}
  \item{labp }{a vector containing the levels \eqn{i} of \code{p$marks}.}
  \item{gp.  }{a data frame containing values of the pair density function \eqn{g12(r)}.}
  \item{np.  }{a data frame containing values of the local neighbour density function \eqn{n12(r)}.}
  \item{kp.  }{a data frame containing values of the \eqn{K12(r)} function.}
  \item{lp.  }{a data frame containing values of the modified \eqn{L12(r)} function.\cr}
 Each component except \code{r} is a data frame with the following variables:\cr
 \item{obs }{a vector of estimated values for the observed point pattern.}
 \item{theo }{a vector of theoretical values expected under the null hypothesis of population independence (see \code{\link{k12fun}}).}
}
\author{\email{Raphael.Pelissier@ird.fr}}
\note{
  There are printing and plotting methods for \code{"fads"} objects.
}
\seealso{
\code{\link{plot.fads}},
  \code{\link{spp}},
  \code{\link{kfun}},
  \code{\link{k12fun}},
  \code{\link{kpqfun}}.
}
\examples{
  data(BPoirier)
  BP <- BPoirier
  \dontrun{multivariate spatial point pattern in a rectangle sampling window}
  swrm <- spp(BP$trees, win=BP$rect, marks=BP$species)
  kp.swrm <- kp.fun(swrm, 25, 1)
  plot(kp.swrm)
  
 \dontrun{multivariate spatial point pattern in a circle with radius 50 centred on (55,45)}
  swcm <- spp(BP$trees, win=c(55,45,45), marks=BP$species)
  kp.swcm <- kp.fun(swcm, 25, 1)
  plot(kp.swcm)
  
  \dontrun{multivariate spatial point pattern in a complex sampling window}
  swrtm <- spp(BP$trees, win=BP$rect, tri=BP$tri2, marks=BP$species)
  kp.swrtm <- kp.fun(swrtm, 25, 1)
  plot(kp.swrtm)
}
\keyword{spatial}
