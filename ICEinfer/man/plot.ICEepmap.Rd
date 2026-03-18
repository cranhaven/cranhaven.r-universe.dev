\name{plot.ICEepmap}
\alias{plot.ICEepmap}
\title{Display Indifference Curves on a standardized ICE Economic Preference Map}
\description{
 Display plots of the Indifference Curves of an ICE Economic Preference Map using the
 contourplot() and expand.grid() functions from the \bold{lattice} R-package.
}
\usage{
 \method{plot}{ICEepmap}(x, xygrid = FALSE, \dots)
}
\arguments{
  \item{x}{Output list object from either ICEepmap or ICEomega.}
  \item{xygrid}{Either FALSE or a grid object for a lattice of (x, y) plotting positions.}
  \item{\dots}{Optional argument(s) passed on to contourplot().}
}
\details{
 If xygrid == FALSE, the default xygrid wiil be a 201 x 201 lattice of equally spaced plotting
 positions covering the x=DeltaEffe and y=DeltaCost ranges [-10,+10].  This default is:
 x <- seq(-10, +10, length = 201); y <- x; xygrid <- expand.grid(x = x, y = y)
}
\value{NULL}
\references{
 Cook JR, Heyse JF.  Use of an angular transformation for ratio estimation in cost-effectiveness
 analysis.  \emph{Statistics in Medicine}  2000; \bold{19}: 2989-3003.
 
 Obenchain RL.  Incremental Cost-Effectiveness (ICE) Preference Maps.  2001 \emph{JSM Proceedings
 (Biopharmaceutical Section) on CD-ROM}. (10 pages.)  Alexandria, VA: American Statistical
 Association.  2002.
 
 Obenchain RL.  ICE Preference Maps: Nonlinear Generalizations of Net Benefit and Acceptability.
 \emph{Health Serv Outcomes Res Method}  2008; \bold{8}: 31-56. DOI 10.1007/s10742-007-0027-2.
 Open Access.
}
\author{Bob Obenchain <wizbob@att.net>}
\seealso{\code{\link{ICEepmap}} and \code{\link{ICEomega}}}
\examples{
 epm <- ICEomega(beta=0.8)
 require(lattice)
 plot(epm)
}
\keyword{methods}
\keyword{hplot}