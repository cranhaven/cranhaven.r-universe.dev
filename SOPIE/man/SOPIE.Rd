\name{SOPIE}
\alias{SOPIE}

\title{Sequential Off-Pulse Interval Estimation of a Pulsar Light Curve}
\description{\code{SOPIE} is a wrapper-function that utilises \code{\link{findh}}, \code{\link{circ.kernel}}, \code{\link{a.estimate}}
 and \code{\link{b.estimate}} to produce the estimated off-pulse intervals in an easy readable matrix format, together with
a graph.}
\usage{SOPIE(data, h = 1, to = 1, alpha = 0.05, g = 20, r = 10, m = 1, grid = 512)}

\arguments{
  \item{data}{the data vector within which to find the estimated 
smoothing parameter \eqn{\hat h} that will be used in the circular kernel density estimator. After obtaining the minimum 
point(s) from the circular kernel density estimator, the estimate off-pulse interval \eqn{[\hat a ; \hat b]} is given as result.}
  \item{h}{integer value from 1 to 9, specifying the smoothing parameter to calculate
according to the following table:
\tabular{l}{
\eqn{\hat h_1=1.06sn^{-1/5}}\cr
\eqn{\hat h_2=1.06s_\circ n^{-1/5}}\cr
\eqn{\hat h_3=1.06\bar{D}_\circ n^{-1/5}}\cr
\eqn{\hat h_4=1.06|{D}_\circ|n^{-1/5}}\cr
\eqn{\hat h_5=1.06{IQR}_\circ n^{-1/5}}\cr
\eqn{\hat h_6=\frac{1.06}{1.349}{IQR}_\circ n^{-1/5}}\cr
\eqn{\hat h_7=0.9s_\circ n^{-1/5}}\cr
\eqn{\hat h_8=\frac{0.9}{1.349}{IQR}_\circ n^{-1/5}}\cr
\eqn{\hat h_9=\frac{1}{8}\sum _{i=1}^8{h_i}}
}
}
  \item{to}{ the value of the maximum domain of the data. Values will usually
either be 1 or 2\eqn{\pi}.}
  \item{alpha}{significance level (\eqn{\alpha}) that will be used during the sequential application 
of the goodness-of-fit tests for uniformity when estimating the off-pulse interval.
}
  \item{g}{ the value of the incremental growth of each subsequent
interval over which uniformity is tested. In the suggested procedure, uniformity is
sequentially tested, with the interval used in the test growing by \code{g} observations after
every iteration. The selection of \code{g} not only influences the computation time of the
procedure, but also has an effect on the point where rejection of the hypothesis takes
place. For large values of \code{g}, the user takes the risk that uniformity is rejected for
a certain (larger) interval, while it should have been rejected earlier (for a smaller
interval). On the other hand, a very small choice of \code{g} results in long execution
times. Small values of \code{g} may also result in the early rejection of uniformity, e.g.
in the situation where a few observations may cause the rejection of uniformity,
while uniformity is again confirmed when several more observations are included
in the interval. If the user suspects that this situation may occur, the problem can
be overcome by selecting a larger value of the integer \code{r}.
}
  \item{r}{ the number of subsequent intervals that must
result in the rejection of uniformity before the function will stop. The choice of \code{r}
must therefore be linked to the choice of \code{g} as explained above. For smaller values
of \code{g}, it would be safer to select larger values of \code{r}, and vice versa. Since small values
of \code{g} may result in a temporary rejection of uniformity for an interval, a larger value
of \code{r} would prevent the method from immediately stopping at the first occurrence
of rejection. It is very important to note that, for a large value of \code{r}, there will be
no impact on the value of \eqn{\hat b} or \eqn{\hat a} if rejection takes place for each interval after a
certain point.}
  \item{m}{ the number of local minimum points included in the output.}
  \item{grid}{ the number of equally spaced grid points at which the density is to be estimated.}
}
\details{
SOPIE is a wrapper-function in the sense that it utilises the function \code{\link{findh}}, \code{\link{circ.kernel}}, \code{\link{a.estimate}}
 and \code{\link{b.estimate}} to produce
the estimated off-pulse intervals of a pulsar light curve in an easy readable matrix format, together with
a graph consisting of the histogram estimate of the sample data, the kernel density
estimator, and a visual representation of the estimated off-pulse intervals.
}
\value{The output produced by the function is a list containing the following:
\item{summary }{is a matrix that contains the estimated value of \eqn{a} and \eqn{b} for each of
the four goodness-of-fit tests, namely the Anderson-Darling, Kolmogorov-Smirnov,
Cramer-von Mises and the Rayleigh goodness-of-fit test. Based on the four estimated values of \eqn{a} and \eqn{b}, the median values of \eqn{a} and \eqn{b} are also calculated.
 This median off-pulse interval is the recommended interval and also the interval that is depicted on the graph.
}
\item{general }{is a list containing the function call, the minimum value(s) used in the
estimation, the level of significance (\eqn{\alpha}), the value of \code{g} and the value of \code{r}.}
A histogram estimate of the data is produced with the circular kernel density estimate overlaid. An indication of the estimated median off-pulse interval derived from the four goodness-of-fit tests is illustrated with two solid vertical lines. 
}
\references{
Schutte WD (2014). Nonparametric estimation of the off-pulse interval(s) of a pulsar light
curve. Ph.D. thesis, North-West University. URL \url{http://hdl.handle.net/10394/12199}. \cr
Schutte WD, Swanepoel JWH (2016). SOPIE: an R package for the non-parametric estimation of the off-pulse interval of a pulsar light curve. Monthly Notices of the Royal Astronomical Society, 461, 627-640.\cr
}
\author{ Willem Daniel Schutte }
%%\note{}
%%\seealso{}
\examples{
set.seed(777)
simdata<-von_mises_sim(n=5000,k=1,c=0.3,noise=0.2)
SOPIE(simdata,h=1,to=1,alpha=0.05,g=5,r=10,m=1,grid=100)
data(crab)
SOPIE(crab)
data(J1709)
SOPIE(J1709)
}
\keyword{nonparametric}

