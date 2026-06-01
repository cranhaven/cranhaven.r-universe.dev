\name{b.estimate}
\alias{b.estimate}
\title{Estimate the Right Endpoint of the Off-Pulse Interval of a Pulsar
}
\description{\code{a.estimate} and \code{b.estimate} is almost identical function. \code{a.estimate} is the function
used to obtain the estimated values of \eqn{a}, i.e. \eqn{\hat a} for the off-pulse interval of a pulsar light curve. \code{b.estimate} is the function
used to obtain the estimated values of \eqn{b}, i.e. \eqn{\hat b}, for the off-pulse interval of a pulsar light curve.
}
\usage{
b.estimate(data, to = 1, min_points, alpha = 0.05, g = 1, r = 1)
}
\arguments{
 \item{data }{the data vector used to estimate \eqn{b}.}
  \item{to }{the value of the maximum domain of the data. Values will usually
either be 1 or 2\eqn{\pi}.}
  \item{min_points }{a scalar or vector containing the value(s) of the minimum point(s)
calculated during the kernel density estimation. This argument does not represent
the index value(s) of the observations within data. The minimum point(s) can be obtained with the function \code{\link{circ.kernel}}.
}
 \item{alpha }{significance level (\eqn{\alpha}) that will be used during the sequential application 
of the goodness-of-fit tests for uniformity when estimating the off-pulse interval.
}
\item{g }{the value of the incremental growth of each subsequent
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
  \item{r }{the number of subsequent intervals that must
result in the rejection of uniformity before the function will stop. The choice of \code{r}
must therefore be linked to the choice of \code{g} as explained above. For smaller values
of \code{g}, it would be safer to select larger values of \code{r}, and vice versa. Since small values
of \code{g} may result in a temporary rejection of uniformity for an interval, a larger value
of \code{r} would prevent the method from immediately stopping at the first occurrence
of rejection. It is very important to note that, for a large value of \code{r}, there will be
no impact on the value of \eqn{\hat b} or \eqn{\hat a} if rejection takes place for each interval after a
certain point.}
}

\value{a list containing the following components:
\item{summary }{a vector containing the estimated value of b, i.e. \eqn{\hat b}, for each of
the four goodness-of-fit tests, namely the Anderson-Darling, Kolmogorov-Smirnov,
Cramer-von Mises and the Rayleigh goodness-of-fit test.}
\item{$general }{a list containing the function call, the minimum value(s) used in the
estimation, the level of significance (\eqn{\alpha}), the value of \code{g} and the value of \code{r}.}
}
\references{D'Agostino, R. & Stephens, M. (eds) (1986). Goodness-of-t techniques, Marcel Dekker, Inc. \cr
Jammalamadaka, S. Rao and SenGupta, A. (2001). Topics in Circular Statistics, World Scientific Publishing Co. Pte. Ltd. \cr
Marsaglia G, Marsaglia J (2004). Evaluating the Anderson-Darling Distribution. Journal of Statistical software, 9, 1-5. \cr
Marsaglia G, Tsang WW, Wang J (2003). Evaluating Kolmogorov's Distribution. Journal of Statistical Software, 8(18), 1-4.\cr
Schutte WD, Swanepoel JWH (2016). SOPIE: an R package for the non-parametric estimation of the off-pulse interval of a pulsar light curve. Monthly Notices of the Royal Astronomical Society, 461, 627-640.\cr
Stephens M (1970). Use of the Kolmogorov-Smirnov, Cramer-Von Mises and related statistics without extensive tables. Journal of the Royal Statistical Society. Series B (Methodological), 32, 115-122.
}
\author{ Willem Daniel Schutte }
%%\note{}
\seealso{\code{\link[ADGofTest]{ad.test}} , \code{\link[stats]{ks.test}} , \code{\link[circular]{rayleigh.test}}}
%%\seealso{\link{[ADGofTest]{\code{ad.test}}}}
%%,\link{[circular]{\code{rayleigh.test}}},\link{\code{ks.test}}}
%%\seealso{[circular]{\code{rayleigh.test}}}
%%\seealso{\code{ks.test}}

\examples{
## This function is to be used inside the wrapper function SOPIE

simdata<-von_mises_sim(n=5000,k=1,c=0.3,noise=0.2)
SOPIE(simdata,h=1,to=1,alpha=0.05,g=5,r=10,m=1,grid=100)
}
\keyword{nonparametric}
