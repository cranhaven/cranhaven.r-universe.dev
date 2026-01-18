% This is a template R document file
% Distributed under GPL 3 or later

\name{fit.gld}
\alias{fit.gld}
\alias{plot.gld}
\alias{print.gld}
\alias{lines.gld}
\title{Fitting a Ramberg-Schmeiser-Tukey (RST) lambda distribution}
\description{
  To fit a Ramberg-Schmeiser-Tukey (RST) lambda distribution 
  with the three moment-matching methods.
}
\usage{
  fit.gld(x,method='LMoM')
}
\arguments{
  \item{x}{A sample of size at least 6. 'NA' values will be automatically removed.}
  \item{method}{Choose GLD fitting method.  Default: 'LMoM'.  Other options: 'MoM'-- method of moments; "MoP", method of percentiles; "LMoM", method of L-moments. 'best' chooses the best fit from the above three methods, which takes a while.}
}
\author{
  B. Wang  \email{bwang@jaguar1.usouthal.edu}
}
\references{

Karian, Z.A.,
  Dudewicz, E.J., McDonald, P., 1996. The Extended Generalized Lambda
  Distribution System for Fitting Distributions to Data:
  history,completion of theory, tables, applications, the ``final
  word'' on moment fits, \emph{Comm. in Statist.- Simul. \& Comput.}
  \bold{25}(3), 611-642.

Karian, Z.A., Dudewicz,
  E.J., 2000. \emph{Fitting Statistical Distributions: The Generalized
  Lambda Distribution and Generalized Bootstrap Methods}, Chapman and
  Hall/CRC.

}

\seealso{
 \code{\link{fit.egld}},
 \code{\link{qrsgld}},\code{\link{prsgld}},
 \code{\link{rrsgld}},\code{\link{drsgld}}.
}

\examples{

mu = 34.5; sig=1.5
y = rnorm(1000,mu,sig)
x = round(y)  ###  rounding errors

x0 = seq(min(y),max(y),length=100)
f0 = dnorm(x0,mu,sig)
plot(f0~x0,type='l')
lines(density(y),col=4)
## fit with method of moments
(out1 = fit.gld(x, method='MoM')) 
lines(out1,col=2)
##  Method of percentile
(out2 = fit.gld(x, method='mop'))
lines(out2, col=3)
## Method of L-moments
(out3 = fit.gld(x, method='lmom'))
lines(out3, col=5)
##  Fitting EGLD
(out0 = fit.egld(x))
lines(out0,col=6)

legend(max(x0), max(f0), xjust=1,yjust=1,
  legend=c("true","kde","MoM","MoP","LMoM","egld"),
  lty=c(1,1,1,1,1,1),
  col=c(1,4,2,3,5,6))

}
\keyword{stats}

