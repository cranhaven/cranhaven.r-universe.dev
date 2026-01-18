% This is a template R document file
% Distributed under GPL 3 or later

\name{fit.egld}
\alias{fit.egld}
\alias{print.egld}
\alias{plot.egld}
\alias{lines.egld}
\title{Fit Extended Generalized Lambda Distribution (EGLD/GBD)}
\description{
  To fit a EGLD or generalize beta distribution with the maximum likelihood methods.
}
\usage{
  fit.egld(x,xmin=NULL,xmax=NULL)
}
\arguments{
  \item{x}{A sample. 'NA' values will be automatically removed.}
  \item{xmin}{The lower limit of the underlying distribution. Default: NULL.}
  \item{xmax}{The upper limit of the underlying distribution. Default: NULL.}
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
  \code{\link{fit.gld}},
 \code{\link{qrsgld}},\code{\link{prsgld}},
 \code{\link{rrsgld}},\code{\link{drsgld}}.
}

\examples{

b3=4;b4=4; b1=1;b2=5; # EGLD(b1,b2,b3,b4)
b1=0;b2=1; # equivalently beta(b3,b4)
b1=-3;b2=5; 
xr = rbeta(100,b3,b4)
x = xr * b2 + b1 
min(x); range(x)
sum(dbeta(xr,b3,b4,1))
x0 = seq(min(x),max(x),length=100)
x1 = (x0-b1)/b2
plot(dbeta(x1,b3,b4)/b2~x0,type='l',lwd=2,col=2)
lines(density(x),lty=2, col=2)

## no prior information on min and max
(out0 = fit.egld(x))
lines(out0,col=1)
## xmin known
(out1 = fit.egld(x,xmin=-3))
lines(out1,col=3,lwd=2)
## xmax known
(out2 = fit.egld(x,xmax=2))
lines(out2, col=4)
## both known
(out3 = fit.egld(x,xmin=-3,xmax=2))
lines(out3, col=5)


}
\keyword{stats}

