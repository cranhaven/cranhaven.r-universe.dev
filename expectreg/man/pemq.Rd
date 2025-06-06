\name{pemq}
\Rdversion{1.1}
\alias{pemq}
\alias{demq}
\alias{remq}
\alias{qemq}
\alias{eemq}
\alias{peemq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The "expectiles-meet-quantiles" distribution family.
}
\description{
Density, distribution function, quantile function, random generation, expectile function
and expectile distribution function for a family of distributions
for which expectiles and quantiles coincide.
}
\usage{
pemq(z,ncp=0,s=1)
demq(z,ncp=0,s=1)
qemq(q,ncp=0,s=1)
remq(n,ncp=0,s=1)
eemq(asy,ncp=0,s=1)
peemq(e,ncp=0,s=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ncp}{
non centrality parameter and mean of the distribution.
}
  \item{s}{
scaling parameter, has to be positive.
}
  \item{z, e}{
vector of quantiles / expectiles.
}
  \item{q, asy}{
vector of asymmetries / probabilities.
}
  \item{n}{
number of observations. If length(n) > 1, the length is taken to be the number required.
}
}
\details{
This distribution has the cumulative distribution function:
\eqn{ F(x;ncp,s) = \frac{1}{2}(1 + sgn(\frac{x-ncp}{s}) \sqrt{1 - \frac{2}{2 + (\frac{x-ncp}{s})^2}}) }

and the density:
\eqn{ f(x;ncp,s) = \frac{1}{s}( \frac{1}{2 + (\frac{x-ncp}{s})^2} )^\frac{3}{2}
}

It has infinite variance, still can be scaled by the parameter \code{s}.
It has mean \code{ncp}.
In the canonical parameters it is equal to a students-t distribution with 2 degrees of freedom.
For \eqn{ s = \sqrt{2} } it is equal to a distribution introduced by Koenker(2005).
}

\value{
\code{demq} gives the density, \code{pemq} and \code{peemq} give the distribution function,
\code{qemq} gives the quantile function, \code{eemq} computes the expectiles numerically and is only provided for completeness,
since the quantiles = expectiles can be determined analytically using \code{qemq}, 
and \code{remq} generates random deviates.
}
\references{
Koenker R (2005)
\emph{ Quantile Regression }
Cambridge University Press, New York
}
\author{
Fabian Otto- Sobotka \cr
Carl von Ossietzky University Oldenburg \cr
\url{https://uol.de} \cr

Thomas Kneib \cr
Georg August University Goettingen \cr
\url{https://www.uni-goettingen.de} \cr
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{enorm}}
}
\examples{
x <- seq(-5,5,length=100)
plot(x,demq(x))
plot(x,pemq(x,ncp=1))

z <- remq(100,s=sqrt(2))
plot(z)

y <- seq(0.02,0.98,0.2)
qemq(y)
eemq(y)

pemq(x) - peemq(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ distribution }
