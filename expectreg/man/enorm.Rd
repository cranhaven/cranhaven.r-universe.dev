\name{enorm}
\Rdversion{1.1}
\alias{enorm}
\alias{ebeta}
\alias{eunif}
\alias{et}
\alias{egamma}
\alias{elnorm}
\alias{echisq}
\alias{eexp}
\alias{penorm}
\alias{pebeta}
\alias{peunif}
\alias{pet}
\alias{pegamma}
\alias{pelnorm}
\alias{pechisq}
\alias{peexp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Expectiles of distributions
}
\description{
Much like the 0.5 quantile of a distribution is the median,
the 0.5 expectile is the mean / expected value.
These functions add the possibility of calculating expectiles
of known distributions. The functions starting with 'e' calculate
an expectile value for given asymmetry values, the functions 
starting with 'pe' calculate vice versa.
}
\usage{
enorm(asy, m = 0, sd = 1)
penorm(e, m = 0, sd = 1)

ebeta(asy, a = 1, b = 1)
pebeta(e, a = 1, b = 1)

eunif(asy, min = 0, max = 1)
peunif(e, min = 0, max = 1)

et(asy, df)
pet(e, df)

elnorm(asy, meanlog = 0, sdlog = 1)
pelnorm(e, meanlog = 0, sdlog = 1)

egamma(asy, shape, rate = 1, scale = 1/rate)
pegamma(e, shape, rate = 1, scale = 1/rate)

eexp(asy, rate = 1)
peexp(e, rate = 1)

echisq(asy, df)
pechisq(e, df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{asy}{
vector of asymmetries with values between 0 and 1.
}
  \item{e}{
vector of expectiles from the respective distribution.
}
  \item{m, sd}{
mean and standard deviation of the Normal distribution.
}
  \item{a, b}{
positive parameters of the Beta distribution.
}
  \item{min, max}{
minimum, maximum of the uniform distribution.
}
  \item{df}{
degrees of freedom of the student t and chi squared distribution.
}
  \item{meanlog, sdlog}{
parameters of the lognormal distribution.  
}
  \item{shape, rate, scale}{
parameters of the gamma distribution (with 2 different parametrizations)
and parameter of the exponential distribution which is a special case of the gamma
with shape=1.  
}
}
\details{
An expectile of a distribution cannot be determined explicitely,
but instead is given by an equation.
The expectile z for an asymmetry p is:
\eqn{ p = \frac{G(z) - z F(z)}{2(G(z) - z F(z)) + z - m}}
where m is the mean, F the cdf and G the partial moment function
\eqn{ G(z) = \int\limits_{-\infty}^{z} uf(u) \mbox{d}u }.
}
\value{
Vector of the expectiles or asymmetry values for the desired distribution.
}
\references{
Newey W and Powell J (1987)
\emph{ Asymmetric least squares estimation and testing }
Econometrica, 55:819-847
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
\code{\link{eemq}}
}
\examples{
x <- seq(0.02,0.98,0.2)

e = enorm(x)
e
penorm(e)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ distribution }
