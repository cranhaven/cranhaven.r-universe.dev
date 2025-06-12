\name{bssn-package}
\alias{bssn-package}
\docType{package}
\title{
Birnbaum-Saunders model
}
\description{
It provides the density, distribution function, quantile function,
random number generator, reliability function, failure rate, likelihood function,
moments and EM algorithm for Maximum Likelihood estimators, also empirical quantile
and generated envelope for a given sample, all this for the three parameter
Birnbaum-Saunders model based on Skew-Normal Distribution.
Also, it provides the random number generator for the mixture of Birbaum-Saunders model based on Skew-Normal distribution. Additionally, we incorporate the EM algorithm based on the assumption that the error term follows a finite mixture of Sinh-normal distributions.
}
\details{
\tabular{ll}{
Package: \tab bssn\cr
Type: \tab Package\cr
Version: \tab 1.5\cr
Date: \tab 2020-02-12\cr
License: \tab GPL (>=2)\cr
}
}

\author{Rocio Maehara \email{rmaeharaa@gmail.com} and Luis Benites \email{lbenitesanchez@gmail.com}
}

\references{
Vilca, Filidor; Santana, L. R.; Leiva, Victor; Balakrishnan, N. (2011). Estimation of extreme percentiles in Birnbaum Saunders distributions. Computational Statistics & Data Analysis (Print), 55, 1665-1678.

Santana, Lucia; Vilca, Filidor; Leiva, Victor (2011). Influence analysis in skew-Birnbaum Saunders regression models and applications. Journal of Applied Statistics, 38, 1633-1649.
}

\keyword{package}

\seealso{
\code{\link{bssn}}, \code{\link{EMbssn}}, \code{\link{momentsbssn}}, \code{\link{ozone}}, \code{\link{reliabilitybssn}}, \code{\link{FMshnReg}}
}

\examples{
#See examples for the bssnEM function linked above.
}
