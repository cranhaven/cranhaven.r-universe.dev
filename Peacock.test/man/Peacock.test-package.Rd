\name{Peacock.test-package}
\alias{Peacock.test-package}
\alias{Peacock.test}
\docType{package}
\title{Multidimensional Kolmogorov-Smirnov Two-Sample Test}
\description{
Two R-functions: peacock2 and peacock3 are provided to compute the two dimensional and three dimensional 
KS two-sample tests, respectively. The famous KS two sample test was generalized to multidimensional 
spaces by Peacock (1983). Hence, it is also called the Peacock test. The Peacock is different from
the widely used Fasano and Franceschini test (1987). The latter is a variant of the KS test, invented 
to alleviate the computational intensity of the former. The two R-functions implement the original definition
of the KS test given by Peacock (1983). 
}
\details{
\tabular{ll}{
Package: \tab Peacock.test\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2016-07-13\cr
License: \tab GPL-2 \cr
}
The two functions: peacock2 and peacock3, provided in this package are 
self-explanatory and their usage is straightforward. 
}
\author{
Yuanhui Xiao

Maintainer: Yuanhui Xiao <xiao\_yuanhui@hotmail.com>
~~ The author and/or maintainer of the package ~~
}
\references{
Fasano, G., Franceschini, A. (1987)<DOI:10.1093/mnras/225.1.155>.
A multidimensional version of the Kolmogorov-Smirnov test.
Monthly Notices of the Royal Astronomical Society 225:155-170. 

Peacock J.A. (1983) <DOI:10.1093/mnras/202.3.615>.
Two-dimensional goodness-of-fit testing in astronomy.
Monthly Notices of the Royal Astronomical Society 202:615-627.

Xiao Y(2016). A fast algorithm for two-dimensional Kolmogorov-Smirnov two-sample tests,
Journal Computational Statistics and Data Analysis, under revision. 
}

\keyword{Kolmogorov-Smirnov/Peacock Two-Sample test}
\keyword{Fasano-Franceschini Two-Sample test}
\examples{
# two-dimensional case 
    x2 <- matrix(rnorm(12, 0, 1), ncol=2)
    y2 <- matrix(rnorm(16, 0, 1), ncol=2)
    ks2 <- peacock2(x2, y2)
    ks2
# three-dimensional case 
    x3 <- matrix(rnorm(12, 0, 1), ncol=3)
    y3 <- matrix(rnorm(18, 0, 1), ncol=3)
    ks3 <- peacock3(x3, y3)
    ks3 
}
