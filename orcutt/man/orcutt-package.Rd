\name{orcutt-package}
\alias{orcutt-package}
\alias{orcutt}
\docType{package}
\title{
Estimate Procedure in Case of First Order Autocorrelation
}
\description{
This package has been implemented to solve first order autocorrelation problems using an iterative method. This procedure estimates both autocorrelation and beta coefficients recursively until we reach the convergence (8th decimal). The residuals are computed after estimating Beta using EGLS approach and Rho is estimated using the previous residuals.
}
\details{
\tabular{ll}{
Package: \tab orcutt\cr
Type: \tab Package\cr
Version: \tab 2.3\cr
Date: \tab 2018-09-27\cr
License: \tab GPL-2\cr
}
}
\author{
Stefano Spada [aut, cre],
        Matteo Quartagno [ctb],
        Marco Tamburini [ctb],
        David Robinson	[ctb] 

Maintainer: Stefano Spada <lostefanospada@gmail.com>
}
\references{
Verbeek M. (2004) \emph{A guide to modern econometrics}, John Wiley & Sons Ltd, ISBN:978-88-08-17054-5
}
\keyword{ cochrane }
\keyword{ orcutt }
\keyword{ autocorrelation }