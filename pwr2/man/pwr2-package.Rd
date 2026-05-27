\name{pwr2-package}
\alias{pwr2-package}
\alias{pwr2}
\docType{package}
\title{
Power and Sample Size Analysis for One-way and Two-way ANOVA Models
}
\description{
User friendly functions for power and sample size analysis at one-way and two-way ANOVA settings take either effect size or delta and sigma as arguments. They are designed for both one-way and two-way ANOVA settings. In addition, a function for plotting power curves is available for power comparison, which can be easily visualized by statisticians and clinical researchers.
}
\details{
\tabular{ll}{
Package: \tab SPA\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2017-05-01\cr
License:\tab  GPL-2\cr
}
There are major five functions in the package. The pwr.1way and pwr.2way functions provide the power analysis for one-way and two-way ANOVA models. The ss.1way and ss.2way functions provide the sample size calculation for one-way and two-way ANOVA models. The pwr.plot function illustrates drawing power curves for different parameter settings.
}
\author{
Pengcheng Lu, Junhao Liu, and Devin Koestler.

Maintainer:Pengcheng Lu <plu2@kumc.edu>
}
\references{
[1] Angela Dean & Daniel Voss (1999). Design and Analysis of Experiments. Springer.
}
\keyword{ANOVA}
\keyword{Sample Size}
\keyword{Power}
\examples{
## Example 1
pwr.2way(a=3, b=3, alpha=0.05, size.A=4, size.B=5, f.A=0.8, f.B=0.4)
pwr.2way(a=3, b=3, alpha=0.05, size.A=4, size.B=5, delta.A=4, delta.B=2, sigma.A=2, sigma.B=2)

## Example 2
ss.2way(a=3, b=3, alpha=0.05, beta=0.1, delta.A=1, delta.B=2, sigma.A=2, sigma.B=2, B=100)

## Example 3
n <- seq(2, 30, by=4)
f <- seq(0.1, 1.0, length.out=10)
pwr.plot(n=n, k=5, f=f, alpha=0.05)
}