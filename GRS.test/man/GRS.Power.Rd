\name{GRS.Power}
\alias{GRS.Power}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Statistical Power of the GRS test
}
\description{
Calculates the power of the GRS test with density functions under H0 and H1
}
\usage{
GRS.Power(T, N, K, theta, ratio, alpha = 0.05, xmax = 10, Graph = "TRUE")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{T}{ sample size }
  \item{N}{ the number of portfolio returns}
  \item{K}{ the number of risk factors }
  \item{theta}{ maximum Sharpe ratio of the K factor portfolios}
  \item{ratio}{ theta/thetas, proportion of the potential efficiency }
  \item{alpha}{ the level of significance, default is 0.05 }
  \item{xmax}{ the support of the desnity is from 0 to xmax, default is 10 }
  \item{Graph}{ show graph if TRUE. No graph otherwise }
}
\details{
Calculate the power following GRS (1989) <DOI:10.2307/1913625>


The distribution under H1 is based on the value of theta and ratio

Under H0: ratio = 1; under H1: ratio < 1
}
\value{
  \item{Power }{power of the test}
  \item{Critical.value }{critical value at alpha}
}
\references{
Gibbons, Ross, Shanken, 1989. A test of the efficiency of a given portfolio, Econometrica, 57,1121-1152.
<DOI:10.2307/1913625>

}
\author{
Jae H. Kim
}
\note{
The graph option plots the density functions of the GRS test under H0 and H1.

The blue vertical line represents the critical value at alpha level of significance

The black density function is the one under H0, and the gray-shaded area is level of significance.

The red one is the one under H1, and the red-shaded area is the power.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{GRS(1989) <DOI:10.2307/1913625>}
\examples{
GRS.Power(T=120, N=25, K=3, theta=0.3, ratio=0.5)  # Figure 1 of Kim and Shamsuddin (2016)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }% use one of  RShowDoc("KEYWORDS")

