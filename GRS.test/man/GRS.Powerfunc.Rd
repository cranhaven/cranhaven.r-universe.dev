\name{GRS.Powerfunc}
\alias{GRS.Powerfunc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Power functions for the GRS test }
\description{
The function plots the power functions for a range of sample size (T), given the other parameter values
}
\usage{
GRS.Powerfunc(Tvec, N, K, theta, alpha = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Tvec}{ a vector of sample sizes }
  \item{N}{ the number of portfolio returns}
  \item{K}{ the number of risk factors }
  \item{theta}{ maximum Sharpe ratio of the K factor portfolios}
  \item{alpha}{ the level of significance, default is 0.05 }
}
\details{The power is plotted against the ratio=theta/thetas, the proprotion of potential efficiency }
\value{
{Power: }{Matrix of power values plotted}
}
\references{Gibbons, Ross, Shanken, 1989. A test of the efficiency of a given portfolio, Econometrica, 57,1121-1152.
<DOI:10.2307/1913625>
}
\author{
Jae H. Kim
}
\note{
Under H0: ratio = 1, so the power = alpha when ratio = 1.

The power increases as the ratio declines from 1.

The power increases with sample size, so the upper power function is associated with larger sample size. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
GRS (1989) <DOI:10.2307/1913625>
}
\examples{
GRS.Powerfunc(Tvec=c(60,120),N=25, K=3,theta=0.3)  # Figure 2 of Kim and Shamsuddin (2016)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }% use one of  RShowDoc("KEYWORDS")

