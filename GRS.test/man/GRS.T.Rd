\name{GRS.T}
\alias{GRS.T}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sample Size Selection for the GRS test
}
\description{

Given the desired level of Type I and II error probabilities, the function returns the sample size required.

}
\usage{
GRS.T(N, K, theta, ratio, alpha, beta, Tmax = 10000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{N}{ the number of portfolio returns}
  \item{K}{ the number of risk factors }
  \item{theta}{ maximum Sharpe ratio of the K factor portfolios}
  \item{ratio}{ theta/thetas, proportion of the potential efficiency }
  \item{alpha}{ the desried level of significance, or Type I error probability }
  \item{beta}{  the desried level of Type II error probability}
  \item{Tmax}{ the maximum number of sample size, default is 10000 }
}
\details{ the desired level of power = 1 - beta }
\value{
    \item{Required.T}{required sample size}
    \item{Critical.value}{the corresponding critical value}   
%% ...
}
\references{Gibbons, Ross, Shanken, 1989. A test of the efficiency of a given portfolio, Econometrica, 57,1121-1152.
<DOI:10.2307/1913625>
}
\author{
Jae H. Kim
}
\note{
Critical.value is from the F-distribution with df1=N and df2=Required.T-N-K degrees of freedom, at the alpha level of significance. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Kim and Shamsuddin, 2017, Empirical Validity of Asset-pricing Models: Application of Optimal Significance Level and Equal Probability Test
}
\examples{
GRS.T(N=25,K=3,theta=0.25,ratio=0.4,alpha=0.05, beta=0.05, Tmax=5000) 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }% use one of  RShowDoc("KEYWORDS")

