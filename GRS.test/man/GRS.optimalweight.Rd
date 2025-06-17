\name{GRS.optimalweight}
\alias{GRS.optimalweight}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Weighted Optimal Level of Significance for the GRS test: Normality Assumption
}
\description{
The optimal level is calculated by minimizing expected loss from hypothesis testing.

The F-distributions are used to calculate the power, under the normality assumption

The power is calculated using a range of non-centrality parameters (lamdba), folloing a folded-normal distribution.

The weights are obtained from the density function of folded-normal distribution. 

See, for details, Kim and Choi, 2017, Choosing the Level of Significance: A Decision-theoretic Approach. 
}
\usage{
GRS.optimalweight(T, N, K, theta, ratio, delta = 3, p = 0.5, k = 1, Graph = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{T}{ sample size }
  \item{N}{ the number of portfolio returns}
  \item{K}{ the number of risk factors }
  \item{theta}{ maximum Sharpe ratio of the K factor portfolios}
  \item{ratio}{ theta/thetas, proportion of the potential efficiency }
  \item{delta}{ the standard deviation of the folded-normal distribution, default is 3 }
  \item{p}{ prior probability for H0, default is p = 0.5}
  \item{k}{ relative loss, k = L2/L1, default is k = 1}
  \item{Graph}{ show graph if TRUE. No graph otherwise }
}
\details{
Based on the power calculation of the GRS test, as in GRS (1989) <DOI:10.2307/1913625>.

The plot shows the folded-normal distribution. 
}
\value{
\item{opt.sig }{Optimal level of significance}
\item{opt.crit }{Critical value corresponding to opt.sig}
}

\references{
Leamer, E. 1978, Specification Searches: Ad Hoc Inference with Nonexperimental Data, Wiley, New York.

Kim, JH and Ji, P. 2015, Significance Testing in Empirical Finance: A Critical Review and Assessment, Journal of Empirical Finance 34, 1-14.
<DOI:http://dx.doi.org/10.1016/j.jempfin.2015.08.006>

Gibbons, Ross, Shanken, 1989. A test of the efficiency of a given portfolio, Econometrica, 57,1121-1152.
<DOI:10.2307/1913625>

Kim and Shamsuddin, 2017, Empirical Validity of Asset-pricing Models: Application of Optimal Significance Level and Equal Probability Test
}
\author{
Jae H. Kim
}
\note{
ratio = theta/thetas

thetas = maximum Sharpe ratio of the K factor portfolios: GRS (1989) <DOI:10.2307/1913625>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Kim and Choi, 2017, Choosing the Level of Significance: A Decision-theoretic Approach
}
\examples{
GRS.optimalweight(T=90, N=25, K=3, theta=0.25, ratio=0.4) 
}
\keyword{ htest }% use one of  RShowDoc("KEYWORDS")

