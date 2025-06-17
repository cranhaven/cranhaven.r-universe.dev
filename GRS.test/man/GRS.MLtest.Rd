\name{GRS.MLtest}
\alias{GRS.MLtest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
GRS Test Statistic and p-value based on Maximum Likelihood Estimator for Covariance matrix
}
\description{
W statistic given in (7) of GRS (1989) <DOI:10.2307/1913625>

}
\usage{
GRS.MLtest(ret.mat, factor.mat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ret.mat}{ portfolio return matrix, T by N }
  \item{factor.mat}{ matrix of risk factors, T by K }
}
\details{T: sample size, N: number of portfolio returns, K: number of risk factors}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
 \item{GRS.stat}{GRS test statistic}
 \item{GRS.pval}{its p-value}
 \item{theta}{ maximum Sharpe ratio of the K factor portfolios}
 \item{thetas}{slope of the efficient frontier based on all assets}
 \item{ratio}{theta/thetas, proportion of the potential efficiency}
%% ...
}
\references{
Gibbons, Ross, Shanken, 1989. A test of the efficiency of a given portfolio, Econometrica, 57,1121-1152.
<DOI:10.2307/1913625>
}
\author{
Jae H. Kim
}
\note{
Applicable to CAPM as well as a multi-factor model
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Fama and French, 1993, Common risk factors in the returns on stocks and bonds, Journal of Financial Economics, 33, 3-56.
<DOI:10.1016/0304-405X(93)90023-5>

Fama and French, 2015, A five-factor asset-pricing model, Journal of Financial Economics, 116-1-22.
<DOI:http://dx.doi.org/10.1016/j.jfineco.2014.10.010>
}
\examples{
data(data)
factor.mat = data[1:342,2:4]            # Fama-French 3-factor model
ret.mat = data[1:342,8:ncol(data)]      # 25 size-BM portfolio returns
GRS.MLtest(ret.mat,factor.mat)          # See column (iv), Table 9C of Fama-French (1993)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }% use one of  RShowDoc("KEYWORDS")

