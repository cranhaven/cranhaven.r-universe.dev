\name{GRS.test}
\alias{GRS.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
GRS test and Model Estimation Results
}
\description{
Wu statistic given in (5) of GRS (1989) <DOI:10.2307/1913625>


The function also provide estimation results for asset pricing models

}
\usage{
GRS.test(ret.mat, factor.mat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ret.mat}{ portfolio return matrix, T by N }
  \item{factor.mat}{ matrix of risk factors, T by K }
}
\details{ T: sample size, N: number of portfolio returns, K: number of risk factors }
\value{
    \item{GRS.stat}{GRS test statistic}
    \item{GRS.pval}{its p-value}  
    \item{coef}{matrix of coefficient estimates from N equations, N by (K+1)} 
    \item{resid}{matrix of residuals from N equations, T by N}   
    \item{tstat}{matrix of t-statistics for coefficients, N by (K+1)}   
    \item{se}{matrix of standard errors for coefficients, N by (K+1)}   
    \item{R2}{matrix of R-squares for N equations, N by 1}   
%% ...
}
\references{Gibbons, Ross, Shanken, 1989. A test of the efficiency of a given portfolio, Econometrica, 57,1121-1152.
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

Fama and French, 2015, A five-factor asset-pricing model, Journal of Financial Economics, 1-22.
<DOI:http://dx.doi.org/10.1016/j.jfineco.2014.10.010>
}
\examples{
data(data)
factor.mat = data[1:342,2:4]            # Fama-French 3-factor model
ret.mat = data[1:342,8:ncol(data)]      # 25 size-BM portfolio returns
GRS.test(ret.mat,factor.mat)$GRS.stat   # See Table 9C of Fama-French (1993)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }% use one of  RShowDoc("KEYWORDS")

