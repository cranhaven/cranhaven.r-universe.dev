\name{GRS.optimalbootweight}
\alias{GRS.optimalbootweight}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Weighted Optimal Level of Significance for the GRS test: Bootstrapping
}
\description{
The optimal level is calculated by minimizing expected loss from hypothesis testing

The bootstrap is used to calculate the power.

The non-centrality paramter is estimated and its bootstrap distribution is obtained. 

9 percentiles from this distribution is used to caculate the power and the optimal level. 

These optimal levesl ared weighted using the weights from the density of the bootstrap distribution of lambda. 

}
\usage{
GRS.optimalbootweight(ret.mat,factor.mat,p=0.5,k=1,nboot=3000,wild=FALSE,Graph=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ret.mat}{ portfolio return matrix, T by N }
  \item{factor.mat}{ matrix of risk factors, T by K }
  \item{p}{ prior probability for H0, default is p = 0.5}
  \item{k}{ relative loss, k = L2/L1, default is k = 1}
  \item{nboot}{ the number of bootstrap iterations, the default is 3000}
  \item{wild}{ if TRUE, wild bootsrap is conducted; if FALSE  (default), bootstrap is based on iid residual resampling}
  \item{Graph}{ show graph if TRUE (default). No graph otherwise }
}

\details{
Power is calculated based on the bootstrap

The plot shows the bootstrap distribution of lambda (non-centrality parameter)

See Kim and Choi, 2017, Choosing the Level of Significance: A Decision-theoretic Approach

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
The example below sets nboot=500 for faster execution, but a higher number is recommended. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Kim and Choi, 2017, Choosing the Level of Significance: A Decision-theoretic Approach
}
\examples{
data(data)
n=60; m1=nrow(data)-n+1; m2=nrow(data)   # Choose the last n observations from the data set
factor.mat = data[m1:m2,2:6]            # Fama-French 5-factors
ret.mat = data[m1:m2,8:ncol(data)]      # 25 size-BM portfolio returns
GRS.optimalboot(ret.mat,factor.mat,p=0.5,k=1,nboot=500,wild=TRUE,Graph=TRUE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }% use one of  RShowDoc("KEYWORDS")

