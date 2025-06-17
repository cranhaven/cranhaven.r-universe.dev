\name{lis.test}
\alias{lis.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Test for independence between paired samples}
\description{ Test for independence between X and Y computed from a paired sample (x1,y1),...(xn,yn) of (X,Y), using one of the following statistics (a) the Longest Increasing Subsequence (Ln), (b) JLn, a Jackknife version of Ln or (c) JLMn, a Jackknife version of the longest monotonic subsequence. This family of tests can be applied under the assumption of continuity of X and Y.}
\usage{
lis.test(x, y, alternative = c("two.sided", "less", "greater"), 
method = c("JLMn", "Ln", "JLn"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y}{numeric vectors of data values. x and y must have the same length.}
%  \item{y}{
%%     ~~Describe \code{y} here~~}
  \item{alternative}{indicates the alternative hypothesis and must be one of "two.sided"(default), "greater" or "less". 
}
  \item{method}{a character string indicating which statistics is to be used for the test. One of "Ln", "JLn", or "JLMn"(default).
}
}
\details{
For sample sizes less than 20, the correction introduced in subsection 3.2 from main reference, with c = 0.4 was avoided.}
\value{
  \item{sample.estimate}{the value of the statistic.}
  \item{p.value}{the p-value for the test.}
  \item{alternative}{a character string describing the alternative hypothesis.}
  \item{method}{a character string indicating what type of Lis-test was performed.}
}
\references{J. E. Garcia, V. A. Gonzalez-Lopez, Independence tests for continuous random variables based on the longest increasing subsequence, Journal of Multivariate Analysis (2014), \url{http://dx.doi.org/10.1016/j.jmva.2014.02.010}}
\author{J. E. Garcia and V. A. Gonzalez-Lopez}
%\note{
%%  ~~further notes~~}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~}
\examples{
# Example 1
# mixture of two bivariate normal, one with correlation 0.9 
# and the other with correlation -0.9 

N <-100
ro<- 0.90
Z1<-rnorm(N)
Z2<-rnorm(N)
X2<-X1<-Z1
I<-(1:floor(N*0.5))
I2<-((floor(N*0.5)+1):N)
X1[I]<-Z1[I]
X2[I]<-(Z1[I]*ro+Z2[I]*sqrt(1-ro*ro))
X1[I2]<-Z1[I2]
X2[I2]<-(Z1[I2]*(-ro)+Z2[I2]*sqrt(1-ro*ro))
plot(X1,X2)
# calculate the p.value using the default settings (method="JLMn" 
# and alternative="two.sided")
lis.test(X1,X2)
# calculate the p.value using method="JLn" and 
# alternative="two.sided".
lis.test(X1,X2,method="JLn")
#
# Example 2: see subsection 4.3.2-Application 2 from main reference.
# (It requires the package VGAM) 
#
#require(VGAM)
#plot(coalminers$BW, coalminers$nBW)
#lis.test(coalminers$BW, coalminers$nBW, 
#alternative = "greater", method = "Ln")
#lis.test(coalminers$BW, coalminers$nBW, 
#alternative = "greater", method = "JLn")
#
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~longest increasing subsequence }
\keyword{ ~copula }% __ONLY ONE__ keyword per line
