\name{JLn}
\alias{JLn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
JLn statistic, to test independence
}
\description{
It compute the JLn-statistic, from a bivariate sample of continuous random variables X and Y.}
\usage{
JLn(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{x, y}{numeric vectors of data values. x and y must have the same length.}

%  \item{y}{
%%     ~~Describe \code{y} here~~}
}
\details{See subsection 3.2.-Main reference. For sample sizes less than 20, the correction introduced in subsection 3.2 from main reference, with c = 0.4 was avoided.}
\value{
The value of the JLn-statistic.
}
\references{J. E. Garcia, V. A. Gonzalez-Lopez, Independence tests for continuous random variables based on the longest increasing subsequence, Journal of Multivariate Analysis (2014), \url{http://dx.doi.org/10.1016/j.jmva.2014.02.010}}
\author{
J. E. Garcia and V. A. Gonzalez-Lopez
}
%\note{
%%  ~~further notes~~}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~}
\examples{
## mixture of two bivariate normal, one with correlation 0.9 and
## the other with correlation -0.9 
#
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

# calculate the statistic
a<-JLn(X1,X2)
a

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~longest increasing subsequence }
\keyword{ ~copula }% __ONLY ONE__ keyword per line
