\name{lis}
\alias{lis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Longest increasing subsequence for a univariate sample}
\description{
It compute the size of the longest increasing subsequence from a sample of a (continuous) random variable.}
\usage{
lis(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{numeric vector of data values.}
}
\details{
See example 2.1-Main reference.}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
    Integer, the size of the longest increasing subsequence.
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{J. E. Garcia, V. A. Gonzalez-Lopez, Independence tests for continuous random variables based on the longest increasing subsequence, Journal of Multivariate Analysis (2014), \url{http://dx.doi.org/10.1016/j.jmva.2014.02.010} }
\author{J. E. Garcia and V. A. Gonzalez-Lopez}
%\note{
%%  ~~further notes~~}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~}
\examples{
#see Example 2.1 (reference)
a<-lis(c(3,6,1,7,4,2,5,8))
a
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~longest increasing subsequence }
\keyword{ ~copula }% __ONLY ONE__ keyword per line
