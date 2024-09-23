\name{CopTestdm}
\alias{CopTestdm}
\title{Test for break detection in copula with change-point known in the marginal cumulative distributions}

\description{Give the p-value of the test based on the sequential empirical copula process when a break occurs in the marginal cumulative distributions at time m known. 
}

\usage{
CopTestdm(X,b=1,M=1000)
}

\arguments{
\item{X}{a (non-empty) numeric matrix of \eqn{d}-dimensional data values, greater than 2. Each row of the matrix contains one mutlivariate data.}
\item{M}{a strictly positive integer (default \eqn{M=1000}) specifying the number of bootstrap repetitions. A sequence of i.i.d. normal multipliers is generated. For no independent or normal multipliers, you also can specify the matrix of multipliers as \eqn{M}. The specified matrix is required to have an identical number of rows than \eqn{X}.}
\item{b}{a single value or a vector of real values on (0,1] indicating the location(s) of the potential break time(s) in marginal cumulative distribution functions. You can specify \eqn{b=1} (default) for any break time. In this case, the test corresponds to the test described in the third reference using the hat version of bootstrap replications.}
}

\details{Note that the e.c.d.f.s \eqn{F_{k:l,j}} appearing in the construction of pseudo-values (as defined in the section 2 of the first reference) evaluated from the sub-samble \eqn{X_{kj},\ldots, X_{lj}}  are multiplied by \eqn{\frac{l-k+1}{l-k+2}}. Discussions about this subject can be found in the third reference. For serially dependent data, you need to specify dependent multipliers, see the second and third reference for details. 
}

\value{
A list with class \code{htest} containing the following components:
\item{m}{the value of the potential break times in marginal cumulative distribution functions.}
\item{data.name}{a character string giving the name of the data.}
\item{method}{a character string indicating what type of change-point test was performed.}
\item{p.value}{the estimated p-value for the test.}
\item{statistic}{the value of the statistic \eqn{S_{nm}}.}
}
\references{
Tom Rohmer, Some results on change-point detection in cross-sectional dependence of multivariate data with changes in marginal distributions, Statistics & Probability Letters, Volume 119, December 2016, Pages 45-54, ISSN 0167-7152

A. Bucher and I. Kojadinovic (2016), A dependent multiplier bootstrap for the sequential empirical copula process under strong mixing, Bernoulli 22:2, pages 927-968

A. Bucher, I. Kojadinovic, T. Rohmer and J. Segers (2014), Detecting changes in cross-sectional dependence in multivariate time series, Journal of Multivariate Analysis 132, pages 111-128
}
\author{Rohmer Tom
}




\examples{


#Example 1: under the nulle hypothesis 
#of an abrupt change in the m.c.d.f. at time m=50 and no change in the copula

n=100
m=50
sigma = matrix(c(1,0.4,0.4,1),2,2)
mean1 = rep(0,2)
mean2 = rep(4,2)

X=matrix(rep(0,n*2),n,2)

for(j in 1:n) X[j,]=t(chol(sigma))\%*\%rnorm(2)

X[1:m,] = X[1:m,]+mean1
X[(m+1):n,] = X[(m+1):n,]+mean2

CopTestdm(X,b=0.5)


\donttest{      
#Example 2: under the alternative hypothesis 
#of an abrupt change in the m.c.d.f at and in the copula time k=m=50  

n=100
m=50

mean1 = rep(0,2)
mean2 = rep(4,2)
sigma1 = matrix(c(1,0.2,0.2,1),2,2)
sigma2 = matrix(c(1,0.6,0.6,1),2,2)

X=matrix(rep(0,n*2),n,2)
for(j in 1:m) X[j,]=t(chol(sigma1))\%*\%rnorm(2) + mean1
for(j in (m+1):n) X[j,]=t(chol(sigma2))\%*\%rnorm(2) + mean2

CopTestdm(X,b=0.5)

  
#Example 3: under the alternative hypothesis 
#of abrupt changes in the m.c.d.f at times m=100 and 150 and in the copula at time k=50

n=200
m1 = 100
m2 = 150
k = 50

sigma1 = matrix(c(1,0.2,0.2,1),2,2)
sigma2 = matrix(c(1,0.6,0.6,1),2,2)

mean1 = rep(0,2)
mean2 = rep(2,2)
mean3 = rep(4,2)

X=matrix(rep(0,n*2),n,2)
for(j in 1:k) X[j,]=t(chol(sigma1))\%*\%rnorm(2)
for(j in (k+1):n) X[j,]=t(chol(sigma2))\%*\%rnorm(2)

X[1:m1,]=X[1:m1,]+mean1
X[(m1+1):m2,]=X[(m1+1):m2,]+mean2
X[(m2+1):n,]=X[(m2+1):n,]+mean3

CopTestdm(X,b=c(0.5,0.75))
}
}


\keyword{nonparametric}
\keyword{htest}
\keyword{multivariate}
