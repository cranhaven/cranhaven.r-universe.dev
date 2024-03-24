\name{howmany_dependent}
\alias{howmany_dependent}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Number of correct rejections, for dependent test statistics}
\description{
Lower bounds for the number of correct rejections, for multiple tests of
associations with dependent test statistics.
}
\usage{
howmany_dependent(X, Y, alpha = 0.05, test = wilcox.test,
     alternative = "two.sided", n.permutation=round(20/alpha) )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{ a n*p matrix, where each of the p columns contains n
    observations}
  \item{Y}{ a factor or numerical vector of length n, containing a binary class variable }
  \item{alpha}{the level, a scalar in [0,1]  }
  \item{test}{ the test to be used }
  \item{alternative}{ an alternative for the test, supplied as an
    argument to function \code{test} }
  \item{n.permutation}{ the number of permutations to use to determine
    the bounding function }
}
\details{
  For multiple tests of associations (with possibly dependent test
  statistics), a lower bound for the number of correct rejections is
  calculated, which is valid simultaneously for all possible number of
  rejections and under arbitrary dependence between test statistics. The bound is monotonically increasing with the number of made
  rejections.

  The matrix \code{X} contains the observations, while \code{Y} contains binary
  class labels. For each hypothesis k=1,...p, a
  p-value is calculated internally according to supplied function
  \code{test}, with first argument \code{X[Y==0,k]}, second argument \code{X[Y==1,k]}, and
  additional argument \code{alternative} (if the
  class labels are not 0 and 1, they are converted accordingly). The object returned by \code{test} has
  to have a component \code{p.value}, containing (perhaps unsurprisingly) the p-value
  of the corresponding test.

  The focus of the current implementation is on portability, not speed,
  and computations might take some time.
}
\value{
  An object of class \code{howmany}, for which summary, plot, and print
  methods are available. 
  
  The lower bound for the number of correct rejections (as a function of
  the number of rejections) can be accessed with the function \code{\link{lowerbound}}.
}
\references{N. Meinshausen and J. Rice (2006) 
"Estimating the proportion of false discoveries among a large number of independently
  tested hypotheses", Annals of Statistics 34(1), 373-393

N. Meinshausen (2006)
"False discovery control for multiple tests of association under general dependence", Scandinavian Journal of Statistics 33(2), 227-237

N. Meinshausen and P. Buhlmann (2005)
"Lower bounds for the number of false null hypotheses for multiple testing of associations",
Biometrika 92(4), 893-907 
}
\author{ Nicolai Meinshausen, \email{nicolai@stat.berkeley.edu}}


\seealso{ \code{\link{lowerbound}} for extracting the number of
  correct rejections (as a function of the number of made rejections).

\code{\link{howmany}} for similar functionality for multiple
tests of associations, where test statistics are independent. }

\examples{

## Warning: running example might take a
## few minutes of computing time...

## create observation matrix X 
## for p=200 hypotheses with n=40 observations
p <- 200
n <- 40
Indep <- matrix( rnorm(p*n) , ncol= p )  
C <- diag(p); C <- C+matrix( 0.01*rbinom(p^2,1,0.2) , ncol=p ) 
X <- Indep\%*\%C

## create binary class variables Y
Y <- c( rep(0,round(n/2)), rep(1,n-round(n/2)) )

## 100 false null hypotheses (random effects)
for (k in 1:100){  X[Y==1, k] <- X[Y==1, k] + rnorm(1) }



## compute object of class 'howmany' and print the result
(object <- howmany_dependent(X,Y))

## extract the lower bound
(lower <- lowerbound(object))

## plot the result
plot(object)

## for comparison: number of rejections with Bonferroni correction
(bonf <- sum(object$pvalues<0.05/p))

}
\keyword{htest}
