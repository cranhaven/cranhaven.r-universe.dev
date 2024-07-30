\name{tau2.coef}

\alias{tau2.coef}

\title{Computes the Tau squared coefficient for a multivariate linear hypothesis} 
\description{
Computes the Tau squared  index of "effect magnitude".  
The maximization of this criterion is equivalent to the minimization of Wilk's lambda statistic.

}

\usage{tau2.coef(mat, H, r, indices,
tolval=10*.Machine$double.eps, tolsym=1000*.Machine$double.eps)}

\arguments{
  \item{mat}{the Variance or Total sums of squares and products matrix for the full data set.}

  \item{H}{the Effect description sums of squares and products matrix (defined in the same way as the \code{mat} matrix).}

  \item{r}{the Expected rank of the H matrix. See the \code{Details} below.}

  \item{indices}{a numerical vector, matrix or 3-d array of integers
  giving the indices of the variables in the subset. If a matrix is
  specified, each row is taken to represent a different
  \emph{k}-variable subset. If a 3-d array is given, it is assumed
  that the third dimension corresponds to different cardinalities.}

  \item{tolval}{the tolerance level to be used in checks for 
ill-conditioning and positive-definiteness of the 'total' and
'effects' (H) matrices. Values smaller than \code{tolval} are
considered equivalent to zero.}

 \item{tolsym}{the tolerance level for symmetry of the
   covariance/correlation/total matrix and for the effects (\code{H})
   matrix. If corresponding matrix entries differ by more than this
   value, the input matrices will be considered asymmetric and execution
   will be aborted. If corresponding entries are different, but by less
   than this value, the input matrix will be replaced by its symmetric
   part, i.e., input matrix A becomes (A+t(A))/2.}
}

\details{
Different kinds of statistical methodologies are considered within the framework, of a multivariate
linear model:
\deqn{X = A \Psi + U}{X = A B + U} where \eqn{X} is the (nxp) data
matrix of original variables, \eqn{A} is a known (nxp) design matrix, 
\eqn{\Psi}{B} an (qxp) matrix of unknown parameters and \eqn{U} an
(nxp) matrix of residual vectors. 
The \eqn{\tau^2}  index is related to the traditional test statistic
(Wilk's lambda statistic) and  
measures the contribution of each subset to an Effect characterized by
the violation of a linear hypothesis of the form 
\eqn{C \Psi = 0}{C B = 0}, where \eqn{C} is a known cofficient matrix
of rank r. The Wilk's lambda statistic (\eqn{\lambda}) is given by: 
 \deqn{\Lambda=\frac{det(E)}{det(T)}}{\Lambda = det(E)/det(T)} where
\eqn{E} is the Error matrix and \eqn{T} is the Total matrix. 
 The index \eqn{\tau^2} is related to the Wilk's lambda statistic
(\eqn{\Lambda})   by:  
 \deqn{\tau^2 = 1 - \lambda^{(1/r)}} where \eqn{r} is the
rank of \eqn{H} the Effect matrix.



The fact that \code{indices} can be a matrix or 3-d array allows for
the computation of the \eqn{\tau^2} values of subsets produced by the search
functions \code{\link{anneal}}, \code{\link{genetic}}, \code{\link{improve}} and
\code{\link{eleaps}} (whose output option \code{$subsets} are
matrices or 3-d arrays), using a different criterion (see the example
below).
}

\value{
The value of the \eqn{\tau^2}{tau2} coefficient.
}

\examples{

## ---------------------------------------------------------------

## 1) A Linear Discriminant Analysis example with a very small data set. 
## We considered the Iris data and three groups, 
## defined by species (setosa, versicolor and virginica).
 
data(iris)
irisHmat <- ldaHmat(iris[1:4],iris$Species)
tau2.coef(irisHmat$mat,H=irisHmat$H,r=2,c(1,3))
## [1] 0.8003044

## ---------------------------------------------------------------

## 2) An example computing the value of the tau_2 criterion for two  
## subsets produced when the anneal function attempted to optimize  
## the xi_2 criterion (using an absurdly small number of iterations).

xiresults<-anneal(irisHmat$mat,2,nsol=2,niter=2,criterion="xi2",
H=irisHmat$H,r=2)
tau2.coef(irisHmat$mat,H=irisHmat$H,r=2,xiresults$subsets)

##              Card.2
##Solution 1 0.8079476
##Solution 2 0.7907710

## ---------------------------------------------------------------

}

\keyword{manip}
