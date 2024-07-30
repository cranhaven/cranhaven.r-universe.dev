\name{ccr12.coef}

\alias{ccr12.coef}

\title{First Squared Canonical Correlation  for a multivariate linear hypothesis} 
\description{
Computes the first squared canonical correlation.  
The maximization of this criterion is equivalent to the maximization of the Roy first root.

}

\usage{ccr12.coef(mat, H, r, indices,
tolval=10*.Machine$double.eps, tolsym=1000*.Machine$double.eps)} 

\arguments{
  \item{mat}{the Variance or Total sums of squares and products matrix for the full data set.}

  \item{H}{the Effect description sums of squares and products matrix (defined in the same way as the \code{mat} matrix).}

  \item{r}{the Expected rank of the H matrix. See the \code{Details} section below.}

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
Different kinds of statistical methodologies are considered within the
framework, of a multivariate linear model:
\deqn{X = A \Psi + U}{X = A B + U} where \eqn{X} is the (nxp) data matrix of
original variables, \eqn{A} is a known (nxp) design matrix, 
\eqn{\Psi}{B} an (qxp) matrix of unknown parameters and \eqn{U} an (nxp)
matrix of residual vectors. 
The \eqn{ccr_1^2}{ccr12}  index is related to the traditional test statistic
(the Roy first root) and  measures the contribution of each subset to
an Effect characterized by the violation of a linear hypothesis of the form 
\eqn{C \Psi = 0}{C B = 0}, where \eqn{C} is a known cofficient matrix
of rank r.  The Roy first root is the first eigen value of \eqn{HE^{-1}}, where
\eqn{H} is the Effect matrix and \eqn{E} is the Error matrix. 
 The index \eqn{ccr_1^2}{ccr12} is related to the Roy first root
(\eqn{\lambda_1}) by: 
\deqn{ccr_1^2 =\frac{\lambda_1}{1+\lambda_1}}{ccr1^2 =\lambda_1/(1+\lambda_1)} 


The fact that \code{indices} can be a matrix or 3-d array allows for
the computation of the \eqn{ccr_1^2}{ccr12} values of subsets produced
by the search 
functions \code{\link{anneal}}, \code{\link{genetic}}, 
\code{\link{improve}}  and
\code{\link{anneal}}  (whose output option \code{$subsets} are
matrices or 3-d arrays), using a different criterion (see the example
below).

}

\value{
The value of the \eqn{ccr_1^2}{ccr12} coefficient.
}

\examples{

## 1) A Linear Discriminant Analysis example with a very small data set. 
## We considered the Iris data and three groups, 
## defined by species (setosa, versicolor and virginica).

data(iris)
irisHmat <- ldaHmat(iris[1:4],iris$Species)
ccr12.coef(irisHmat$mat,H=irisHmat$H,r=2,c(1,3))
## [1]  0.9589055

## ---------------------------------------------------------------

## 2) An example computing the value of the ccr1_2 criteria for two  
## subsets produced when the anneal function attempted to optimize
## the zeta_2 criterion (using an absurdly small number of iterations).

zetaresults<-anneal(irisHmat$mat,2,nsol=2,niter=2,criterion="zeta2",
H=irisHmat$H,r=2)
ccr12.coef(irisHmat$mat,H=irisHmat$H,r=2,zetaresults$subsets)

##              Card.2
##Solution 1 0.9526304
##Solution 2 0.9558787

## ---------------------------------------------------------------

}

\keyword{manip}
