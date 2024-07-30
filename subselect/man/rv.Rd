\name{rv.coef}

\alias{rv.coef}

\title{Computes the RV-coefficient applied to the variable subset selection problem} 

\description{
Computes the RV coefficient, measuring the similarity 
 (after rotations, translations and global re-sizing) of two
configurations
of n points given by: (i) observations on each of p variables,  and 
(ii) the regression of those p observed variables on a subset  of 
the variables. 
}

\details{
  Input data is expected in the form of a (co)variance or
correlation matrix of the full data set. If a non-square matrix is
given, it is assumed to 
be a data matrix, and its correlation matrix is used as input.
The subset of variables on which the full data set will be regressed
is given by \code{indices}.

The RV-coefficient, for a (coumn-centered)
data matrix (with p variables/columns) X,
and for the regression of these columns on a k-variable subset, is
given by:
\deqn{RV = \frac{\mathrm{tr}(X X^t \cdot (P_v X)(P_v X)^t)}
{\sqrt{\mathrm{tr}((X X^t)^2) \cdot \mathrm{tr}(((P_v X) (P_v X)^t)^2)}
}}{RV = tr(X X' (PvX)(PvX)') / sqrt(tr(XX' XX') tr((PvX)(PvX)' (PvX)(PvX)'))}
where \eqn{P_v}{Pv} is the matrix of orthogonal projections on the
subspace defined by the k-variable subset.

This definition is equivalent to the expression used in the
code, which only requires the covariance (or correlation) matrix of
the data under consideration.

The fact that \code{indices} can be a matrix or 3-d array allows for
the computation of the RV values of subsets produced by the search
functions \code{\link{anneal}}, \code{\link{genetic}} and
\code{\link{improve}} (whose output option \code{$subsets} are
matrices or 3-d arrays), using a different criterion (see the example
below).
}

\usage{rv.coef(mat, indices)}

\arguments{
  \item{mat}{the full data set's covariance (or correlation) matrix}

  \item{indices}{a numerical vector, matrix or 3-d array of integers
  giving the indices of the variables in the subset. If a matrix is
  specified, each row is taken to represent a different
  \emph{k}-variable subset. If a 3-d array is given, it is assumed
  that the third dimension corresponds to different cardinalities.}
}

\value{
The value of the RV-coefficient.
}

\references{
 Robert, P. and Escoufier, Y. (1976), "A Unifying tool for linear
multivariate statistical methods: the RV-coefficient", \emph{Applied
Statistics}, Vol.25, No.3, p. 257-265.
}

\examples{

# A simple example with a trivially small data set

data(iris3) 
x<-iris3[,,1]
rv.coef(var(x),c(1,3))
## [1] 0.8659685


## An example computing the RVs of three subsets produced when the
## anneal function attempted to optimize the RM criterion (using an
## absurdly small number of iterations).

data(swiss)
rmresults<-anneal(cor(swiss),2,nsol=4,niter=5,criterion="Rm")
rv.coef(cor(swiss),rmresults$subsets)

##              Card.2
##Solution 1 0.8389669
##Solution 2 0.8663006
##Solution 3 0.8093862
##Solution 4 0.7529066
}

\keyword{manip}
