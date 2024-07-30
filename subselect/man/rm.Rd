\name{rm.coef}

\alias{rm.coef}

\title{Computes the RM coefficient for variable subset selection} 
\description{
Computes the RM coefficient, measuring the similarity of the
spectral decompositions of a p-variable data matrix, and of the matrix which
results from regressing all the variables on a subset of only k variables. 
}

\usage{rm.coef(mat, indices)}

\arguments{
  \item{mat}{the full data set's covariance (or correlation) matrix}

  \item{indices}{a numerical vector, matrix or 3-d array of integers
  giving the indices of the variables in the subset. If a matrix is
  specified, each row is taken to represent a different
  \emph{k}-variable subset. If a 3-d array is given, it is assumed
  that the third dimension corresponds to different cardinalities.}

}

\details{
Computes the RM coefficient that measures the similarity of the
spectral decompositions of a p-variable data matrix, and of the matrix which
results from regressing those variables on a subset (given by "indices") of
the variables.  Input data is expected in the form of a (co)variance or
correlation matrix. If a non-square matrix is given, it is assumed to
be a data matrix, and its correlation matrix is used as input.

The definition of the RM coefficient is as follows:
\deqn{RM = \sqrt{\frac{\mathrm{tr}(X^t P_v X)}{\mathrm{X^t X}}} }{RM = sqrt(tr(X' Pv X)/tr(X'X))}
where \eqn{X} is the full 
(column-centered) data matrix and \eqn{P_v}{Pv} is the matrix of 
orthogonal projections on the subspace spanned by a k-variable subset.

This definition is equivalent to:
\deqn{RM = \sqrt{\frac{\sum\limits_{i=1}^{p}\lambda_i
(r)_i^2}{\sum\limits_{j=1}^{p}\lambda_j}} }{RM = sqrt(sum_i(lambda_i
r_i^2)/sum_j(lambda_j))}
where \eqn{\lambda_i}{lambda_i} stands for the \eqn{i}-th largest
eigenvalue of the covariance matrix defined by X and
\eqn{r} stands for the multiple correlation between the
\code{i}-th Principal Component and the k-variable subset.

These definitions are also equivalent to the expression used in the
code, which only requires the covariance (or correlation) matrix of
the data under consideration.

The fact that \code{indices} can be a matrix or 3-d array allows for
the computation of the RM values of subsets produced by the search
functions \code{\link{anneal}}, \code{\link{genetic}} and
\code{\link{improve}} (whose output option \code{$subsets} are
matrices or 3-d arrays), using a different criterion (see the example
below).
}

\value{
The value of the RM coefficient.
}

\references{
 Cadima, J. and Jolliffe, I.T. (2001), "Variable Selection and the
Interpretation of Principal Subspaces", \emph{Journal of Agricultural,
Biological and Environmental Statistics}, Vol. 6, 62-79.

 McCabe, G.P. (1986) "Prediction of Principal Components by Variable
Subsets", \emph{Technical Report 86-19, Department of Statistics,
Purdue University}.

 Ramsay, J.O., ten Berge, J. and Styan, G.P.H. (1984), "Matrix
Correlation", \emph{Psychometrika}, 49, 403-423. 
}

\examples{

## An example with a very small data set.

data(iris3) 
x<-iris3[,,1]
rm.coef(var(x),c(1,3))
## [1] 0.8724422

## An example computing the RMs of three subsets produced when the
## anneal function attempted to optimize the RV criterion (using an
## absurdly small number of iterations).

data(swiss)
rvresults<-anneal(cor(swiss),2,nsol=4,niter=5,criterion="Rv")
rm.coef(cor(swiss),rvresults$subsets)

##              Card.2
##Solution 1 0.7982296
##Solution 2 0.7945390
##Solution 3 0.7649296
##Solution 4 0.7623326
}

\keyword{manip}
