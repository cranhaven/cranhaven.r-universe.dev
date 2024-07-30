\name{gcd.coef}

\alias{gcd.coef}

\title{Computes Yanai's GCD in the context of the variable-subset selection problem}

\description{
Computes Yanai's Generalized Coefficient of Determination for the
similarity of the subspaces spanned by a subset of
variables and a subset of the full data set's Principal
Components.
}

\usage{gcd.coef(mat, indices, pcindices =  NULL)}

\arguments{

  \item{mat}{the full data set's covariance (or correlation) matrix.}

  \item{indices}{a numerical vector, matrix or 3-d array of integers
  giving the indices of the variables in the subset. If a matrix is
  specified, each row is taken to represent a different
  \emph{k}-variable subset. If a 3-d array is given, it is assumed
  that the third dimension corresponds to different cardinalities.} 
 
  \item{pcindices}{a numerical vector of indices of Principal
  Components. By default, the first \emph{k} PCs are chosen, where
  \emph{k} is the cardinality of the subset of variables whose
  criterion value is being computed. \bold{If a vector of PCs is
  specified by the user, those PCs will be used for all cardinalities
  that were requested}.}  
}

\details{
Computes Yanai's Generalized Coefficient of Determination for the
similarity of the subspaces spanned by a subset of
variables (specified by \code{indices}) and a subset of the
full-data set's Principal Components (specified by \code{pcindices}).
 Input data is expected in the form of a (co)variance or
correlation matrix. If a non-square matrix is given, it is assumed to
be a data matrix, and its correlation matrix is used as input. The
number of variables (k) and of PCs (q) does not have to be the same.

Yanai's GCD is defined as:
\deqn{GCD = \frac{\mathrm{tr}(P_v\cdot P_c)}{\sqrt{k\cdot q}}}{GCD = tr(PvPc)/sqrt(k q)}
where \eqn{P_v}{Pv} and \eqn{P_c}{Pc} are the matrices of orthogonal
projections on the subspaces spanned by the k-variable subset and by
the q-Principal Component subset, respectively.

This definition is equivalent to:
\deqn{GCD = \frac{1}{\sqrt{k q}} \sum\limits_{i}(r_m)_i^2}{GCD = sum_i (r_i^2) / sqrt(k q)}
where \eqn{(r_m)_i}{r_i} stands for the multiple correlation between the
\code{i}-th Principal Component and the k-variable subset, and the sum
is carried out over the q PCs (i=1,...,q) selected.

These definitions are also equivalent to the expression used in the
code, which only requires the covariance (or correlation) matrix of
the data under consideration.

The fact that \code{indices} can be a matrix or 3-d array allows for
the computation of the GCD values of subsets produced by the search
functions \code{\link{anneal}}, \code{\link{genetic}} and
\code{\link{improve}} (whose output option \code{$subsets} are
matrices or 3-d arrays), using a different criterion (see the example
below).
}

\value{
The value of the GCD coefficient.
}

\references{
 Cadima, J. and Jolliffe, I.T. (2001), "Variable Selection and the
Interpretation of Principal Subspaces", \emph{Journal of Agricultural,
Biological and Environmental Statistics}, Vol. 6, 62-79.

 Ramsay, J.O., ten Berge, J. and Styan, G.P.H. (1984), "Matrix
Correlation", \emph{Psychometrika}, 49, 403-423. 
}

\examples{

## An example with a very small data set.

data(iris3) 
x<-iris3[,,1]
gcd.coef(cor(x),c(1,3))
## [1] 0.7666286
gcd.coef(cor(x),c(1,3),pcindices=c(1,3))
## [1] 0.584452
gcd.coef(cor(x),c(1,3),pcindices=1)
## [1] 0.6035127

## An example computing the GCDs of three subsets produced when the
## anneal function attempted to optimize the RV criterion (using an
## absurdly small number of iterations).

data(swiss)
rvresults<-anneal(cor(swiss),2,nsol=4,niter=5,criterion="Rv")
gcd.coef(cor(swiss),rvresults$subsets)

##              Card.2
##Solution 1 0.4962297
##Solution 2 0.7092591
##Solution 3 0.4748525
##Solution 4 0.4649259
}
\keyword{manip}
