\name{qVarSelLP}
\alias{qVarSelLP}
\title{
A Mixed Integer Linear Programming Formulation for the Variable Selection Problem
}
\description{
The function solves the mixed integer linear programming formulation of the variable selection problem.}
\usage{
qVarSelLP(d,
          q,
          binary = FALSE,
          write = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d}{ A 3-dimensional distance matrix, in which d[i,j,k] is the distance between unit i and prototype j, according to variable k.}
  \item{q}{The number of variables to selct.}
  \item{binary}{Set this value to TRUE if you wish to solve the problem with integer variables, or set it to FALSE if you just want to solve the continuous relaxation}
  \item{write}{Set this value to TRUE if you want that the optimization problem is exported in a file called qdistsel.lp}
}
\details{
The function solves the linear problem through the lpSolve solver available through the package lpSolveAPI. The linear programming formulation is the implementation of model P1 described in the paper below.
}
\value{
  \item{status}{The result of the optimization as output of the function solve() of library lpSolveAPI}
  \item{obj}{The value of the objective function}
  \item{x}{The value of the problem variables corresponding to the variable selection: x[j] = 1 means that variable j has been selected, 0 otherwise. If the continuous relaxation has been solved, the vector can contain fractional variables (most likely meaningless).}
}
\references{
S. Benati, S. Garcia Quiles, J. Puerto "Mixed Integer Linear Programming and heuristic methods for feature selection in clustering", Journal of the Operational Research Society, 69:9, (2018), pp. 1379-1395
}
\author{
Stefano Benati
}
\note{
The computational time to solve an integer programming problem can easily become exponential, therefore be carefull when set variable "binary" to TRUE, as you could wait days even to get the solution of a small scale problem.
Even though the continuos version can contain fractional variables, comparing the objective functions of subroutines qVarSelH and qVarSelLP is a certificate of the solution quality.
}


\seealso{
lpSolveAPI
}
\examples{
# Simulated data with 100 units, 10 true variables,
# 10 masking variables, 2 hidden clusters

n1 = 50
n2 = 50
n_true_var = 10
n_mask_var = 10
g1 = matrix(rnorm(n1*n_true_var, 0, 1), ncol = n_true_var)
g2 = matrix(rnorm(n2*n_true_var, 2, 1), ncol = n_true_var)
m1 = matrix(runif((n1 + n2)*n_mask_var, min = 0, max = 5), ncol = n_mask_var)
a = cbind(rbind(g1, g2), m1)

## calculate data prototypes using k-means

sl2 <- kmeans(a, 2, iter.max = 100, nstart = 2)
p = sl2$centers

## calculate distances between observations and prototypes
## Remark: d is a 3-dimensions matrix

d = PrtDist(a, p)

## Select 10 most representative variables, use heuristic

lsH <- qVarSelH(d, 10, maxit = 200)

## Select 10 variables, use linear relaxation

require(lpSolveAPI)
lsC <- qVarSelLP(d, 10)

## check optimality

if (abs(lsH$obj - lsC$obj) < 0.001)
  message = "Heuristic Solution is Optimal"


}
\keyword{ classif }
\keyword{ cluster }
\keyword{ optimize }
