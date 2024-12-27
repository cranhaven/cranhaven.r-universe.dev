\name{qVarSel-package}
\alias{qVarSel-package}
\alias{qVarSel}
\docType{package}
\title{
Selecting Variables for Clustering and Classification
}
\description{
For a given data matrix A and cluster centers/prototypes collected in the matrix P, the functions described here select a subset of variables Q that mostly explains/justifies P as prototypes. The functions are useful to reduce the dimension of the data for classification as they discard masking variables for clustering.
}
\details{
\tabular{ll}{
Package: \tab qVarSel\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2024-11-15\cr
License: \tab gpl-2\cr
}
The package is useful to reduce the variable dimension for clustering. The example below shows the sequence of the operations. First, k-means can applied to the whole data sets, to calculate prototypes P. Then, distances between units U and P are calculated ans stored in a matrix D. Then, apply package subroutine q-VarSelH to select the most important variables. Apply EM optimization on data D for full clustering parameters estimation.
}
\author{
Stefano Benati

Maintainer: Stefano Benati <stefano.benati@unitn.it>
}
\references{
S. Benati, S. Garcia Quiles, J. Puerto "Mixed Integer Linear Programming and heuristic methods for feature selection in clustering", Journal of the Operational Research Society, 69:9, (2018), pp. 1379-1395
}
\keyword{ classif }
\keyword{ cluster }
\keyword{ optimize }
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

# reduce the dimension of a

sq = 1:(dim(a)[2])
vrb = sq[lsH$x > 0.01]
a_reduced = a[ ,vrb]

# use the EM methodology for effcient clustering on the reduced data

require(mclust)
sl1 <- Mclust(a_reduced, G = 2, modelName = "VVV")

}
