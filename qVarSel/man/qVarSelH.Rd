\name{qVarSelH}
\alias{qVarSelH}
\title{
Selection of Variables for Clustering or Data Dimension Reduction
}
\description{ The function implements the q-Vars heuristic described in the reference below. Given a 3-dimension matrix D, with d[i,j,k] being the distance between statistic unit i and prototype j measured through variable k, the function calculates the set of variables of cardinality q that mostly explains the prototypes.
}
\usage{
qVarSelH(d,
         q,
         maxit = 100)
}
\arguments{
  \item{d}{A numeric 3-dimensional matrix where elements d(i,j,k) are the     distances between observation i and cluster center/prototype j, that are measured through variable k.
  }
  \item{q}{A positive scalar, that is the number of variables to select}
  \item{maxit}{A positive scalar, that is the maximum number of iteration allowed}
}
\details{
The heuristic repeatedly selects a set of variables and then allocates units to prototypes, while a local optimum is reached. Random restart is used to continue the search until the maximum number of iteration is reached.
}
\value{
\item{obj}{The value of the objective function}
\item{x}{A 0-1 vector describing wheter variable k is selected: If x[k] = 1 then k is selected}
\item{ass}{A vector of assignment of units to clusters: if ass[i] = j then unit i is assigned to the cluster represented by center/prototype j }
\item{bestit}{The iteration in which the optimal solution is found}
}
\references{
S. Benati, S. Garcia Quiles, J. Puerto "Mixed Integer Linear Programming and heuristic methods for feature selection in clustering", Journal of the Operational Research Society, 69:9, (2018), pp. 1379-1395
}
\author{
Stefano Benati
}
\note{
The methodology is heuristic and some steps are random. It may be the case that different runs provide different solutions.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
qVarSelLP
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



}
\keyword{ classif }
\keyword{ cluster }
\keyword{ optimize }
