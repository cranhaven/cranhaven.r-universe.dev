\name{PrtDist}
\alias{PrtDist}
\title{
Calculation of the distances between units and centers
}
\description{
Given a data set A, with a[i,k] be the measure of variable k on unit i, and a prototype set P, with p[j,k] be the measure of variable k on prototype j,
the function calculate d[i,j,k], the i,j distance according variable k.
}
\usage{
PrtDist(a,
        p)
}
\arguments{
  \item{a}{ Matrix with n rows (units) and m columns (variables)}
  \item{p}{ Matrix with g rows (prototypes) and m columns (variables)}
}

\details{d[i,j,k] is the squared distance: d[i,j,k] = (a[i,k] - p[j,k])^2
}
\value{d: The 3-dimensional matrix of i,j,k distances}

\references{
S. Benati, S. Garcia Quiles, J. Puerto "Mixed Integer Linear Programming and heuristic methods for feature selection in clustering", Journal of the Operational Research Society, 69:9, (2018), pp. 1379-1395
}
\author{
Stefano Benati
}
\note{
The function has been written to simplify the code examples. It has been written as a R script with minimal vectorization, therefore it is not computationally much efficient. Moreover d(i,j,k) is the square of differences and users may prefer to employ other dissimilarity measures. In that case, they should better write their own function.
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

}
\keyword{ classif }
\keyword{ cluster }
\keyword{ optimize }
