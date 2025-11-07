\name{simulateData2}
\alias{simulateData2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Simulate bivariate data for funLBM}
\description{
Simulate bivariate data according to the funLBM model with K=4 groups for rows and L=3 groups for columns.
}
\usage{
simulateData2(n = 100, p = 100, t = 30)
}
\arguments{
  \item{n}{
The number of rows (individuals) of the simulated data array,
}
  \item{p}{
The number of columns (functional variables) of the simulated data array,
}
  \item{t}{
The number of measures for the functions of the simulated data array.
}
}
\value{
The resulting object contains:
\item{data1}{ data array of size n x p x t for first variable}
\item{data2}{ data array of size n x p x t for second variable}
\item{row_clust}{Group memberships of rows}
\item{col_clust}{Group memberships of columns}
}
\references{
C. Bouveyron, L. Bozzi, J. Jacques and F.-X. Jollois, The Functional Latent Block Model for the Co-Clustering of Electricity Consumption Curves, Journal of the Royal Statistical Society, Series C, 2018 (https://doi.org/10.1111/rssc.12260).
}
\seealso{
\code{\link{funLBM}}
}
\examples{
# Simulate data and co-clustering
set.seed(12345)
X = simulateData2(n = 50, p = 50, t = 15)

\donttest{
# Co-clustering with funLBM
out = funLBM(list(X$data1,X$data2),K=4,L=3)

# Visualization of results
plot(out,type='blocks')
plot(out,type='proportions')
plot(out,type='means')

# Evaluating clustering results
ari(out$col_clust,X$col_clust)
ari(out$row_clust,X$row_clust)
}
}
