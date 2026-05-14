\name{betaCV}
\alias{betaCV}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
BetaCV
}
\description{
function to calculates the BetaCV.
}
\usage{
betaCV(clust,dist)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{clust}{
Determine in which cluster a data is belonged. clust should be a numeric, 0 indicates a noise and cluster start at 1.
}
  \item{dist}{
Distance matrix
}
}
\details{
BetaCV measures how well the clusters based on compactness (intra-cluster distance) and separability (inter-cluster distance). BetaCV is the ratio between the average of intra-cluster distance to the average of inter-claster distance. The smaller BetaCV value indicates the better the clustering.
}
\value{
This function returns the betaCV value.
}
\references{
University of Illinois. (2020, January 10). 6.1 Methods for Clustering Validation. Retrieved from Coursera: https://www.coursera.org/lecture/cluster-analysis/6-1-methods-for-clustering-validation-k59pn
}
\author{
Fella Ulandari and Robert Kurniawan
}
\seealso{
https://www.coursera.org/lecture/cluster-analysis/6-1-methods-for-clustering-validation-k59pn
}
\examples{
x <- runif(20,-1,1)
y <- runif(20,-1,1)
dataset <- cbind(x,y)
l <- lsdbc(dataset, 7,3,"euclidean")

dmat <- as.matrix(dist(dataset,"euclidean"))
betaCV(l$cluster,dmat)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{betaCV}
\keyword{cluster validation}
