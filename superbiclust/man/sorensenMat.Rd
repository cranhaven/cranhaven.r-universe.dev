\name{sorensenMat}
\alias{sorensenMat}
\title{
Sorensen similarity Matrix for bicluster output
}
\description{
Computes Sorensen similarity matrix for biclusters in two bicluster sets}
\usage{
sorensenMat(x, y, type=c("rows", "cols", "both"))
}

\arguments{
  \item{x}{
BiclustSet object containing row and column indicators of bicluster membership, number of biclusters
}
 \item{y}{
BiclustSet object containing row and column indicators of bicluster membership, number of biclusters
}
  \item{type}{
whether to compute Sorensen index in two dimensions, row dimension or column dimension 
}
}
\details{
This function operates on BiclustSet objects and computes pairwise similarity based on the common elements between biclusters. 
Sorensen similarity score \eqn{so} for two biclusters A and B is computed as 
\deqn{so = \frac{2|A\cap B|}{|A| + |B|}}
}
\value{
matrix of pairwise Sorensen indices
}
\author{
Tatsiana Khamiakova
}
\seealso{
\code{\link{similarity}},\code{\link{jaccardMat}}, \code{\link{ochiaiMat}}, \code{\link{sensitivityMat}},
\code{\link{specificityMat}},\code{\link{kulczynskiMat}} 
}
