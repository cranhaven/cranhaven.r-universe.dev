\name{jaccardMat}
\alias{jaccardMat}
\title{
Jaccard similarity Matrix for bicluster output
}
\description{
computes jaccard similarity matrix for biclusters in two bicluster sets}
\usage{
jaccardMat(x, y, type=c("rows", "cols", "both"))
}

\arguments{
  \item{x}{
BiclustSet object containing row and column indicators of bicluster membership, number of biclusters
}
 \item{y}{
BiclustSet object containing row and column indicators of bicluster membership, number of biclusters
}
  \item{type}{
whether to compute Jaccard index in two dimensions, row dimension or column dimension 
}
}
\details{
This function operates on BiclustSet objects and computes pairwise similarity based on the common elements between biclusters. 
The Jaccard similarity score \eqn{ja} for two biclusters A and B is computed as \deqn{ja=\frac{|A\cap B|}{|A\cup B|}}}
\value{
matrix of pairwise Jaccard indices
}
\author{
Tatsiana Khamiakova
}
\seealso{
\code{\link{similarity}},\code{\link{kulczynskiMat}}, \code{\link{ochiaiMat}}, \code{\link{sensitivityMat}},
\code{\link{specificityMat}},\code{\link{sorensenMat}} 
}
