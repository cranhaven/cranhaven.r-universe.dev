\name{ochiaiMat}
\alias{ochiaiMat}
\title{
Ochiai similarity Matrix for bicluster output
}
\description{
Computes Ochiai similarity matrix for biclusters in two bicluster sets}
\usage{
ochiaiMat(x, y, type=c("rows", "cols", "both"))
}

\arguments{
  \item{x}{
BiclustSet object containing row and column indicators of bicluster membership, number of biclusters
}
 \item{y}{
BiclustSet object containing row and column indicators of bicluster membership, number of biclusters
}
  \item{type}{
whether to compute Ochiai index in two dimensions, row dimension or column dimension 
}
}
\details{
This function operates on BiclustSet objects and computes pairwise similarity based on the common elements between biclusters. 
The Ochiai similarity score \eqn{ja} for two biclusters A and B is computed as 
		\deqn{oc=\frac{|A\cap B|}{\sqrt{|A| |B|}}}
}
\value{
matrix of pairwise Ochiai indices
}
\author{
Tatsiana Khamiakova
}
\seealso{
\code{\link{similarity}},\code{\link{jaccardMat}}, \code{\link{ochiaiMat}}, \code{\link{sensitivityMat}},
\code{\link{specificityMat}},\code{\link{sorensenMat}} 
}
