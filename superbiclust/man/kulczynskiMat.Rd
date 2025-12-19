\name{kulczynskiMat}
\alias{kulczynskiMat}
\title{
Kulczynski similarity Matrix for bicluster output
}
\description{
computes Kulczynski similarity matrix for biclusters in two bicluster sets}
\usage{
kulczynskiMat(x, y, type=c("rows", "cols", "both"))
}

\arguments{
  \item{x}{
BiclustSet object containing row and column indicators of bicluster membership, number of biclusters
}
 \item{y}{
BiclustSet object containing row and column indicators of bicluster membership, number of biclusters
}
 \item{type}{
whether to compute Kulczynski index in two dimensions, row dimension or column dimension 
}
}
\details{
This function operates on BiclustSet objects and computes pairwise similarity based on the common elements between biclusters. 
Kulczynski similarity score \eqn{ku} for two biclusters A and B is computed as 
		\deqn{ku = 2|A\cap B|\left(\frac{1}{|A|} + \frac{1}{|B|}\right)}
}
\value{
matrix of pairwise Kulczynski indices
}
\author{
Tatsiana Khamiakova
}
\seealso{
\code{\link{similarity}},\code{\link{jaccardMat}}, \code{\link{ochiaiMat}}, \code{\link{sensitivityMat}},
\code{\link{specificityMat}},\code{\link{sorensenMat}} 
}
