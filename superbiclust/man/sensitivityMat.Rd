\name{sensitivityMat}
\alias{sensitivityMat}
\title{
Sensitivity Matrix for bicluster output
}
\description{
Computes sensitivity matrix for biclusters in two bicluster sets}
\usage{
sensitivityMat(x, y, type=c("rows", "cols", "both"))
}

\arguments{
  \item{x}{
BiclustSet object containing row and column indicators of bicluster membership, number of biclusters
}
 \item{y}{
BiclustSet object containing row and column indicators of bicluster membership, number of biclusters
}
\item{type}{
whether to compute sensitivity in two dimensions, row dimension or column dimension 
}
}
\details{
This function operates on BiclustSet objects and computes pairwise similarity based on the common elements between biclusters. 
Sensitivity inclusion score \eqn{sen} of biclusters A and B is computed as 
		\deqn{sen=\frac{|A\cap B|}{|A|}}
}
\value{
matrix of pairwise sensitivities
}
\author{
Tatsiana Khamiakova
}
\seealso{
\code{\link{similarity}},\code{\link{jaccardMat}}, \code{\link{ochiaiMat}}, \code{\link{kulczynskiMat}},
\code{\link{specificityMat}},\code{\link{sorensenMat}} 
}
