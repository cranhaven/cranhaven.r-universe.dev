\name{similarity}
\alias{similarity}
\title{
Similarity Matrix for bicluster output
}
\description{
computes similarity matrix for the biclustering output based on one of the pairwise similarity indices of biclusters in a given bicluster set}
\usage{
similarity(x, index = "jaccard", type="rows")
}

\arguments{
  \item{x}{
BiclustSet object containing row and column indicators of bicluster membership, number of biclusters
}
  \item{index}{
similarity index for the biclusters in output
}
  \item{type}{
whether to perform similarity in two dimensions, "both" (recommended for biclustering), row dimension,
"rows" (default, requires less computations) or column dimension "cols"
}
}
\details{
This function operates on BiclustSet object and computes pairwise similarity based on the common elements between biclusters. 
\code{type} variable controls whether similarity index is constructed for all elements, or in one dimension (rows or columns) only. 
In general, similarity indices for one dimension (row or column) are higher than for two-dimensions. 
Several options for similarity indices are offered: jaccard (default), kulczynski, sensitivity, specificity, sorensen  and ochiai indices.
}
\value{
a "similarity" object containing similarity matrix
}
\author{
Tatsiana Khamiakova
}
\seealso{
\code{\link{HCLtree}}, \code{\link{plotSuper}} , \code{\link{jaccardMat}},\code{\link{kulczynskiMat}}, \code{\link{ochiaiMat}}, \code{\link{sensitivityMat}},
\code{\link{specificityMat}},\code{\link{sorensenMat}} 
}
\examples{
#compute sensitivity for BiMAX biclusters
 test <- matrix(rnorm(5000), 100, 50)
 test[11:20,11:20] <- rnorm(100, 3, 0.1)
 test[17:26,21:30] <- rnorm(100, 3, 0.1)
 testBin <- binarize(test,2)
 res <- biclust(x=testBin, method=BCBimax(), minr=4, minc=4, number=10)
 BiMaxBiclustSet <-  BiclustSet(res)
 SensitivityMatr<- similarity(BiMaxBiclustSet,index="sensitivity", type="rows")
 SensitivityMatr
}
