\name{HCLtree}
\alias{HCLtree}
\title{
Hierarchical structure of bicluster output
}
\description{
Constructs and plots hierarchical tree of biclusters output based on the similarity matrix}
\usage{
HCLtree(x)
}

\arguments{
  \item{x}{
Similarity object containing pairwise similarity indices for all biclusters in the output
}
}

\details{
This function operates on a similarity matrix, which is converted to the distance between biclusters according to 
\eqn{dist(a,b)= 1-sim(a,b)}, where the smaller the distance, the higher is overlap in terms of rows and columns.
The tree is constructed using complete method and plotted. 
Further, the structure must be explored and robust or super-biclusters obtained after cutting the tree.
\code{identify} function can be applied to the hierarchical tree to see the partition and get the plots of biclusters. 
}
\value{
tree
}
\author{
Tatsiana Khamiakova
}
\seealso{
\code{\link{similarity}}, \code{\link{plotSuper}}
}
\examples{
#compute sensitivity for BiMAX biclusters
 test <- matrix(rnorm(5000), 100, 50)
 test[11:20,11:20] <- rnorm(100, 3, 0.1)
 test[17:26,21:30] <- rnorm(100, 3, 0.1)
 testBin <- binarize(test,2)
 res <- biclust(x=testBin, method=BCBimax(), minr=4, minc=4, number=10)
 BiMaxBiclustSet <-  BiclustSet(res)
 SensitivityMatr<- similarity(BiMaxBiclustSet,index="sensitivity")
#construct hierarchical clustering based on the sensitivities
HCLsensitivity <- HCLtree(SensitivityMatr) 
plot(HCLsensitivity, main="structure of bicluster solution")
}