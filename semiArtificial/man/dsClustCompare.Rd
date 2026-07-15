\name{dsClustCompare}
\alias{dsClustCompare}


\title{ Evaluate clustering similarity of two data sets}
\description{
   Similarity of two data sets is compared with a method using any of
   clustering comparison metrics: Adjusted Rand Index (ARI), Fowlkes-Mallows index(FM),
   Jaccard Index (J), or Variation of Information index (VI).
    }
\usage{
dsClustCompare(data1, data2) 
}

\arguments{
  \item{data1}{ A \code{data.frame} containing the reference data.}
  \item{data2}{ A \code{data.frame} with the same number and names of columns as \code{data1}.}
}
\details{
   The function compares data stored in \code{data1} with \code{data2} by first performing partitioning around medoids (PAM) 
   clustering on \code{data1}. 
   Instances from \code{data2} are than assigned to the cluster with the closest medoid. 
   In second step PAM clustering is performed on \code{data2} and instances from \code{data1} are assigned to the clusters with closest medoids. 
   The procedure gives us two clusterings on the same instances which we can compare using any of ARI, FM, J, or VI. 
   The higher the value of ARI/FM/J the more similar are the two data sets, and reverse is true for VI, where two perfectly matching partitions
   produce 0 score.    
   For random clustering ARI returns a value around zero (negative values are possible) and for perfectly matching clustering ARI is 1. 
   FM and J values are strictly in [0, 1].
   }
\value{
  The method returns a value of a list containing ARI and/or FM, depending on the parameters.
  }

\author{ Marko Robnik-Sikonja}

\seealso{
\code{\link{newdata.RBFgenerator}}.
}
\examples{
# use iris data set

# create RBF generator
irisGenerator<- rbfDataGen(Species~.,iris)

# use the generator to create new data
irisNew <- newdata(irisGenerator, size=200)

# compare ARI computed on clustering with original and new data
dsClustCompare(iris, irisNew)

}


\keyword{ datagen }
\keyword{ multivariate }