\name{getRFsizes}
\alias{getRFsizes}

\title{ Get sizes of the trees in RF }
\description{
Get numerical characteristics of the trees in a RF model related to the size
and depth.
}
\usage{
getRFsizes(model, type=c("size", "sumdepth")) 
}

\arguments{
  \item{model}{ The model structure as returned by \code{\link{CoreModel}}. }
  \item{type}{ The required characteristics. }
}
\details{
Size is the number of leaves. The sum of depths means the sum of the depth
of all leaves.
}
\value{
Numerical vector of the length equal to the number of trees in RF.
}

\author{ Petr Savicky}

\seealso{
\code{\link{CoreModel}},
\code{\link{CORElearn}}.
}

\examples{
# uses iris data set

# build random forests model with certain parameters, 
# do not make too many and too large trees
modelRF <- CoreModel(Species ~ ., iris, model="rf", 
              selectionEstimator="MDL", minNodeWeightRF=50, 
              rfNoTrees=50, maxThreads=1)

getRFsizes(modelRF) 

destroyModels(modelRF) # clean up
}

\keyword{ models }
\keyword{ regression }
\keyword{ tree }
\keyword{ multivariate }
\keyword{ classif }

