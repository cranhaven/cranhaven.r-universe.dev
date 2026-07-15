\name{getCoreModel}
\alias{getCoreModel}

\title{ Conversion of model to a list  }
\description{
  Function converts given model from internal structures in C++ to R's data structures.  
 }
\usage{
getCoreModel(model) 
}

\arguments{
  \item{model}{ The model structure as returned by \code{\link{CoreModel}}. }
  }
\details{
   The function converts the model referenced by \code{model} from C++ internal structures 
   to R's lists. Currently it is implemented only for random forests models.
}
\value{
  For random forest a resulting list contains first all the information on the forest level, 
  followed by the list of trees. For each tree the nodes are recursively nested with indication 
  of node type (leaf or internal node) and than required information for that data type.
}

\author{ Marko Robnik-Sikonja}

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
              rfNoTrees=5, maxThreads=1)
print(modelRF)

# get the structure of the forest 
forest <- getCoreModel(modelRF) 
# forest

destroyModels(modelRF) # clean up
 
}

\keyword{ models }
\keyword{ regression }
\keyword{ tree }
\keyword{ multivariate }
\keyword{ classif }
