\name{destroyModels}
\alias{destroyModels}
\title{Destroy single model or all CORElearn models }
\description{
  Destroys internal representation of a given model or all constructed models.
  As side effect the memory used by the model(s) is freed.
  }
\usage{
destroyModels(model=NULL) 
}

\arguments{
  \item{model}{ The model structure as returned by \code{\link{CoreModel}}. The default value of NULL represents all generated models.}
  }
\details{
   The function destroys the \code{model} structure as returned by \code{\link{CoreModel}}.
   Subsequent work with this model is no longer possible. 
   If parameter \code{model=NULL} (default value) all generated models are destroyed and
   memory used by their internal representation is freed.
}
\value{
  There is no return value.
}

\author{ Marko Robnik-Sikonja, Petr Savicky }

\keyword{ models }
\keyword{ regression }
\keyword{ tree }
\keyword{ classif }

\seealso{
\code{\link{CORElearn}},
\code{\link{CoreModel}}.
}

\examples{
# use iris data set

# build random forests model with certain parameters
model <- CoreModel(Species ~ ., iris, model="rf", 
              selectionEstimator="MDL", minNodeWeightRF=5,
              rfNoTrees=100, maxThreads=1)

# prediction 
pred <- predict(model, iris, rfPredictClass=FALSE)
# print(pred)

# destruction of model's internal representation
destroyModels(model)

}

