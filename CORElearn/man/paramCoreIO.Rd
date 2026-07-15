\name{paramCoreIO}
\alias{paramCoreIO}
\title{ Input/output of parameters from/to file }
\description{
  All the parameters of the given model are written directly to file,
  or read from file into model.
}
\usage{
paramCoreIO(model, fileName, io=c("read","write")) 
}

\arguments{
  \item{model}{ The model structure as returned by \code{\link{CoreModel}}. }
  \item{fileName}{ Name of the parameter file. }
  \item{io}{ Controls weather the parameters will be read or written. } 
  }
\details{
   The function uses the \code{model} structure as returned by \code{\link{CoreModel}} and
   reads or writes all its parameters from/to file.
   If parameter \code{io="read"} parameters are read from file \code{filename}.
   If parameter \code{io="write"} parameters are written to file \code{filename}.
}
\value{
  Returns invisible list with parameters passed to C function:
  \code{list(modelID, filename, io}.
}

\author{ Marko Robnik-Sikonja}

\keyword{ models }
\keyword{ regression }
\keyword{ nonlinear }
\keyword{ tree }
\keyword{ classif }

\seealso{
\code{\link{CORElearn}},
\code{\link{helpCore}}.
}

\examples{
# use iris data
# build random forests model with certain parameters
modelRF <- CoreModel(Species ~ ., iris, model="rf", 
              selectionEstimator="MDL",minNodeWeightRF=5,
              rfNoTrees=50, maxThreads=1)

# writes all the used parameters to file
paramCoreIO(modelRF, "parameters.par", io="write")
# and reads them back into the model
paramCoreIO(modelRF, "parameters.par", io="read")

# clean up for the sake of R package checks
file.remove("parameters.par")

destroyModels(modelRF) # clean up

}

