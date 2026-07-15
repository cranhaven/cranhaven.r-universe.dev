\name{saveRF}
\alias{saveRF}
\alias{loadRF}
\title{ Saves/loads random forests model to/from file }
\description{
  \code{saveRF}: the internal structure of given random forests model is saved to file.
  \code{loadRF}: the internal structure of random forests model is loaded from given file and a model is created and returned.
  }
\usage{
saveRF(model, fileName) 
loadRF(fileName) 
 }

\arguments{
  \item{model}{ The model structure as returned by \code{\link{CoreModel}}. }
  \item{fileName}{ Name of the file to save/load the model to/from. }
 }
\details{
   The function \code{saveRF} saves the internal structure of given random forests model to file. 
   The structures from C++ code are stored to the file with specified file, while internal structures
   from R are stored to file named \code{fileName.Rda}.   
   The \code{model} must be a valid structure returned by \code{\link{CoreModel}}.
   
   The function \code{loadRF} loads the internal structure of random forests saved in a specified files and
   returns access to it. 
   }
\value{
  \code{saveRF} invisibly returns some debugging information, while \code{loadRF}
     returns a loaded model as a list, similarly to \code{\link{CoreModel}}.
}

\author{ Marko Robnik-Sikonja}

\keyword{ models }
\keyword{ regression }
\keyword{ nonlinear }
\keyword{ tree }
\keyword{ classif }

\seealso{
\code{\link{CORElearn}},
\code{\link{CoreModel}}.
}

\examples{
# use iris data set


# build random forests model with certain parameters
modelRF <- CoreModel(Species ~ ., iris, model="rf", 
              selectionEstimator="MDL",minNodeWeightRF=5,
              rfNoTrees=100, maxThreads=1)
print(modelRF)

# prediction with node distribution
pred <- predict(modelRF, iris, rfPredictClass=FALSE, type="both")
# print(pred)


# saves the random forests model to file
saveRF(modelRF, "tempRF.txt")

# restore the model to another model
loadedRF = loadRF("tempRF.txt")

# prediction should be the same
predLoaded <- predict(loadedRF, iris, rfPredictClass=FALSE, type="both")
# print(predLoaded)
# sum of differences should be zero subject to numeric imprecision 
sum(pred$probabilities - predLoaded$probabilities) 

cat("Are predicted classes of original and retrieved models equal? ", 
                           all(pred$class == predLoaded$class), "\n" ) 
# cat("Are predicted probabilities of original and retrieved model equal? ", 
#                 all(pred$probabilities == predLoaded$probabilities), "\n" ) 

# clean up the models when no longer needed
destroyModels(modelRF) 
destroyModels(loadedRF) 

# clean up for the sake of R package checks
file.remove("tempRF.txt")
file.remove("tempRF.txt.Rda")



}

