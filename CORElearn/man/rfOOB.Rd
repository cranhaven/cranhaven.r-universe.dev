\name{rfOOB}
\alias{rfOOB}
\title{ Out-of-bag performance estimation for random forests}
\description{
  The method returns internal out-of-bag performance evaluation for given random forests \code{model}.
  }
\usage{
  rfOOB(model)
}
\arguments{
  \item{model}{ The model of type \code{rf} or \code{rfNear} as returned by \code{\link{CoreModel}}. } 
  }
\details{
  The method returns random forest performance estimations obtained via its out-of-bag sets. 
  The performance measures returned are classification accuracy, average classification margin, and correlation between trees in the forest.
  The classification margin is defined as the difference between probability of the correct class and probability of the most probable incorrect class.
  The correlation between models is estimated as the ratio between classification margin variance and 
  variance of the forest as defined in (Breiman, 2001).   
}
\value{
  The list containing three performance measures computed with out-of-bag instances is returned:
  \item{accuracy}{the classification accuracy of the forest,}
  \item{margin}{the average margin of classification with the forest,}
  \item{correlation}{the correlation between trees in the forest.}  
}

\author{ Marko Robnik-Sikonja.}


\keyword{ models }
\keyword{ nonlinear }
\keyword{ tree }
\keyword{ classif }

\seealso{
\code{\link{CORElearn}},
\code{\link{CoreModel}}.
}

\examples{
# build random forests model with certain parameters
modelRF <- CoreModel(Species ~ ., iris, model="rf", 
              selectionEstimator="MDL", minNodeWeightRF=5, 
              rfNoTrees=100, maxThreads=1)
rfOOB(modelRF) 

destroyModels(modelRF) # clean up

}
\references{ 

Leo Breiman: Random Forests. Machine Learning Journal, 2001, 45, 5-32

 }