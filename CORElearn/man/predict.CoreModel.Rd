\name{predict.CoreModel}
\alias{predict.CoreModel}
\alias{predict}

\title{ Prediction using constructed model }
\description{
   Using a previously built model and new data, predicts the class value and probabilities for 
  classification problem and function value for regression problem.
 }
\usage{
\method{predict}{CoreModel}(object, newdata, ..., costMatrix=NULL, 
                            type=c("both","class","probability"))
}

\arguments{
  \item{object}{ The model structure as returned by \code{\link{CoreModel}}. }
  \item{newdata}{ Data frame with fresh data. }
  \item{costMatrix}{ Optional cost matrix can provide nonuniform costs for classification problems. }
  \item{type}{ Controls what will be return value in case of classification. } 
  \item{... }{ Other model dependent options for prediction. See \code{\link{helpCore}}. }
  }
\details{
   The function uses the \code{object} structure as returned by \code{\link{CoreModel}} and
   applies it on the data frame \code{newdata}. The \code{newdata} must be transformable
   using the formula specified for building  the model (with dependent variable removed). If the dependent
    variable is present in \code{newdata}, it is ignored. 
  
  Optional cost matrix can provide nonuniform costs for classification problems. For regression
  problem this parameter is ignored. The costs can be different from the ones used for building the model 
   in \code{\link{CoreModel}}.
    
 }
\value{
  For regression model a vector of predicted values for given input instances. For classification
  problem the parameter \code{type} controls what is returned. With default value \code{"both"} 
  function returns a list with two components \code{class}
  and \code{probabilities} containing predicted class values and probabilities for all class values, respectively.
  With \code{type} set to  \code{"class"} or \code{"probability"} the function returns only the selected component 
  as vector or matrix.   
}

\author{ Marko Robnik-Sikonja, Petr Savicky }

\seealso{
\code{\link{CORElearn}},
\code{\link{CoreModel}},
\code{\link{modelEval}},
\code{\link{helpCore}}, 
\code{\link{paramCoreIO}}.
}

\examples{
# use iris data set

# build random forests model with certain parameters
modelRF <- CoreModel(Species ~ ., iris, model="rf", 
              selectionEstimator="MDL",minNodeWeightRF=5,rfNoTrees=100)
print(modelRF)

# prediction with node distribution
pred <- predict(modelRF, iris, rfPredictClass=FALSE, type="both")
# print(pred)

destroyModels(modelRF) # clean up

}

\keyword{ models }
\keyword{ regression }
\keyword{ nonlinear }
\keyword{ tree }
\keyword{ multivariate }
\keyword{ loess }
\keyword{ classif }
