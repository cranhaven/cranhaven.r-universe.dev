\name{wrap4Explanation}
\alias{wrap4Explanation}

\title{Wrap prediction model for explanations}
\description{
  The function wraps given prediction model to be used with ExplainPrediction package.
  Currently \code{\link{nnet}} from \code{nnet} package and models of class \code{svm} 
  from package \code{e1071} are supported, but others can easily be added. Please, note that models from
  \code{\link{CORElearn-package}} can be used directly and need no wrapper.
  If inclusion of other models into ExplainPrediction is desired, please, contact the author.
 }
\usage{
  wrap4Explanation(model)
}
\arguments{
  \item{model}{ The model as returned by \code{\link{nnet}} or any of \code{svm} functions in \code{e1071}} package.                                    
}
\details{
The function adds necessary components to the prediction model so that function \code{\link{explainVis}} 
can generate explanations and their visualizations.
Currently, four components are added:
 \itemize{
 \item \code{formula}, a formula specifying the dependent and independent variables used by the supplied model.
 \item \code{model}, a name of the supplied model.
 \item \code{noClasses}, a number of class values for classification problems and 0 for regression.
 \item \code{class.lev}, for classification problem a vector of class value names.
  }
  If for a given \code{model} the method \code{\link{predict}} returns the class value probabilities
   as matrix or in a list with component \code{probabilities}, nothing else is needed, otherwise
   the internal function \code{getPredictions} has to be adequately modified.
 }
\value{
 The function returns unchanged \code{model} with the components described in Details. 
 }
\author{ Marko Robnik-Sikonja }

\seealso{
\code{\link{explainVis}}
}

\examples{
\dontrun{
# use iris data set, split it randomly into a training and testing set
trainIdxs <- sample(x=nrow(iris), size=0.7*nrow(iris), replace=FALSE)
testIdxs <- c(1:nrow(iris))[-trainIdxs]
# build a nnet model with certain parameters
require(nnet)
modelNN <- nnet(Species ~ ., iris[trainIdxs,], size=20)

# use wrapper
modelNNet <- wrap4Explanation(modelNN)

# generate model explanation and visualization
# turn on history in the visualization window to see all graphs
explainVis(modelNNet, iris[trainIdxs,], iris[testIdxs,], method="EXPLAIN",visLevel="both",
           problemName="iris", fileType="none", 
           naMode="avg", explainType="WE", classValue=1) 
}
}
\keyword{ models }
\keyword{ regression }
\keyword{ classif }
