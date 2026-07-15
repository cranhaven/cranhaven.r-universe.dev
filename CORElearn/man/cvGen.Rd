\name{cvGen}
\alias{cvGen}
\alias{cvGenStratified}
\alias{gatherFromList}

\title{ Cross-validation and stratified cross-validation}
\description{
 Generate indices for cross-validation and stratified cross-validation }
\usage{
cvGen(n, k) 
cvGenStratified(classVal,k) 
gatherFromList(lst)

}

\arguments{
  \item{n}{ The number of instances in a data set.}
  \item{k}{ The number of folds in cross-validation.}
  \item{classVal}{ A vector of factors representing class values.}
  \item{lst}{A list of lists from which we collect results of the same components.}
 }
\details{
The functions \code{cvGen} and \code{cvGenStratified} generate indices of instances from a data set which can be used in cross-validation. 
The function \code{cvGenStratified} generates the same distribution of class values in each fold.
The function \code{gatherFromList} is an auxiliary function helping in collection of results, see the example below.
}
\value{
The functions \code{cvGen} and \code{cvGenStratified} return a vector of indices indicating fold membership i.e. from 1:k.
The function \code{gatherFromList} returns a list with components containing elements of the same name.
}

\author{ Marko Robnik-Sikonja}

\seealso{
\code{\link{CORElearn}}.
}

\examples{
data <- iris
folds <- 10
foldIdx <- cvGen(nrow(data), k=folds)
evalCore<-list()
for (j in 1:folds) {
	dTrain <- data[foldIdx!=j,]
	dTest  <- data[foldIdx==j,]
	modelCore <- CoreModel(Species~., dTrain, model="rf") 
	predCore <- predict(modelCore, dTest)
	evalCore[[j]] <- modelEval(modelCore, correctClass=dTest$Species,
	          predictedClass=predCore$class, predictedProb=predCore$prob ) 
	destroyModels(modelCore)
}
results <- gatherFromList(evalCore)
sapply(results, mean)
}


