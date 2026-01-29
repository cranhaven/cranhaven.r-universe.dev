\docType{methods}
\name{runCV}
\alias{runCV}
\title{Run Cross-Validation}
\description{
Run a cross-validation experiment
}
\usage{
runCV(algorithm, dataset, numFolds, cores)
}
\arguments{
\item{algorithm}{Algorithm to be executed in the CV. It must has the parameters to be used in the executions.}
\item{dataset}{Dataset to perform the CV. It is divided in numFolds disjoint partitions and in each iteration, one is used for test and the rest for train.}
\item{numFolds}{Number of folds for the cross-validation procedure.}
\item{cores}{Number of cores to execute in parallel. If it is missed, default value is 1 (sequential execution).}
}
\value{
Returns a list with the mean results of the numFolds executions.
}
\examples{
#Load datasets
iris <- RKEEL::loadKeelDataset("iris")

#Create algorithm
learner_C45_C <- RKEEL::C45_C(iris, iris)

#Perform 5-folds CV
results <- RKEEL::runCV(learner_C45_C, iris, 5)
}
\keyword{utils}

