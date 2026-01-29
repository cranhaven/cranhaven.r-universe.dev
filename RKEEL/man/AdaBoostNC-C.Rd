\docType{class}
\name{AdaBoostNC_C}
\alias{AdaBoostNC_C}
\alias{R6_AdaBoostNC_C}
\title{AdaBoostNC_C KEEL Classification Algorithm}
\description{
AdaBoostNC_C Classification Algorithm from KEEL. 
}
\usage{
AdaBoostNC_C(train, test, pruned, confidence, instancesPerLeaf,
   numClassifiers, algorithm, trainMethod, lambda, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{pruned}{pruned. Default value = TRUE}
\item{confidence}{confidence. Default value = 0.25}
\item{instancesPerLeaf}{instancesPerLeaf. Default value = 2}
\item{numClassifiers}{numClassifiers. Default value = 10}
\item{algorithm}{algorithm. Default value = "ADABOOST.NC"}
\item{trainMethod}{trainMethod. Default value = "NORESAMPLING"}
\item{lambda}{lambda. Default value = 2}
\item{seed}{Seed for random numbers. If it is not assigned a value, the seed will be a random number}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("iris_train")
data_test <- RKEEL::loadKeelDataset("iris_test")

#Create algorithm
algorithm <- RKEEL::AdaBoostNC_C(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

