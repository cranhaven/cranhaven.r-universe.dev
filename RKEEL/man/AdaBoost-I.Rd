\docType{class}
\name{AdaBoost_I}
\alias{AdaBoost_I}
\alias{R6_AdaBoost_I}
\title{AdaBoost_I KEEL Imbalanced Classification Algorithm}
\description{
AdaBoost_I Imbalanced Classification Algorithm from KEEL. 
}
\usage{
AdaBoost_I(train, test, pruned, confidence, instancesPerLeaf,
   numClassifiers, algorithm, trainMethod, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{pruned}{pruned. Default value = TRUE}
\item{confidence}{confidence. Default value = 0.25}
\item{instancesPerLeaf}{instancesPerLeaf. Default value = 2}
\item{numClassifiers}{numClassifiers. Default value = 10}
\item{algorithm}{algorithm. Default value = "ADABOOST"}
\item{trainMethod}{trainMethod. Default value = "NORESAMPLING"}
\item{seed}{Seed for random numbers. If it is not assigned a value, the seed will be a random number}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
\donttest{
data_train <- RKEEL::loadKeelDataset("iris_train")
data_test <- RKEEL::loadKeelDataset("iris_test")

#Create algorithm
algorithm <- RKEEL::AdaBoost_I(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
}
\keyword{classification}

