\docType{class}
\name{GFS_AdaBoost_C}
\alias{GFS_AdaBoost_C}
\alias{R6_GFS_AdaBoost_C}
\title{GFS_AdaBoost_C KEEL Classification Algorithm}
\description{
GFS_AdaBoost_C Classification Algorithm from KEEL. 
}
\usage{
GFS_AdaBoost_C(train, test, numLabels, numRules, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{numLabels}{numLabels. Default value = 3}
\item{numRules}{numRules. Default value = 8}
\item{seed}{Seed for random numbers. If it is not assigned a value, the seed will be a random number}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("iris_train")
data_test <- RKEEL::loadKeelDataset("iris_test")

#Create algorithm
algorithm <- RKEEL::GFS_AdaBoost_C(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

