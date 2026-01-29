\docType{class}
\name{CFKNN_C}
\alias{CFKNN_C}
\alias{R6_CFKNN_C}
\title{CFKNN_C KEEL Classification Algorithm}
\description{
CFKNN_C Classification Algorithm from KEEL. 
}
\usage{
CFKNN_C(train, test, k, alpha, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{k}{k. Default value = 3}
\item{alpha}{alpha. Default value = 0.6}
\item{seed}{Seed for random numbers. If it is not assigned a value, the seed will be a random number}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("iris_train")
data_test <- RKEEL::loadKeelDataset("iris_test")

#Create algorithm
algorithm <- RKEEL::CFKNN_C(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

