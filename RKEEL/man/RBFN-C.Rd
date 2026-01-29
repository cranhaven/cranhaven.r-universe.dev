\docType{class}
\name{RBFN_C}
\alias{RBFN_C}
\alias{R6_RBFN_C}
\title{RBFN_C KEEL Classification Algorithm}
\description{
RBFN_C Classification Algorithm from KEEL. 
}
\usage{
RBFN_C(train, test, neurons, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{neurons}{neurons. Default value = 50}
\item{seed}{Seed for random numbers. If it is not assigned a value, the seed will be a random number}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("iris_train")
data_test <- RKEEL::loadKeelDataset("iris_test")

#Create algorithm
algorithm <- RKEEL::RBFN_C(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

