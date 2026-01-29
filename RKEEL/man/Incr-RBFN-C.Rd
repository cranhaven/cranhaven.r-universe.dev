\docType{class}
\name{IncrRBFN_C}
\alias{IncrRBFN_C}
\alias{R6_IncrRBFN_C}
\title{IncrRBFN_C KEEL Classification Algorithm}
\description{
IncrRBFN_C Classification Algorithm from KEEL. 
}
\usage{
IncrRBFN_C(train, test, epsilon, alfa, delta, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{epsilon}{epsilon. Default value = 0.1}
\item{alfa}{alfa. Default value = 0.3}
\item{delta}{delta. Default value = 0.5}
\item{seed}{Seed for random numbers. If it is not assigned a value, the seed will be a random number}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("iris_train")
data_test <- RKEEL::loadKeelDataset("iris_test")

#Create algorithm
algorithm <- RKEEL::IncrRBFN_C(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

