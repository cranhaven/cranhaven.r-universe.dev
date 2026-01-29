\docType{class}
\name{DecrRBFN_C}
\alias{DecrRBFN_C}
\alias{R6_DecrRBFN_C}
\title{DecrRBFN_C KEEL Classification Algorithm}
\description{
DecrRBFN_C Classification Algorithm from KEEL. 
}
\usage{
DecrRBFN_C(train, test, percent, num_neurons_ini, alfa, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{percent}{percent. Default value = 0.1}
\item{num_neurons_ini}{num_neurons_ini. Default value = 20}
\item{alfa}{alfa. Default value = 0.3}
\item{seed}{Seed for random numbers. If it is not assigned a value, the seed will be a random number}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("iris_train")
data_test <- RKEEL::loadKeelDataset("iris_test")

#Create algorithm
algorithm <- RKEEL::DecrRBFN_C(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

