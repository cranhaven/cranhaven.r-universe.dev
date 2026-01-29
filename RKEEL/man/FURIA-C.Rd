\docType{class}
\name{FURIA_C}
\alias{FURIA_C}
\alias{R6_FURIA_C}
\title{FURIA_C KEEL Classification Algorithm}
\description{
FURIA_C Classification Algorithm from KEEL. 
}
\usage{
FURIA_C(train, test, optimizations, folds, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{optimizations}{optimizations. Default value = 2}
\item{folds}{folds. Default value = 3}
\item{seed}{Seed for random numbers. If it is not assigned a value, the seed will be a random number}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("iris_train")
data_test <- RKEEL::loadKeelDataset("iris_test")

#Create algorithm
algorithm <- RKEEL::FURIA_C(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

