\docType{class}
\name{CNN_C}
\alias{CNN_C}
\alias{R6_CNN_C}
\title{CNN_C KEEL Classification Algorithm}
\description{
CNN_C Classification Algorithm from KEEL. 
}
\usage{
CNN_C(train, test, k, distance, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{k}{k. Default value = 1}
\item{distance}{distance. Default value = "Euclidean"}
\item{seed}{Seed for random numbers. If it is not assigned a value, the seed will be a random number}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("iris_train")
data_test <- RKEEL::loadKeelDataset("iris_test")

#Create algorithm
algorithm <- RKEEL::CNN_C(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

