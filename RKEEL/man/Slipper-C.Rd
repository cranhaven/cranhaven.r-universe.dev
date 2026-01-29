\docType{class}
\name{Slipper_C}
\alias{Slipper_C}
\alias{R6_Slipper_C}
\title{Slipper_C KEEL Classification Algorithm}
\description{
Slipper_C Classification Algorithm from KEEL. 
}
\usage{
Slipper_C(train, test, grow_pct, numBoosting, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{grow_pct}{grow_pct. Default value = 0.66}
\item{numBoosting}{numBoosting. Default value = 100}
\item{seed}{Seed for random numbers. If it is not assigned a value, the seed will be a random number}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("iris_train")
data_test <- RKEEL::loadKeelDataset("iris_test")

#Create algorithm
algorithm <- RKEEL::Slipper_C(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

