\docType{class}
\name{DSM_C}
\alias{DSM_C}
\alias{R6_DSM_C}
\title{DSM_C KEEL Classification Algorithm}
\description{
DSM_C Classification Algorithm from KEEL. 
}
\usage{
DSM_C(train, test, iterations, percentage, alpha_0, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{iterations}{iterations. Default value = 100}
\item{percentage}{percentage. Default value = 10}
\item{alpha_0}{alpha_0. Default value = 0.1}
\item{seed}{Seed for random numbers. If it is not assigned a value, the seed will be a random number}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("iris_train")
data_test <- RKEEL::loadKeelDataset("iris_test")

#Create algorithm
algorithm <- RKEEL::DSM_C(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

