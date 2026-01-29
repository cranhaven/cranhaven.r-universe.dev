\docType{class}
\name{RBFN_R}
\alias{RBFN_R}
\alias{R6_RBFN_R}
\title{RBFN_R KEEL Regression Algorithm}
\description{
RBFN_R Regression Algorithm from KEEL. 
}
\usage{
RBFN_R(train, test, neurons, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{neurons}{neurons. Default value = 50}
\item{seed}{Seed for random numbers. If it is not assigned a value, the seed will be a random number}
}
\value{
A data.frame with the actual and predicted values for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("autoMPG6_train")
data_test <- RKEEL::loadKeelDataset("autoMPG6_test")

#Create algorithm
algorithm <- RKEEL::RBFN_R(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{regression}

