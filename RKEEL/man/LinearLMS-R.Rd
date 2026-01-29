\docType{class}
\name{LinearLMS_R}
\alias{LinearLMS_R}
\alias{R6_LinearLMS_R}
\title{LinearLMS_R KEEL Regression Algorithm}
\description{
LinearLMS_R Regression Algorithm from KEEL. 
}
\usage{
LinearLMS_R(train, test, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{seed}{Seed for random numbers. If it is not assigned a value, the seed will be a random number}
}
\value{
A data.frame with the actual and predicted values for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("autoMPG6_train")
data_test <- RKEEL::loadKeelDataset("autoMPG6_test")

#Create algorithm
algorithm <- RKEEL::LinearLMS_R(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{regression}

