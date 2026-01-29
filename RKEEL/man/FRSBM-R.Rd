\docType{class}
\name{FRSBM_R}
\alias{FRSBM_R}
\alias{R6_FRSBM_R}
\title{FRSBM_R KEEL Regression Algorithm}
\description{
FRSBM_R Regression Algorithm from KEEL.
}
\usage{
FRSBM_R(train, test, numrules, sigma, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{numrules}{numrules. Default value = 1}
\item{sigma}{sigma. Default value = 0.0001}
\item{seed}{Seed for random numbers. If it is not assigned a value, the seed will be a random number}
}
\value{
A data.frame with the actual and predicted values for both \code{train} and \code{test} datasets.
}
\examples{
\donttest{
data_train <- RKEEL::loadKeelDataset("autoMPG6_train")
data_test <- RKEEL::loadKeelDataset("autoMPG6_test")

#Create algorithm
algorithm <- RKEEL::FRSBM_R(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
}
\keyword{regression}

