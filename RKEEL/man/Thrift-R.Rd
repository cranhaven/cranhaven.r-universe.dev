\docType{class}
\name{Thrift_R}
\alias{Thrift_R}
\alias{R6_Thrift_R}
\title{Thrift_R KEEL Regression Algorithm}
\description{
Thrift_R Regression Algorithm from KEEL.
}
\usage{
Thrift_R(train, test, numLabels, popSize, evaluations,
   crossProb, mutProb, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{numLabels}{numLabels. Default value = 3}
\item{popSize}{popSize. Default value = 61}
\item{evaluations}{evaluations. Default value = 10000}
\item{crossProb}{crossProb. Default value = 0.6}
\item{mutProb}{mutProb. Default value = 0.1}
\item{seed}{Seed for random numbers. If it is not assigned a value, the seed will be a random number}
}
\value{
A data.frame with the actual and predicted values for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("autoMPG6_train")
data_test <- RKEEL::loadKeelDataset("autoMPG6_test")

#Create algorithm
algorithm <- RKEEL::Thrift_R(data_train, data_test)
algorithm <- RKEEL::Thrift_R(data_train, data_test, popSize = 5, evaluations = 10)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{regression}

