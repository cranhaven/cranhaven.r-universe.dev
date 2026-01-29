\docType{class}
\name{GFS_RB_MF_R}
\alias{GFS_RB_MF_R}
\alias{R6_GFS_RB_MF_R}
\title{GFS_RB_MF_R KEEL Regression Algorithm}
\description{
GFS_RB_MF_R Regression Algorithm from KEEL.
}
\usage{
GFS_RB_MF_R(train, test, numLabels, popSize, generations,
   crossProb, mutProb, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{numLabels}{numLabels. Default value = 3}
\item{popSize}{popSize. Default value = 50}
\item{generations}{generations. Default value = 100}
\item{crossProb}{crossProb. Default value = 0.9}
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
algorithm <- RKEEL::GFS_RB_MF_R(data_train, data_test)
algorithm <- RKEEL::GFS_RB_MF_R(data_train, data_test, popSize = 5, generations = 10)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{regression}

