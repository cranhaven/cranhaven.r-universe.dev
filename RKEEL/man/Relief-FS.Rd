\docType{class}
\name{Relief_FS}
\alias{Relief_FS}
\alias{R6_Relief_FS}
\title{Relief_FS KEEL Preprocess Algorithm}
\description{
Relief_FS Preprocess Algorithm from KEEL. 
}
\usage{
Relief_FS(train, test, paramKNN, relevanceThreshold,
   numInstancesSampled, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{paramKNN}{paramKNN. Default value = 1}
\item{relevanceThreshold}{relevanceThreshold. Default value = 0.20}
\item{numInstancesSampled}{numInstancesSampled. Default value = 1000}
\item{seed}{Seed for random numbers. If it is not assigned a value, the seed will be a random number}
}
\value{
A data.frame with the preprocessed data for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("car_train")
data_test <- RKEEL::loadKeelDataset("car_test")

#Create algorithm
algorithm <- RKEEL::Relief_FS(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$preprocessed_test
}
\keyword{preprocess}

