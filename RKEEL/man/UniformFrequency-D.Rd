\docType{class}
\name{UniformFrequency_D}
\alias{UniformFrequency_D}
\alias{R6_UniformFrequency_D}
\title{UniformFrequency_D KEEL Preprocess Algorithm}
\description{
UniformFrequency_D Preprocess Algorithm from KEEL. 
}
\usage{
UniformFrequency_D(train, test, numIntervals, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{numIntervals}{numIntervals. Default value = 10}
\item{seed}{Seed for random numbers. If it is not assigned a value, the seed will be a random number}
}
\value{
A data.frame with the preprocessed data for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("car_train")
data_test <- RKEEL::loadKeelDataset("car_test")

#Create algorithm
algorithm <- RKEEL::UniformFrequency_D(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$preprocessed_test
}
\keyword{preprocess}

