\docType{class}
\name{KMeans_MV}
\alias{KMeans_MV}
\alias{R6_KMeans_MV}
\title{KMeans_MV KEEL Preprocess Algorithm}
\description{
KMeans_MV Preprocess Algorithm from KEEL. 
}
\usage{
KMeans_MV(train, test, k, error, iterations, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{k}{k. Default value = 10}
\item{error}{error. Default value = 100}
\item{iterations}{iterations. Default value = 100}
\item{seed}{Seed for random numbers. If it is not assigned a value, the seed will be a random number}
}
\value{
A data.frame with the preprocessed data for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("car_train")
data_test <- RKEEL::loadKeelDataset("car_test")

#Create algorithm
algorithm <- RKEEL::KMeans_MV(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$preprocessed_test
}
\keyword{preprocess}

