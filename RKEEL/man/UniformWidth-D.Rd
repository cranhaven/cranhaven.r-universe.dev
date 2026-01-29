\docType{class}
\name{UniformWidth_D}
\alias{UniformWidth_D}
\alias{R6_UniformWidth_D}
\title{UniformWidth_D KEEL Preprocess Algorithm}
\description{
UniformWidth_D Preprocess Algorithm from KEEL. 
}
\usage{
UniformWidth_D(train, test, numIntervals)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{numIntervals}{numIntervals. Default value = 10}
}
\value{
A data.frame with the preprocessed data for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("car_train")
data_test <- RKEEL::loadKeelDataset("car_test")

#Create algorithm
algorithm <- RKEEL::UniformWidth_D(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$preprocessed_test
}
\keyword{preprocess}

