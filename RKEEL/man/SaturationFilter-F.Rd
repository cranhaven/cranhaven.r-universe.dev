\docType{class}
\name{SaturationFilter_F}
\alias{SaturationFilter_F}
\alias{R6_SaturationFilter_F}
\title{SaturationFilter_F KEEL Preprocess Algorithm}
\description{
SaturationFilter_F Preprocess Algorithm from KEEL.
}
\usage{
SaturationFilter_F(train, test, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{seed}{Seed for random numbers. If it is not assigned a value, the seed will be a random number}
}
\value{
A data.frame with the preprocessed data for both \code{train} and \code{test} datasets.
}
\examples{
\donttest{
data_train <- RKEEL::loadKeelDataset("car_train")
data_test <- RKEEL::loadKeelDataset("car_test")

#Create algorithm
algorithm <- RKEEL::SaturationFilter_F(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$preprocessed_test
}
}
\keyword{preprocess}

