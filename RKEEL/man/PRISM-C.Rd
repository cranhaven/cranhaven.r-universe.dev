\docType{class}
\name{PRISM_C}
\alias{PRISM_C}
\alias{R6_PRISM_C}
\title{PRISM_C KEEL Classification Algorithm}
\description{
PRISM_C Classification Algorithm from KEEL.
}
\usage{
PRISM_C(train, test, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{seed}{Seed for random numbers. If it is not assigned a value, the seed will be a random number}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
\donttest{
data_train <- RKEEL::loadKeelDataset("car_train")
data_test <- RKEEL::loadKeelDataset("car_test")

#Create algorithm
algorithm <- RKEEL::PRISM_C(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
}
\keyword{classification}

