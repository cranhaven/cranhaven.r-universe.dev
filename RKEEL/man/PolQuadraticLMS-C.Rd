\docType{class}
\name{PolQuadraticLMS_C}
\alias{PolQuadraticLMS_C}
\alias{R6_PolQuadraticLMS_C}
\title{PolQuadraticLMS_C KEEL Classification Algorithm}
\description{
PolQuadraticLMS_C Classification Algorithm from KEEL. 
}
\usage{
PolQuadraticLMS_C(train, test, seed)
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
data_train <- RKEEL::loadKeelDataset("iris_train")
data_test <- RKEEL::loadKeelDataset("iris_test")

#Create algorithm
algorithm <- RKEEL::PolQuadraticLMS_C(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

