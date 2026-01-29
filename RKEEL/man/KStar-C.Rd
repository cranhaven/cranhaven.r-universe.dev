\docType{class}
\name{KStar_C}
\alias{KStar_C}
\alias{R6_KStar_C}
\title{KStar_C KEEL Classification Algorithm}
\description{
KStar_C Classification Algorithm from KEEL. 
}
\usage{
KStar_C(train, test, selection_method, blend, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{selection_method}{selection_method. Default value = "Fixed"}
\item{blend}{blend. Default value = 0.2}
\item{seed}{Seed for random numbers. If it is not assigned a value, the seed will be a random number}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("iris_train")
data_test <- RKEEL::loadKeelDataset("iris_test")

#Create algorithm
algorithm <- RKEEL::KStar_C(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

