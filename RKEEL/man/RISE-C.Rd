\docType{class}
\name{RISE_C}
\alias{RISE_C}
\alias{R6_RISE_C}
\title{RISE_C KEEL Classification Algorithm}
\description{
RISE_C Classification Algorithm from KEEL. 
}
\usage{
RISE_C(train, test, Q, S)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{Q}{Q. Default value = 1}
\item{S}{S. Default value = 2}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("iris_train")
data_test <- RKEEL::loadKeelDataset("iris_test")

#Create algorithm
algorithm <- RKEEL::RISE_C(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

