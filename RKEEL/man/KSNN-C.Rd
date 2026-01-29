\docType{class}
\name{KSNN_C}
\alias{KSNN_C}
\alias{R6_KSNN_C}
\title{KSNN_C KEEL Classification Algorithm}
\description{
KSNN_C Classification Algorithm from KEEL. 
}
\usage{
KSNN_C(train, test, k)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{k}{k. Default value = 1}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("iris_train")
data_test <- RKEEL::loadKeelDataset("iris_test")

#Create algorithm
algorithm <- RKEEL::KSNN_C(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

