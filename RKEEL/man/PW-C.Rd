\docType{class}
\name{PW_C}
\alias{PW_C}
\alias{R6_PW_C}
\title{PW_C KEEL Classification Algorithm}
\description{
PW_C Classification Algorithm from KEEL. 
}
\usage{
PW_C(train, test, beta, ro, epsilon)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{beta}{beta. Default value = 8.0}
\item{ro}{ro. Default value = 0.001}
\item{epsilon}{epsilon. Default value = 0.001}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("iris_train")
data_test <- RKEEL::loadKeelDataset("iris_test")

#Create algorithm
algorithm <- RKEEL::PW_C(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

