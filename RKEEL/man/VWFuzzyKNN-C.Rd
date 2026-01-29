\docType{class}
\name{VWFuzzyKNN_C}
\alias{VWFuzzyKNN_C}
\alias{R6_VWFuzzyKNN_C}
\title{VWFuzzyKNN_C KEEL Classification Algorithm}
\description{
VWFuzzyKNN_C Classification Algorithm from KEEL. 
}
\usage{
VWFuzzyKNN_C(train, test, k, init_k)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{k}{k. Default value = 3}
\item{init_k}{init_k. Default value = 3}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("iris_train")
data_test <- RKEEL::loadKeelDataset("iris_test")

#Create algorithm
algorithm <- RKEEL::VWFuzzyKNN_C(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

