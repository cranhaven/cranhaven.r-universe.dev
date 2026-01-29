\docType{class}
\name{FuzzyKNN_C}
\alias{FuzzyKNN_C}
\alias{R6_FuzzyKNN_C}
\title{FuzzyKNN_C KEEL Classification Algorithm}
\description{
FuzzyKNN_C Classification Algorithm from KEEL. 
}
\usage{
FuzzyKNN_C(train, test, k, M, initialization, init_k)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{k}{k. Default value = 3}
\item{M}{M. Default value = 2.0}
\item{initialization}{initialization. Default value = "CRISP"}
\item{init_k}{init_k. Default value = 3}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("iris_train")
data_test <- RKEEL::loadKeelDataset("iris_test")

#Create algorithm
algorithm <- RKEEL::FuzzyKNN_C(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

