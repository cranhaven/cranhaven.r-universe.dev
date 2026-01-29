\docType{class}
\name{IF_KNN_C}
\alias{IF_KNN_C}
\alias{R6_IF_KNN_C}
\title{IF_KNN_C KEEL Classification Algorithm}
\description{
IF_KNN_C Classification Algorithm from KEEL. 
}
\usage{
IF_KNN_C(train, test, K, mA, vA, mR, vR, k)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{K}{K. Default value = 3}
\item{mA}{mA. Default value = 0.6}
\item{vA}{vA. Default value = 0.4}
\item{mR}{mR. Default value = 0.3}
\item{vR}{vR. Default value = 0.7}
\item{k}{k. Default value = 5}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("iris_train")
data_test <- RKEEL::loadKeelDataset("iris_test")

#Create algorithm
algorithm <- RKEEL::IF_KNN_C(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

