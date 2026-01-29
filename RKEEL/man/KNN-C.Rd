\docType{class}
\name{KNN_C}
\alias{KNN_C}
\alias{R6_KNN_C}
\title{KNN-C KEEL Classification Algorithm}
\description{
KNN-C Classification Algorithm from KEEL. 
}
\usage{
KNN_C(train, test, k, distance)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{k}{Number of neighbors}
\item{distance}{Distance function}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("iris_train")
data_test <- RKEEL::loadKeelDataset("iris_test")

#Create algorithm
algorithm <- RKEEL::KNN_C(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

