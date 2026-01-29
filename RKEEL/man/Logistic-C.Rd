\docType{class}
\name{Logistic_C}
\alias{Logistic_C}
\alias{R6_Logistic_C}
\title{Logistic_C KEEL Classification Algorithm}
\description{
Logistic_C Classification Algorithm from KEEL. 
}
\usage{
Logistic_C(train, test, ridge, maxIter)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{ridge}{ridge. Default value = 1e-8}
\item{maxIter}{maxIter. Default value = -1}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("iris_train")
data_test <- RKEEL::loadKeelDataset("iris_test")

#Create algorithm
algorithm <- RKEEL::Logistic_C(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

