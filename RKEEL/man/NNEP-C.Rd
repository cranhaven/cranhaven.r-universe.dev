\docType{class}
\name{NNEP_C}
\alias{NNEP_C}
\alias{R6_NNEP_C}
\title{NNEP_C KEEL Classification Algorithm}
\description{
NNEP_C Classification Algorithm from KEEL.
}
\usage{
NNEP_C(train, test, hidden_nodes, transfer, generations, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{hidden_nodes}{hidden_nodes. Default value = 4}
\item{transfer}{transfer. Default value = "Product_Unit"}
\item{generations}{generations. Default value = 200}
\item{seed}{Seed for random numbers. If it is not assigned a value, the seed will be a random number}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
\donttest{
data_train <- RKEEL::loadKeelDataset("iris_train")
data_test <- RKEEL::loadKeelDataset("iris_test")

#Create algorithm
algorithm <- RKEEL::NNEP_C(data_train, data_test)
algorithm <- RKEEL::NNEP_C(data_train, data_test, generations = 5)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
}
\keyword{classification}

