\docType{class}
\name{BNGE_C}
\alias{BNGE_C}
\alias{R6_BNGE_C}
\title{BNGE_C KEEL Classification Algorithm}
\description{
BNGE_C Classification Algorithm from KEEL. 
}
\usage{
BNGE_C(train, test, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{seed}{Seed for random numbers. If it is not assigned a value, the seed will be a random number}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("iris_train")
data_test <- RKEEL::loadKeelDataset("iris_test")

#Create algorithm
algorithm <- RKEEL::BNGE_C(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

