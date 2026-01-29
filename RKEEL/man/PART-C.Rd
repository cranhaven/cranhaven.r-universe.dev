\docType{class}
\name{PART_C}
\alias{PART_C}
\alias{R6_PART_C}
\title{PART_C KEEL Classification Algorithm}
\description{
PART_C Classification Algorithm from KEEL. 
}
\usage{
PART_C(train, test, confidence, itemsetsPerLeaf)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{confidence}{confidence. Default value = 0.25}
\item{itemsetsPerLeaf}{itemsetsPerLeaf. Default value = 2}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("iris_train")
data_test <- RKEEL::loadKeelDataset("iris_test")

#Create algorithm
algorithm <- RKEEL::PART_C(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

