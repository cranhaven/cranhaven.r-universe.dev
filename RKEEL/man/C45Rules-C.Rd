\docType{class}
\name{C45Rules_C}
\alias{C45Rules_C}
\alias{R6_C45Rules_C}
\title{C45Rules_C KEEL Classification Algorithm}
\description{
C45Rules_C Classification Algorithm from KEEL. 
}
\usage{
C45Rules_C(train, test, confidence, itemsetsPerLeaf, threshold,
   seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{confidence}{confidence. Default value = 0.25}
\item{itemsetsPerLeaf}{itemsetsPerLeaf. Default value = 2}
\item{threshold}{threshold. Default value = 10}
\item{seed}{Seed for random numbers. If it is not assigned a value, the seed will be a random number}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("iris_train")
data_test <- RKEEL::loadKeelDataset("iris_test")

#Create algorithm
algorithm <- RKEEL::C45Rules_C(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

