\docType{class}
\name{PUBLIC_C}
\alias{PUBLIC_C}
\alias{R6_PUBLIC_C}
\title{PUBLIC_C KEEL Classification Algorithm}
\description{
PUBLIC_C Classification Algorithm from KEEL. 
}
\usage{
PUBLIC_C(train, test, nodesBetweenPrune, estimateToPrune)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{nodesBetweenPrune}{nodesBetweenPrune. Default value = 25}
\item{estimateToPrune}{estimateToPrune. Default value = "PUBLIC(1)"}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("iris_train")
data_test <- RKEEL::loadKeelDataset("iris_test")

#Create algorithm
algorithm <- RKEEL::PUBLIC_C(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

