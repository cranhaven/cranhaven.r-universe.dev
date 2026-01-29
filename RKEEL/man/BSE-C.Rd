\docType{class}
\name{BSE_C}
\alias{BSE_C}
\alias{R6_BSE_C}
\title{BSE_C KEEL Classification Algorithm}
\description{
BSE_C Classification Algorithm from KEEL. 
}
\usage{
BSE_C(train, test, k, distance)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{k}{k. Default value = 1}
\item{distance}{distance. Default value = "Euclidean"}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("iris_train")
data_test <- RKEEL::loadKeelDataset("iris_test")

#Create algorithm
algorithm <- RKEEL::BSE_C(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

