\docType{class}
\name{JFKNN_C}
\alias{JFKNN_C}
\alias{R6_JFKNN_C}
\title{JFKNN_C KEEL Classification Algorithm}
\description{
JFKNN_C Classification Algorithm from KEEL. 
}
\usage{
JFKNN_C(train, test)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("iris_train")
data_test <- RKEEL::loadKeelDataset("iris_test")

#Create algorithm
algorithm <- RKEEL::JFKNN_C(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

