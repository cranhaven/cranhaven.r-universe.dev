\docType{class}
\name{CenterNN_C}
\alias{CenterNN_C}
\alias{R6_CenterNN_C}
\title{CenterNN_C KEEL Classification Algorithm}
\description{
CenterNN_C Classification Algorithm from KEEL. 
}
\usage{
CenterNN_C(train, test)
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
algorithm <- RKEEL::CenterNN_C(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

