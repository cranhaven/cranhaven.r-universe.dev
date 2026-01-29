\docType{class}
\name{CamNN_C}
\alias{CamNN_C}
\alias{R6_CamNN_C}
\title{CamNN_C KEEL Classification Algorithm}
\description{
CamNN_C Classification Algorithm from KEEL. 
}
\usage{
CamNN_C(train, test, k)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{k}{k. Default value = 1}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("iris_train")
data_test <- RKEEL::loadKeelDataset("iris_test")

#Create algorithm
algorithm <- RKEEL::CamNN_C(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

