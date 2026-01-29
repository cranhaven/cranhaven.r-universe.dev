\docType{class}
\name{CW_C}
\alias{CW_C}
\alias{R6_CW_C}
\title{CW_C KEEL Classification Algorithm}
\description{
CW_C Classification Algorithm from KEEL. 
}
\usage{
CW_C(train, test, beta, mu, epsilon)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{beta}{beta. Default value = 8.0}
\item{mu}{mu. Default value = 0.001}
\item{epsilon}{epsilon. Default value = 0.001}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("iris_train")
data_test <- RKEEL::loadKeelDataset("iris_test")

#Create algorithm
algorithm <- RKEEL::CW_C(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

