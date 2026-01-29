\docType{class}
\name{CART_R}
\alias{CART_R}
\alias{R6_CART_R}
\title{CART_R KEEL Regression Algorithm}
\description{
CART_R Regression Algorithm from KEEL. 
}
\usage{
CART_R(train, test, maxDepth)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{maxDepth}{maxDepth. Default value = 90}
}
\value{
A data.frame with the actual and predicted values for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("autoMPG6_train")
data_test <- RKEEL::loadKeelDataset("autoMPG6_test")

#Create algorithm
algorithm <- RKEEL::CART_R(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{regression}

