\docType{class}
\name{M5_R}
\alias{M5_R}
\alias{R6_M5_R}
\title{M5_R KEEL Regression Algorithm}
\description{
M5_R Regression Algorithm from KEEL. 
}
\usage{
M5_R(train, test, type, pruningFactor, unsmoothed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{type}{type. Default value = "m"}
\item{pruningFactor}{pruningFactor. Default value = 2}
\item{unsmoothed}{unsmoothed. Default value = TRUE}
}
\value{
A data.frame with the actual and predicted values for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("autoMPG6_train")
data_test <- RKEEL::loadKeelDataset("autoMPG6_test")

#Create algorithm
algorithm <- RKEEL::M5_R(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{regression}

