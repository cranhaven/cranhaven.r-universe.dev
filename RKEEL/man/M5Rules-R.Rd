\docType{class}
\name{M5Rules_R}
\alias{M5Rules_R}
\alias{R6_M5Rules_R}
\title{M5Rules_R KEEL Regression Algorithm}
\description{
M5Rules_R Regression Algorithm from KEEL. 
}
\usage{
M5Rules_R(train, test, pruningFactor, heuristic)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{pruningFactor}{pruningFactor. Default value = 2}
\item{heuristic}{heuristic. Default value = "Coverage"}
}
\value{
A data.frame with the actual and predicted values for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("autoMPG6_train")
data_test <- RKEEL::loadKeelDataset("autoMPG6_test")

#Create algorithm
algorithm <- RKEEL::M5Rules_R(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{regression}

