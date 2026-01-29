\docType{class}
\name{WM_R}
\alias{WM_R}
\alias{R6_WM_R}
\title{WM_R KEEL Regression Algorithm}
\description{
WM_R Regression Algorithm from KEEL. 
}
\usage{
WM_R(train, test, numlabels, KB)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{numlabels}{numlabels. Default value = 5}
\item{KB}{KB. Default value = FALSE}
}
\value{
A data.frame with the actual and predicted values for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("autoMPG6_train")
data_test <- RKEEL::loadKeelDataset("autoMPG6_test")

#Create algorithm
algorithm <- RKEEL::WM_R(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{regression}

