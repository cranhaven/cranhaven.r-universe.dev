\docType{class}
\name{CMAR_C}
\alias{CMAR_C}
\alias{R6_CMAR_C}
\title{CMAR_C KEEL Associative Classification Algorithm}
\description{
CMAR_C Associative Classification Algorithm from KEEL.
}
\usage{
CMAR_C(train, test, min_confidence, min_support, databaseCoverage)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{min_confidence}{min_confidence. Default value = 0.5}
\item{min_support}{min_support. Default value = 0.01}
\item{databaseCoverage}{databaseCoverage. Default value = 4}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
data <- loadKeelDataset("breast")

#Create algorithm
algorithm <- RKEEL::CMAR_C(data, data)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

