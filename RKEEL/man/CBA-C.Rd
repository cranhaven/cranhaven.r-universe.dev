\docType{class}
\name{CBA_C}
\alias{CBA_C}
\alias{R6_CBA_C}
\title{CBA_C KEEL Associative Classification Algorithm}
\description{
CBA_C Associative Classification Algorithm from KEEL.
}
\usage{
CBA_C(train, test, min_support, min_confidence, pruning, maxCandidates)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{min_support}{min_support. Default value = 0.01}
\item{min_confidence}{min_confidence. Default value = 0.5}
\item{pruning}{indicates wether pruning or not. Default value = TRUE}
\item{maxCandidates}{maxCandidates; if 0, no limit. Default value = 80000}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
data <- loadKeelDataset("breast")

#Create algorithm
algorithm <- RKEEL::CBA_C(data, data)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

