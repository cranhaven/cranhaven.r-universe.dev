\docType{class}
\name{CPAR_C}
\alias{CPAR_C}
\alias{R6_CPAR_C}
\title{CPAR_C KEEL Associative Classification Algorithm}
\description{
CPAR_C Associative Classification Algorithm from KEEL.
}
\usage{
CPAR_C(train, test, delta, min_gain, alpha, rules_prediction)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{delta}{delta. Default value = 0.05}
\item{min_gain}{min_gain. Default value = 0.7}
\item{alpha}{alpha. Default value = 0.66}
\item{rules_prediction}{rules_prediction. Default value = 5}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
data <- loadKeelDataset("breast")

#Create algorithm
algorithm <- RKEEL::CPAR_C(data, data)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

