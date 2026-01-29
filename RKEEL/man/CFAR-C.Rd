\docType{class}
\name{CFAR_C}
\alias{CFAR_C}
\alias{R6_CFAR_C}
\title{CFAR_C KEEL Classification Algorithm}
\description{
CFAR_C Classification Algorithm from KEEL. 
}
\usage{
CFAR_C(train, test, min_support, min_confidence, threshold,
   num_labels, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{min_support}{min_support. Default value = 0.1}
\item{min_confidence}{min_confidence. Default value = 0.85}
\item{threshold}{threshold. Default value = 0.15}
\item{num_labels}{num_labels. Default value = 5}
\item{seed}{Seed for random numbers. If it is not assigned a value, the seed will be a random number}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("iris_train")
data_test <- RKEEL::loadKeelDataset("iris_test")

#Create algorithm
algorithm <- RKEEL::CFAR_C(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

