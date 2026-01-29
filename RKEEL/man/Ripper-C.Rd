\docType{class}
\name{Ripper_C}
\alias{Ripper_C}
\alias{R6_Ripper_C}
\title{Ripper_C KEEL Classification Algorithm}
\description{
Ripper_C Classification Algorithm from KEEL. 
}
\usage{
Ripper_C(train, test, grow_pct, k, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{grow_pct}{grow_pct. Default value = 0.66}
\item{k}{k. Default value = 2}
\item{seed}{Seed for random numbers. If it is not assigned a value, the seed will be a random number}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("iris_train")
data_test <- RKEEL::loadKeelDataset("iris_test")

#Create algorithm
algorithm <- RKEEL::Ripper_C(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

