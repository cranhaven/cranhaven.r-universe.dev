\docType{class}
\name{FuzzyNPC_C}
\alias{FuzzyNPC_C}
\alias{R6_FuzzyNPC_C}
\title{FuzzyNPC_C KEEL Classification Algorithm}
\description{
FuzzyNPC_C Classification Algorithm from KEEL. 
}
\usage{
FuzzyNPC_C(train, test, M)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{M}{M. Default value = 2.0}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("iris_train")
data_test <- RKEEL::loadKeelDataset("iris_test")

#Create algorithm
algorithm <- RKEEL::FuzzyNPC_C(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

