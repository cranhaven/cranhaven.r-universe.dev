\docType{class}
\name{ID3_C}
\alias{ID3_C}
\alias{R6_ID3_C}
\title{ID3_C KEEL Classification Algorithm}
\description{
ID3_C Classification Algorithm from KEEL. 
}
\usage{
ID3_C(train, test)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
}
\value{
A data.frame with the actual and predicted classes for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("car_train")
data_test <- RKEEL::loadKeelDataset("car_test")

#Create algorithm
algorithm <- RKEEL::ID3_C(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{classification}

