\docType{class}
\name{MinMax_TR}
\alias{MinMax_TR}
\alias{R6_MinMax_TR}
\title{MinMax_TR KEEL Preprocess Algorithm}
\description{
MinMax_TR Preprocess Algorithm from KEEL. 
}
\usage{
MinMax_TR(train, test, newMin, newMax)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{newMin}{newMin. Default value = 0.0}
\item{newMax}{newMax. Default value = 1.0}
}
\value{
A data.frame with the preprocessed data for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("car_train")
data_test <- RKEEL::loadKeelDataset("car_test")

#Create algorithm
algorithm <- RKEEL::MinMax_TR(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$preprocessed_test
}
\keyword{preprocess}

