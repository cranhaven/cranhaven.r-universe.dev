\docType{class}
\name{CleanAttributes_TR}
\alias{CleanAttributes_TR}
\alias{R6_CleanAttributes_TR}
\title{CleanAttributes_TR KEEL Preprocess Algorithm}
\description{
CleanAttributes_TR Preprocess Algorithm from KEEL. 
}
\usage{
CleanAttributes_TR(train, test)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
}
\value{
A data.frame with the preprocessed data for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("car_train")
data_test <- RKEEL::loadKeelDataset("car_test")

#Create algorithm
algorithm <- RKEEL::CleanAttributes_TR(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$preprocessed_test
}
\keyword{preprocess}

