\docType{class}
\name{Ignore_MV}
\alias{Ignore_MV}
\alias{R6_Ignore_MV}
\title{Ignore_MV KEEL Preprocess Algorithm}
\description{
Ignore_MV Preprocess Algorithm from KEEL. 
}
\usage{
Ignore_MV(train, test)
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
algorithm <- RKEEL::Ignore_MV(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$preprocessed_test
}
\keyword{preprocess}

