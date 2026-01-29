\docType{class}
\name{KNN_MV}
\alias{KNN_MV}
\alias{R6_KNN_MV}
\title{KNN_MV KEEL Preprocess Algorithm}
\description{
KNN_MV Preprocess Algorithm from KEEL. 
}
\usage{
KNN_MV(train, test, k)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{k}{k. Default value = 10}
}
\value{
A data.frame with the preprocessed data for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("car_train")
data_test <- RKEEL::loadKeelDataset("car_test")

#Create algorithm
algorithm <- RKEEL::KNN_MV(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$preprocessed_test
}
\keyword{preprocess}

