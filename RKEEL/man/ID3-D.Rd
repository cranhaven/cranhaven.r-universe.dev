\docType{class}
\name{ID3_D}
\alias{ID3_D}
\alias{R6_ID3_D}
\title{ID3_D KEEL Preprocess Algorithm}
\description{
ID3_D Preprocess Algorithm from KEEL. 
}
\usage{
ID3_D(train, test)
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
algorithm <- RKEEL::ID3_D(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$preprocessed_test
}
\keyword{preprocess}

