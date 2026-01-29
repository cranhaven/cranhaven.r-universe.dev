\docType{class}
\name{ZScore_TR}
\alias{ZScore_TR}
\alias{R6_ZScore_TR}
\title{ZScore_TR KEEL Preprocess Algorithm}
\description{
ZScore_TR Preprocess Algorithm from KEEL. 
}
\usage{
ZScore_TR(train, test)
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
algorithm <- RKEEL::ZScore_TR(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$preprocessed_test
}
\keyword{preprocess}

