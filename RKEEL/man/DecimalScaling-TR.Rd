\docType{class}
\name{DecimalScaling_TR}
\alias{DecimalScaling_TR}
\alias{R6_DecimalScaling_TR}
\title{DecimalScaling_TR KEEL Preprocess Algorithm}
\description{
DecimalScaling_TR Preprocess Algorithm from KEEL. 
}
\usage{
DecimalScaling_TR(train, test)
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
algorithm <- RKEEL::DecimalScaling_TR(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$preprocessed_test
}
\keyword{preprocess}

