\docType{class}
\name{Nominal2Binary_TR}
\alias{Nominal2Binary_TR}
\alias{R6_Nominal2Binary_TR}
\title{Nominal2Binary_TR KEEL Preprocess Algorithm}
\description{
Nominal2Binary_TR Preprocess Algorithm from KEEL. 
}
\usage{
Nominal2Binary_TR(train, test)
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
algorithm <- RKEEL::Nominal2Binary_TR(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$preprocessed_test
}
\keyword{preprocess}

