\docType{class}
\name{AllKNN_TSS}
\alias{AllKNN_TSS}
\alias{R6_AllKNN_TSS}
\title{AllKNN_TSS KEEL Preprocess Algorithm}
\description{
AllKNN_TSS Preprocess Algorithm from KEEL. 
}
\usage{
AllKNN_TSS(train, test, k, distance)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{k}{k. Default value = 3}
\item{distance}{distance. Default value = "Euclidean"}
}
\value{
A data.frame with the preprocessed data for both \code{train} and \code{test} datasets.
}
\examples{
\donttest{
data_train <- RKEEL::loadKeelDataset("car_train")
data_test <- RKEEL::loadKeelDataset("car_test")

#Create algorithm
algorithm <- RKEEL::AllKNN_TSS(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$preprocessed_test
}
}
\keyword{preprocess}

