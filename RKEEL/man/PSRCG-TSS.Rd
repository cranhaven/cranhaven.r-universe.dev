\docType{class}
\name{PSRCG_TSS}
\alias{PSRCG_TSS}
\alias{R6_PSRCG_TSS}
\title{PSRCG_TSS KEEL Preprocess Algorithm}
\description{
PSRCG_TSS Preprocess Algorithm from KEEL. 
}
\usage{
PSRCG_TSS(train, test, distance)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
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
algorithm <- RKEEL::PSRCG_TSS(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$preprocessed_test
}
}
\keyword{preprocess}

