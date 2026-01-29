\docType{class}
\name{POP_TSS}
\alias{POP_TSS}
\alias{R6_POP_TSS}
\title{POP_TSS KEEL Preprocess Algorithm}
\description{
POP_TSS Preprocess Algorithm from KEEL. 
}
\usage{
POP_TSS(train, test)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
}
\value{
A data.frame with the preprocessed data for both \code{train} and \code{test} datasets.
}
\examples{
\donttest{
data_train <- RKEEL::loadKeelDataset("car_train")
data_test <- RKEEL::loadKeelDataset("car_test")

#Create algorithm
algorithm <- RKEEL::POP_TSS(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$preprocessed_test
}
}
\keyword{preprocess}

