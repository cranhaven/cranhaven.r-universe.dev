\docType{class}
\name{ClusterAnalysis_D}
\alias{ClusterAnalysis_D}
\alias{R6_ClusterAnalysis_D}
\title{ClusterAnalysis_D KEEL Preprocess Algorithm}
\description{
ClusterAnalysis_D Preprocess Algorithm from KEEL.
}
\usage{
ClusterAnalysis_D(train, test)
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
algorithm <- RKEEL::ClusterAnalysis_D(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$preprocessed_test
}
}
\keyword{preprocess}

