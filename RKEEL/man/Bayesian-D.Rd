\docType{class}
\name{Bayesian_D}
\alias{Bayesian_D}
\alias{R6_Bayesian_D}
\title{Bayesian_D KEEL Preprocess Algorithm}
\description{
Bayesian_D Preprocess Algorithm from KEEL. 
}
\usage{
Bayesian_D(train, test)
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
algorithm <- RKEEL::Bayesian_D(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$preprocessed_test
}
\keyword{preprocess}

