\docType{class}
\name{EPSILON_SVR_R}
\alias{EPSILON_SVR_R}
\alias{R6_EPSILON_SVR_R}
\title{EPSILON_SVR_R KEEL Regression Algorithm}
\description{
EPSILON_SVR_R Regression Algorithm from KEEL. 
}
\usage{
EPSILON_SVR_R(train, test, KernelType, C, eps, degree, gamma,
   coef0, nu, p, shrinking, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{KernelType}{KernelType. Default value = "RBF"}
\item{C}{C. Default value = 100.0}
\item{eps}{eps. Default value = 0.001}
\item{degree}{degree. Default value = 3}
\item{gamma}{gamma. Default value = 0.01}
\item{coef0}{coef0. Default value = 0.0}
\item{nu}{nu. Default value = 0.5}
\item{p}{p. Default value = 1.0}
\item{shrinking}{shrinking. Default value = 0}
\item{seed}{Seed for random numbers. If it is not assigned a value, the seed will be a random number}
}
\value{
A data.frame with the actual and predicted values for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("autoMPG6_train")
data_test <- RKEEL::loadKeelDataset("autoMPG6_test")

#Create algorithm
algorithm <- RKEEL::EPSILON_SVR_R(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{regression}

