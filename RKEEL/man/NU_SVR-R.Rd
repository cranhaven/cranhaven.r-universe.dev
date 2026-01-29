\docType{class}
\name{NU_SVR_R}
\alias{NU_SVR_R}
\alias{R6_NU_SVR_R}
\title{NU_SVR_R KEEL Regression Algorithm}
\description{
NU_SVR_R Regression Algorithm from KEEL. 
}
\usage{
NU_SVR_R(train, test, KernelType, C, eps, degree, gamma,
   coef0, nu, p, shrinking, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{KernelType}{KernelType. Default value = ?}
\item{C}{C. Default value = ?}
\item{eps}{eps. Default value = ?}
\item{degree}{degree. Default value = ?}
\item{gamma}{gamma. Default value = ?}
\item{coef0}{coef0. Default value = ?}
\item{nu}{nu. Default value = ?}
\item{p}{p. Default value = ?}
\item{shrinking}{shrinking. Default value = ?}
\item{seed}{Seed for random numbers. If it is not assigned a value, the seed will be a random number}
}
\value{
A data.frame with the actual and predicted values for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("autoMPG6_train")
data_test <- RKEEL::loadKeelDataset("autoMPG6_test")

#Create algorithm
algorithm <- RKEEL::NU_SVR_R(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{regression}

