\docType{class}
\name{GFS_GSP_R}
\alias{GFS_GSP_R}
\alias{R6_GFS_GSP_R}
\title{GFS_GSP_R KEEL Regression Algorithm}
\description{
GFS_GSP_R Regression Algorithm from KEEL.
}
\usage{
GFS_GSP_R(train, test, numLabels, numRules, deltafitsap,
   p0sap, p1sap, amplMut, nsubsap, probOptimLocal,
   numOptimLocal, idOptimLocal, probcrossga, probmutaga,
   lenchaingap, maxtreeheight, numItera, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{numLabels}{numLabels. Default value = 3}
\item{numRules}{numRules. Default value = 8}
\item{deltafitsap}{deltafitsap. Default value = 0.5}
\item{p0sap}{p0sap. Default value = 0.5}
\item{p1sap}{p1sap. Default value = 0.5}
\item{amplMut}{amplMut. Default value = 0.1}
\item{nsubsap}{nsubsap. Default value = 10}
\item{probOptimLocal}{probOptimLocal. Default value = 0.00}
\item{numOptimLocal}{numOptimLocal. Default value = 0}
\item{idOptimLocal}{idOptimLocal. Default value = 0}
\item{probcrossga}{probcrossga. Default value = 0.5}
\item{probmutaga}{probmutaga. Default value = 0.5}
\item{lenchaingap}{lenchaingap. Default value = 10}
\item{maxtreeheight}{maxtreeheight. Default value = 8}
\item{numItera}{numItera. Default value = 10000}
\item{seed}{Seed for random numbers. If it is not assigned a value, the seed will be a random number}
}
\value{
A data.frame with the actual and predicted values for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("autoMPG6_train")
data_test <- RKEEL::loadKeelDataset("autoMPG6_test")

#Create algorithm
algorithm <- RKEEL::GFS_GSP_R(data_train, data_test)
algorithm <- RKEEL::GFS_GSP_R(data_train, data_test, numRules=2, numItera=10, maxtreeheight=2)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{regression}

