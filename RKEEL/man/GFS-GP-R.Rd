\docType{class}
\name{GFS_GP_R}
\alias{GFS_GP_R}
\alias{R6_GFS_GP_R}
\title{GFS_GP_R KEEL Regression Algorithm}
\description{
GFS_GP_R Regression Algorithm from KEEL. 
}
\usage{
GFS_GP_R(train, test, numLabels, numRules, popSize, numisland,
   steady, numIter, tourSize, mutProb, aplMut, probMigra,
   probOptimLocal, numOptimLocal, idOptimLocal, nichinggap,
   maxindniche, probintraniche, probcrossga, probmutaga,
   lenchaingap, maxtreeheight, seed)
}
\arguments{
\item{train}{Train dataset as a data.frame object}
\item{test}{Test dataset as a data.frame object}
\item{numLabels}{numLabels. Default value = 3}
\item{numRules}{numRules. Default value = 8}
\item{popSize}{popSize. Default value = 30}
\item{numisland}{numisland. Default value = 2}
\item{steady}{steady. Default value = 1}
\item{numIter}{numIter. Default value = 100}
\item{tourSize}{tourSize. Default value = 4}
\item{mutProb}{mutProb. Default value = 0.01}
\item{aplMut}{aplMut. Default value = 0.1}
\item{probMigra}{probMigra. Default value = 0.001}
\item{probOptimLocal}{probOptimLocal. Default value = 0.00}
\item{numOptimLocal}{numOptimLocal. Default value = 0}
\item{idOptimLocal}{idOptimLocal. Default value = 0}
\item{nichinggap}{nichinggap. Default value = 0}
\item{maxindniche}{maxindniche. Default value = 8}
\item{probintraniche}{probintraniche. Default value = 0.75}
\item{probcrossga}{probcrossga. Default value = 0.5}
\item{probmutaga}{probmutaga. Default value = 0.5}
\item{lenchaingap}{lenchaingap. Default value = 10}
\item{maxtreeheight}{maxtreeheight. Default value = 8}
\item{seed}{Seed for random numbers. If it is not assigned a value, the seed will be a random number}
}
\value{
A data.frame with the actual and predicted values for both \code{train} and \code{test} datasets.
}
\examples{
data_train <- RKEEL::loadKeelDataset("autoMPG6_train")
data_test <- RKEEL::loadKeelDataset("autoMPG6_test")

#Create algorithm
algorithm <- RKEEL::GFS_GP_R(data_train, data_test)

#Run algorithm
algorithm$run()

#See results
algorithm$testPredictions
}
\keyword{regression}

