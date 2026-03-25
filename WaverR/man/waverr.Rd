\name{waverr}
\alias{waverr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Data Estimation using Weighted Averages of Multiple Regressions
}
\description{
For multivariate datasets, this function enables the estimation of missing data using the Weighted AVERage of all possible Regressions using the data available as performed by Sears et al (2008).
}
\usage{
waverr(RawData, Nrepeats)
}
\arguments{
  \item{RawData}{
A dataframe with each individual represented by a separate row and each variable by a column. Missing data coded as NA.
}
  \item{Nrepeats}{
The number of repeats performed for the estimation of the error associated with reconstructed data.
}
}
\value{
\item{ReconstructedData}{A matrix of the original data with the missing data replaced by estimated values. It is also saved as a text file in the working directory as "ReconsructedData.txt".}
\item{ReconstructedDataStDev}{A matrix of the standard deviation associated with each estimated value. It is also saved as a text file in the working directory as "ReconsructedDataStDev.txt".}
\item{ResampledData}{A three-dimensional array in which each estimated value is resampled (number of repeats determined by Nrepeats) according to a normal distribution. It is also saved as a text file in the working directory as "ResampledReconstructions.txt".}
\item{VariableStDev}{A matrix of the error associated with each estimated data point as a function of the standard deviation of Nrepeat resampled estimations. It is also saved as a text file in the working directory as "ResampledReconstructionsStandardDeviation.txt".}
}
\references{
Sears, K.E., Finarelli, J.A., Flynn, J.J., & Wyss, A.R. (2008). Estimating body mass in New World "monkeys" (Platyrrhini, Primates), with a consideration of the Miocene platyrrhine, Chilecebus carrascoensis. \emph{American Museum Novitates}, \emph{3617}, 1-29.
}
\author{
Olivia Cheronet, John A. Finarelli
}
\examples{
AllIrisMeasurements<-iris[,1:4]
Xremove<-sample(1:150, 20,replace=TRUE)
Yremove<-sample(1:4, 20,replace=TRUE)
AllIrisMeasurementsWithNA<-AllIrisMeasurements
for (i in 1:20){AllIrisMeasurementsWithNA[Xremove[i],Yremove[i]]<-NA}
waverr(RawData=AllIrisMeasurementsWithNA,Nrepeats=100)
}
\keyword{ Data Estimation }
\keyword{ Multiple Regressions }