\name{WaverR-package}
\alias{WaverR-package}
\alias{WaverR}
\docType{package}
\title{
Data Estimation using Weighted Averages of Multiple Regressions
}
\description{
For multivariate datasets, this function enables the estimation of missing data using the Weighted AVERage of all possible Regressions using the data available as performed by Sears et al (2008).}
\details{
The waverr function will estimate all missing values of a dataset by, initially performing  linear regressions of all possible combinations of variables using the data available, and subsequently using the average of all available values for each variable to estimate, weighting each estimation based on their log likelihood (see Sears et al 2008). Estimations of the error associated with each estimation are also provided.
}
\author{
Olivia Cheronet, John A. Finarelli
}
\references{
Sears, K.E., Finarelli, J.A., Flynn, J.J., & Wyss, A.R. (2008). Estimating body mass in New World "monkeys" (Platyrrhini, Primates), with a consideration of the Miocene platyrrhine, Chilecebus carrascoensis. \emph{American Museum Novitates}, \emph{3617}, 1-29.
}
\keyword{ Data Estimation }
\keyword{ Multiple Regressions }
\examples{
AllIrisMeasurements<-iris[,1:4]
Xremove<-sample(1:150, 20,replace=TRUE)
Yremove<-sample(1:4, 20,replace=TRUE)
AllIrisMeasurementsWithNA<-AllIrisMeasurements
for (i in 1:20){AllIrisMeasurementsWithNA[Xremove[i],Yremove[i]]<-NA}
waverr(RawData=AllIrisMeasurementsWithNA,Nrepeats=100)
}
