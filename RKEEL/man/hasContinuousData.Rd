\docType{methods}
\name{hasContinuousData}
\alias{hasContinuousData}
\title{Has Continuous Data}
\description{
Method for check if a dataset has continuous data
}
\usage{
hasContinuousData(data)
}
\arguments{
\item{data}{Dataset as data.frame}
}
\value{
Returns TRUE if the dataset has continuous data and FALSE if it has not.
}
\examples{
iris <- RKEEL::loadKeelDataset("iris")
hasContinuousData(iris)
}
\keyword{utils}

