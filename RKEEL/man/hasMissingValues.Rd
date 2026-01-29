\docType{methods}
\name{hasMissingValues}
\alias{hasMissingValues}
\title{Has Missing Values}
\description{
Method for check if a dataset has missing values
}
\usage{
hasMissingValues(data)
}
\arguments{
\item{data}{Dataset as data.frame}
}
\value{
Returns TRUE if the dataset has missing values and FALSE if it has not.
}
\examples{
iris <- RKEEL::loadKeelDataset("iris")
hasMissingValues(iris)
}
\keyword{utils}

