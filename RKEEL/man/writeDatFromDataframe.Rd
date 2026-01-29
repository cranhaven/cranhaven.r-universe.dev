\docType{methods}
\name{writeDatFromDataframe}
\alias{writeDatFromDataframe}
\title{Write .dat from data.frame}
\description{
Method for writing a .dat dataset file in KEEL format given a data.frame dataset
}
\usage{
writeDatFromDataframe(data, fileName)
}
\arguments{
\item{data}{data.frame dataset}
\item{fileName}{String with the file name to store the dataset}
}
\examples{
data(iris)
writeDatFromDataframe(iris, paste0(tempdir(), "/iris.dat"))
}
\keyword{utils}

