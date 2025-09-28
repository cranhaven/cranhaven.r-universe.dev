\name{getFileInfo}
\alias{getFileInfo}
\title{lists the content of a given AIDA file.}
\usage{
getFileInfo(fileName)
}
\arguments{
  \item{fileName}{name of the AIDA file}
}
\description{
This function lists the context of a given AIDA file. The
AIDA file should have been written out in "uncompressed"
format which subsequently can be gzip compressed.
}
\examples{
histoFile = system.file("extdata", "histos.xml.gz", package="aidar")
info = getFileInfo(histoFile)
}
\keyword{aida}
\keyword{file}

