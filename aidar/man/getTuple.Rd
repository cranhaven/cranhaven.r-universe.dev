\name{getTuple}
\alias{getTuple}
\title{retrieves a given tuple by it's name from the given file and returns
it as a data.frame}
\usage{
getTuple(fileName, tupName)
}
\arguments{
  \item{fileName}{name of the AIDA file}

  \item{tupName}{name of the AIDA tuple to be returned}
}
\description{
retrieves a given tuple by it's name from the given file
and returns it as a data.frame
}
\examples{
tupleFile = system.file("extdata", "tuple.xml.gz", package="aidar")
t100 = getTuple(tupleFile, '100')
}
\keyword{aida}
\keyword{tuple}

