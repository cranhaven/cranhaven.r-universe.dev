\name{getHisto1D}
\alias{getHisto1D}
\title{retrieves a given 1D histogram by it's name from the given file and returns
it as a data.frame}
\usage{
getHisto1D(fileName, histoName)
}
\arguments{
  \item{fileName}{name of the AIDA file}

  \item{histoName}{name of the AIDA 1D histogram to be
  returned as a data.frame}
}
\description{
retrieves a given 1D histogram by it's name from the given
file and returns it as a data.frame
}
\examples{
histoFile = system.file("extdata", "histos.xml.gz", package="aidar")
h1 = getHisto1D(histoFile, '1')
}
\keyword{aida}
\keyword{histogram}

