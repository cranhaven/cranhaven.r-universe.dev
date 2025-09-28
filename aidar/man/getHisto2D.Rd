\name{getHisto2D}
\alias{getHisto2D}
\title{retrieves a given 2D histogram by it's name from the given file and returns
it as a data.frame}
\usage{
getHisto2D(fileName, histoName)
}
\arguments{
  \item{fileName}{name of the AIDA file}

  \item{histoName}{name of the AIDA 2D histogram to be
  returned as a data.frame}
}
\description{
retrieves a given 2D histogram by it's name from the given
file and returns it as a data.frame
}
\examples{
histoFile = system.file("extdata", "histos.xml.gz", package="aidar")
h2 = getHisto2D(histoFile, '10')
}
\keyword{aida}
\keyword{histogram}

