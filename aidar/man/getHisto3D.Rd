\name{getHisto3D}
\alias{getHisto3D}
\title{retrieves a given 3D histogram by it's name from the given file and returns
it as a data.frame}
\usage{
getHisto3D(fileName, histoName)
}
\arguments{
  \item{fileName}{name of the AIDA file}

  \item{histoName}{name of the AIDA 3D histogram to be
  returned as a data.frame}
}
\description{
retrieves a given 3D histogram by it's name from the given
file and returns it as a data.frame
}
\examples{
histoFile = system.file("extdata", "histos.xml.gz", package="aidar")
h3 = getHisto3D(histoFile, '13')
}
\keyword{aida}
\keyword{histogram}

