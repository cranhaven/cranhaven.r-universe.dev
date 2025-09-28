\name{getProfile1D}
\alias{getProfile1D}
\title{retrieves a given 1D profile histogram by it's name from the given file and returns
it as a data.frame}
\usage{
getProfile1D(fileName, histoName)
}
\arguments{
  \item{fileName}{name of the AIDA file}

  \item{histoName}{name of the AIDA 1D profile histogram to
  be returned}
}
\description{
retrieves a given 1D profile histogram by it's name from
the given file and returns it as a data.frame
}
\examples{
histoFile = system.file("extdata", "histos.xml.gz", package="aidar")
p1d = getProfile1D(histoFile, 'Example profile (gauss)')
}
\keyword{aida}
\keyword{histogram}
\keyword{profile}

