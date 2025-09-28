\name{getProfile2D}
\alias{getProfile2D}
\title{retrieves a given 2D profile histogram by it's name from the given file and returns
it as a data.frame}
\usage{
getProfile2D(fileName, histoName)
}
\arguments{
  \item{fileName}{name of the AIDA file}

  \item{histoName}{name of the AIDA 2D profile histogram to
  be returned}
}
\description{
retrieves a given 2D profile histogram by it's name from
the given file and returns it as a data.frame
}
\examples{
histoFile = system.file("extdata", "histos.xml.gz", package="aidar")
p2d = getProfile2D(histoFile, 'Example 2D profile (gauss)')
}
\keyword{aida}
\keyword{histogram}
\keyword{profile}

