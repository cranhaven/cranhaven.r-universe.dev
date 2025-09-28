\name{getCloud1D}
\alias{getCloud1D}
\title{retrieves a given 1D cloud by it's name from the given file and returns
it as a data.frame}
\usage{
getCloud1D(fileName, cloudName)
}
\arguments{
  \item{fileName}{name of the AIDA file}

  \item{cloudName}{name of the AIDA 1D cloud to be
  returned}
}
\description{
retrieves a given 1D cloud by it's name from the given file
and returns it as a data.frame
}
\examples{
histoFile = system.file("extdata", "clouds.xml.gz", package="aidar")
c1d = getCloud1D(histoFile, '21')
}
\keyword{aida}
\keyword{cloud}

