\name{getAnnotation}
\alias{getAnnotation}
\title{retrieves the annotation of a given AIDA object by it's name from the given file}
\usage{
getAnnotation(fileName, objectName)
}
\arguments{
  \item{fileName}{name of the AIDA file}

  \item{objectName}{name of the AIDA object for which the
  annotation is to be found}
}
\description{
retrieves the annotation of a given AIDA object by it's
name from the given file
}
\examples{
histoFile = system.file("extdata", "histos.xml.gz", package="aidar")
ann = getAnnotation(histoFile, '21')
}
\keyword{aida}
\keyword{annotation}

