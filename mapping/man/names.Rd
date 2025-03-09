\name{names}
\alias{namesWR}
\alias{namesEU}
\alias{namesIT}
\alias{namesUS}
\alias{namesDE}
\alias{namesFR}
\alias{namesUK}

\docType{data}

\title{Statistical Unit Names}
\description{Statistical unit names.}

\usage{
data("namesWR")
data("namesEU")
data("namesIT")
data("namesUS")
data("namesDE")
data("namesFR")
data("namesUK")
}


\format{A list with all names divided for year and type of units.}

\details{Look at \code{\link{getNamesWR}}, \code{\link{getNamesEU}}, \code{\link{getNamesIT}}, \code{\link{getNamesUS}}, \code{\link{getNamesUK}}, \code{\link{getNamesDE}}, \code{\link{getNamesFR}}}

\source{
World Bank, Eurostat, United States Census and Istat}



\examples{
data(namesWR)
str(namesWR)

data(namesEU)
str(namesEU)

data(namesIT)
str(namesIT)

data(namesUS)
str(namesUS)
}

\keyword{datasets}
