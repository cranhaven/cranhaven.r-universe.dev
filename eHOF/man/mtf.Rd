\encoding{UTF-8}
\name{mtf}
\alias{mtf}
\alias{mtf.env}
\docType{data}

\title{ Species Data and Altitude from Mt. Field, Tasmania}

\usage{
 data(mtf)
}

\description{
Species and Environmental Data from Mt. Field, Tasmania. The \code{acre} data frame has 100 selected sites (rows) 
and 6 species (columns). 
Data frame \code{mtf.env} has only one variable: \code{Altitude} above sea level (in meters) for each site.
The data are a subset of well drained sites from a more extensive data set. 
}

\format{
The species data frame contains the following species:
  \describe{
    \item{EPACSERP}{a numeric vector}
    \item{CYATPETI}{a numeric vector}
    \item{NOTHCUNN}{a numeric vector}
    \item{POA.GUNN}{a numeric vector}
    \item{BAUERUBI}{a numeric vector}
  }
}

\references{
Minchin, P.R. (1989). Montane vegetation of the Mt. Field massif, Tasmania: 
  a test of some hypotheses about properties of community patterns. \emph{Vegetatio 83}, 97.110.  }

\examples{
 data(mtf)
}

\keyword{datasets}

