\name{C_fBTi}
\Rdversion{1.2}
\encoding{UTF-8}
\alias{fBTi}

\title{Intra-daily time base}
\description{Construction of an intra-daily time base for solar
  irradiation calculation}

\usage{
fBTi(BTd, sample = 'hour')
}

\arguments{
  \item{BTd}{vector, it may be a result for \code{fBTd} or \code{indexD}}
  \item{sample}{character, identify the sample of the time set. Its
  default value is 'hour'.}
  }
\details{This function is commonly used inside \code{fSolI}.}
\value{This function returns a \code{POSIXct} object.}

\author{Oscar Perpiñán Lamigueiro,
  Francisco Delgado López.}

\examples{
library("data.table")
\dontshow{setDTthreads(2)}

#Average days
BTd <- fBTd(mode = 'prom')

#Intradaily base time for the first day
BTi <- fBTi(BTd = BTd[1], sample = 'hour')
BTi
}

\keyword{utilities}
