\name{artificialLongData}
\Rdversion{1.1}
\alias{artificialLongData}
\docType{data}

\title{~ Data: artificialLongData ~}

\description{
  Some artificial longitudinal data.
}

\usage{data(artificialLongData)}

\format{
  Some longitudinal data in wide format. It includes 120 trajectories
  divided in 4 groups.

  \describe{
    \item{\code{id}}{unique idenfier for each patient.}
    \item{\code{t0}}{Measurment at time t0}
    \item{\code{t1}}{Measurment at time t1}
    \item{\code{...}}{...}
    \item{\code{t10}}{Measurment at time t10}
  }
}

\details{
  Some artificial longitudinal data in wide format. It includes 120 trajectories
  divided in 4 groups.
}

\author{Christophe Genolini}

\examples{
data(artificialLongData)
str(artificialLongData)
}

\keyword{datasets}
\keyword{documentation}


