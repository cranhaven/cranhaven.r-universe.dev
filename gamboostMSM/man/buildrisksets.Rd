\name{buildrisksets}
\alias{buildrisksets}
\title{Calculation of risksets}
\description{Calculates risksets needed for using family \code{multistate}.}
\usage{
buildrisksets(entry, exit, trans, event, statusinfo)
}
\arguments{
  \item{entry}{ a vector with entry times.}
  \item{exit}{ a vector with exit times.}
  \item{trans}{ a vector with transition types.}
  \item{event}{ a vector with noncensoring event indicators.}
  \item{statusinfo}{ a logical indicating if information on the
    calculation process should be printed.}
}
\details{This function calculates riksets needed for family \code{multistate}.}
\value{ A list of length 2 with elements \code{Ci} and \code{Ri}, each
  vectors of length \code{n}.
}
\author{Holger Reulen}