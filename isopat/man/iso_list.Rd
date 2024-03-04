\name{iso_list}
\alias{iso_list}
\docType{data}
\title{
Isotope masses and abundances
}
\description{
Data frame of isotope masses and abundances for a number of elements. 
}
\usage{data(iso_list)}
\format{
  A data frame with four columns:
  \describe{
    \item{\code{element}}{name of element}
    \item{\code{isotope}}{isotopes of an element}
    \item{\code{mass}}{mass of an isotope of an element}
    \item{\code{abundance}}{abundance of an isotope of an element}
  }
}
\details{
The data frame (or any other data frame with that format) serves as argument for the isopattern function.
The first column of the data frame also contains names of specific [isotopes] used for labeled compounds.
}
\source{
www.iupac.org/publications/pac/2003/pdf/7506x0683.pdf
}
\references{
  \code{\link[isopat]{isopattern}}
}
\examples{
data(iso_list)
}
\keyword{datasets}
