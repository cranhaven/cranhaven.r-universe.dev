\name{detangler_data}
\docType{data}
\alias{detangler_data}
\title{Example object that is output from \code{tangles} function.}
\description{This output that would be derived from the \code{tangles} function. It contains the instructions for anonymisation of associated point pattern or raster data, or the re-identify tangled data.}
\usage{data(detangler_data)}

\format{
\code{detangler_data} is a \code{list} with 3 elements. The first element is a unique hash key for tracking data processing provenance. The second element contains the integer sequence of steps that were used to anonymize a given data sets. There are 3 possible anonymizing procedures: shift vertical (1), shift (horizontal), and rotate (3). The last element is a \code{data.frame} that encodes the values to perform either anonymization or re-identification, given the particular anonymization step used.}


\note{This data is the sort of output that one would expect when the \code{tangles} function is used.}

\references{
  \itemize{
    \item CM O’Keefe, S Otorepec, M Elliot, E Mackey, and K O’Hara (2017) The De-Identification Decision Making Framework. CSIRO Reports EP173122 and EP175702. \doi{10.4225/08/59c169433efd4}
  }
}

\examples{
data(detangler_data)
detangler_data
}
\keyword{datasets}
