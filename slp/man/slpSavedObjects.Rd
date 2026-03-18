\name{slpSavedObjects}
\alias{slpSavedObjects}
\alias{slpSavedObject}
\title{slp: Listing of available pre-computed basis sets}
\description{
    List of pre-computed basis sets included with the \code{slp} package.
    For large \code{N}, significant speed-ups can be obtained by pre-computing
    the basis set and simply loading it from disk. 
}
\usage{slpSavedObjects}
\format{A list containing the \code{N}, \code{W} (in \code{df/}year) and 
    \code{K} of the pre-computed basis sets. Each combination is included as a
    \code{data(...)} loadable \code{.RData} file.}
\keyword{datasets}
