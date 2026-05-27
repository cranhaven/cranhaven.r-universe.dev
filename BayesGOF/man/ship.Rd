\name{ship}
\alias{ship}
\docType{data}
\title{
Portsmouth Navy Shipyard Data
}
\description{
Data represents results of quality-control inspections executed by Portsmouth Naval Shipyard on lots of welding materials.  The data has \eqn{k=5} observations of number of defects \eqn{y} out of the total number of tested \eqn{n=5}.
}
\usage{data("ship")}
\format{
  A data frame of \eqn{(y_i, n_i)} for \eqn{i = 1,...,5}.
  \describe{
    \item{\code{y}}{number of defects found in the \eqn{i^{th}} inspection}
    \item{\code{n}}{total samples tested in the \eqn{i^{th}} inspection}
  }
}
\source{ Martz, H.F. and Lian, M.G., 1974. "Empirical Bayes estimation of the binomial parameter," \emph{Biometrika, 61}(3), pp. 517-523.}

\keyword{datasets}
