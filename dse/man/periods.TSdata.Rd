\name{Tobs.TSdata}
\alias{Tobs.TSdata}
\alias{Tobs.TSestModel}
\alias{start.TSdata}
\alias{start.TSestModel}
\alias{end.TSdata}
\alias{end.TSestModel}
\alias{frequency.TSdata}
\alias{frequency.TSestModel}

\title{Specific Methods for tframed Data}
\description{See the generic function description.}
\usage{
    \method{Tobs}{TSdata}(x, ...)
    \method{Tobs}{TSestModel}(x)
    \method{start}{TSdata}(x, ...)
    \method{start}{TSestModel}(x, ...)
    \method{end}{TSdata}(x, ...)
    \method{end}{TSestModel}(x, ...)
    \method{frequency}{TSdata}(x, ...)
    \method{frequency}{TSestModel}(x, ...)
}
\arguments{
    \item{x}{a time series object.}
    \item{...}{(further arguments, currently disregarded).}
}

\seealso{
    \code{\link[tframe]{Tobs}}, 
    \code{\link[tframe]{tfstart}},
    \code{\link[tframe]{tfend}},
    \code{\link[tframe]{tffrequency}}
    \code{\link[stats]{start}},
    \code{\link[stats]{end}},
    \code{\link[stats]{frequency}}
}
\concept{DSE}
\keyword{ts}

