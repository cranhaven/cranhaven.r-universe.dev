\name{tframe.tis}
\alias{tfstart.tis}
\alias{tfstart.tistframe}
\alias{tfend.tis}
\alias{tfend.tistframe}
\alias{Tobs.tis}
\alias{Tobs.tistframe}

\alias{tframe.tis}
\alias{tfSet.tistframe}
\alias{tfwindow.tis}

\title{tis Methods for tframe}
\description{Methods to extend tframe for tis series}

\usage{
    \method{tfstart}{tis}(x)
    \method{tfstart}{tistframe}(x)
    \method{tfend}{tis}(x)
    \method{tfend}{tistframe}(x)
    \method{Tobs}{tis}(x)
    \method{tframe}{tis}(x)
    \method{tfSet}{tistframe}(value, x) 
    \method{tfwindow}{tis}(x, tf=NULL, start=tfstart(tf), end=tfend(tf), warn=TRUE)
}

\arguments{
    \item{x}{a tframe or a tframed object.}
    \item{value}{a tframe.}
    \item{start}{A start date of a format compatible with the time series}
    \item{end}{An end date of a format compatible with the time series}
    \item{tf}{A tframe or tframed object}
    \item{warn}{A logical indicating if warning should be produced}
}
\value{Depends}
\details{
    These methods extend tframe generics.
}

\seealso{
    \code{\link[tframe]{tframe}}, 
    \code{\link[tframe]{tfwindow}}
    \code{\link[tframe]{tfstart}}, 
    \code{\link[tframe]{tfend}}
    \code{\link[tframe]{tftime}}
    \code{\link[stats]{time}}
    \code{\link[tframe]{time.tframed}}
}
\keyword{internal}

