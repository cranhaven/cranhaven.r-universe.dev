\name{tframe.xts}
%%\alias{tfstart.xts}
\alias{tfstart.xtstframe}
%%\alias{tfend.xts}
\alias{tfend.xtstframe}
\alias{Tobs.xts}
\alias{Tobs.xtstframe}

\alias{tframe.xts}
\alias{tfSet.xtstframe}
\alias{tfwindow.xts}
\alias{tfL.xts}
\alias{tfExpand.xts}

\title{xts Methods for tframe}
\description{Methods to extend tframe of xts series}

\usage{
%%    \method{tfstart}{xts}(x)
    \method{tfstart}{xtstframe}(x)
%%    \method{tfend}{xts}(x)
    \method{tfend}{xtstframe}(x)
    \method{Tobs}{xts}(x)
    \method{tframe}{xts}(x)
    \method{tfSet}{xtstframe}(value, x) 
    \method{tfwindow}{xts}(x, tf=NULL, start=tfstart(tf), end=tfend(tf), warn=TRUE)
    \method{tfL}{xts}(x, p = 1)
    \method{tfExpand}{xts}(x, add.start = 0, add.end = 0)
}

\arguments{
    \item{x}{a tframe or a tframed object.}
    \item{value}{a tframe.}
    \item{start}{A start date of a format compatible with the time series}
    \item{end}{An end date of a format compatible with the time series}
    \item{tf}{A tframe or tframed object}
    \item{p}{number of periods to lag}
    \item{add.start}{number of periods to add to the start}
    \item{add.end}{number of periods to add to the end}
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

