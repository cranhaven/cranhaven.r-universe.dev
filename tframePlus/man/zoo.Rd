\name{tframe.zoo}
%%\alias{tfstart.zoo}
\alias{tfstart.zootframe}
%%\alias{tfend.zoo}
\alias{tfend.zootframe}
\alias{Tobs.zoo}
\alias{Tobs.zootframe}

\alias{tframe.zoo}
\alias{tfSet.zootframe}
\alias{tfSet.Date}
\alias{tfSet.POSIXct}
\alias{tfwindow.zoo}
\alias{tfL.zoo}
\alias{tfExpand.zoo}

\title{zoo Methods for tframe}
\description{Methods to extend tframe of zoo series}

\usage{
%%    \method{tfstart}{zoo}(x)
    \method{tfstart}{zootframe}(x)
%%    \method{tfend}{zoo}(x)
    \method{tfend}{zootframe}(x)
    \method{Tobs}{zoo}(x)
    \method{tframe}{zoo}(x)
    \method{tfSet}{zootframe}(value, x) 
    \method{tfSet}{Date}(value, x) 
    \method{tfSet}{POSIXct}(value, x) 
    \method{tfwindow}{zoo}(x, tf=NULL, start=tfstart(tf), end=tfend(tf), warn=TRUE)
    \method{tfL}{zoo}(x, p = 1)
    \method{tfExpand}{zoo}(x, add.start = 0, add.end = 0)
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

