\name{tframe.timeSeries}
%%\alias{tfstart.timeSeries}
\alias{tfstart.timeSeriestframe}
%%\alias{tfend.timeSeries}
\alias{tfend.timeSeriestframe}
\alias{Tobs.timeSeries}
\alias{Tobs,timeSeries-method}
\alias{Tobs.timeSeriestframe}
\alias{tbind.timeSeriestframe}
\alias{tbind.timeSeries}

\alias{is.tframed.timeSeries}
\alias{seriesNames<-,timeSeries-method}
\alias{is.tframed,timeSeries-method}
\alias{tbind,timeSeries-method}

\alias{tframe.timeSeries}
\alias{tframe,timeSeries-method}

\alias{tfSet.timeSeriestframe}
\alias{tfwindow.timeSeries}
\alias{tfwindow,timeSeries-method}
\alias{tfL.timeSeries}
\alias{tfL,timeSeries-method}
\alias{tfExpand.timeSeries}
\alias{tfExpand,timeSeries-method}

\title{timeSeries Methods for tframe}
\description{Methods to extend tframe for timeSeries series}

\usage{
%%    \method{tfstart}{timeSeries}(x)
    \method{tfstart}{timeSeriestframe}(x)
%%    \method{tfend}{timeSeries}(x)
    \method{tfend}{timeSeriestframe}(x)
      \method{Tobs}{timeSeries}(x)
    \S4method{Tobs}{timeSeries}(x)
      \method{tframe}{timeSeries}(x)
    \S4method{tframe}{timeSeries}(x)
      \method{is.tframed}{timeSeries}(x)
    \S4method{is.tframed}{timeSeries}(x)
      \method{tbind}{timeSeries}(x, ..., pad.start=TRUE, pad.end=TRUE, warn=TRUE)
    \S4method{tbind}{timeSeries}(x, ..., pad.start=TRUE, pad.end=TRUE, warn=TRUE)

    \method{tfSet}{timeSeriestframe}(value, x) 
    \method{tfwindow}{timeSeries}(x, tf=NULL, start=tfstart(tf), end=tfend(tf), warn=TRUE)
      \method{tfL}{timeSeries}(x, p = 1)
    \S4method{tfL}{timeSeries}(x, p = 1)
      \method{tfExpand}{timeSeries}(x, add.start = 0, add.end = 0)
    \S4method{tfExpand}{timeSeries}(x, add.start = 0, add.end = 0)
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

