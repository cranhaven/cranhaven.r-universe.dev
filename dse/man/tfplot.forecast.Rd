\name{tfplot.forecast}
\alias{tfplot.forecast}
\alias{tfplot.featherForecasts}
\alias{tfplot.horizonForecasts}
\alias{tfplot.multiModelHorizonForecasts}

\title{Specific Methods for tfplot}
\description{See the generic function description.}
\usage{
    \method{tfplot}{forecast}(x, tf=NULL, start=tfstart(tf), end=tfend(tf),
        series = seq(length=nseriesOutput(x$data)),
	Title="Predictions (dotted) and actual data (solid)",
        ylab = seriesNamesOutput(x$data), 
	graphs.per.page=5, mar=par()$mar, reset.screen=TRUE, ...)
    \method{tfplot}{featherForecasts}(x, tf=NULL, start=tfstart(tf), end=tfend(tf), 
        series=seq(nseries(x)), 
        Title="Predictions (dotted) and actual data (solid)", 
        ylab=seriesNamesOutput(x),
        graphs.per.page=5, mar=par()$mar, reset.screen=TRUE, ...)
    \method{tfplot}{horizonForecasts}(x, tf=NULL, start=tfstart(tf), end=tfend(tf),
         series=seq(length=nseriesOutput(x$data)),
	 Title="Predictions (dotted) and actual data (solid)", 
	 ylab=seriesNamesOutput(x$data), 
	 graphs.per.page=5, mar=par()$mar, reset.screen=TRUE, ...)
    \method{tfplot}{multiModelHorizonForecasts}(x, 
         tf=NULL, start=tfstart(tf), end=tfend(tf), series=NULL, ...)
}
\arguments{
    \item{x}{an object for which a tfplot is to be produced.}
    \item{tf}{see \code{tfplot}.}
    \item{start}{see \code{tfplot}.}
    \item{end}{see \code{tfplot}.}
    \item{Title}{string of characters to use for title.}
    \item{ylab}{vector of strings for y axis labelling.}
    \item{graphs.per.page}{integer indicating number of graphs to place on a
       page.}
    \item{reset.screen}{logical indicating if the plot window should be cleared
       before starting.}
    \item{series}{integer or string indicating
      the series which should be plotted.}
    \item{mar}{plot margins. See \code{par}.}
    \item{...}{arguments passed to other methods.}
}

\seealso{
    \code{\link[tfplot]{tfplot}}
    \code{\link[EvalEst]{EstEval}} }

\concept{DSE}
\keyword{ts}

