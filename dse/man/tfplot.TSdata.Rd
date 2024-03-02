\name{tfplot.TSdata}
\alias{tfplot.TSdata}
\alias{tfplot.TSestModel}

\title{Tfplot Specific Methods}
\description{See the generic function description.}
\usage{
    \method{tfplot}{TSdata}(x, ..., 
        tf=NULL, start=tfstart(tf), end=tfend(tf), 
        select.inputs  = seq(length=nseriesInput(x)),
        select.outputs = seq(length=nseriesOutput(x)),
        Title=NULL, xlab=NULL, ylab=NULL, 
	graphs.per.page=5, mar=par()$mar, reset.screen=TRUE)
    \method{tfplot}{TSestModel}(x, ..., 
        tf=NULL, start=tfstart(tf), end=tfend(tf), 
        select.inputs=NULL, select.outputs=NULL,
        Title=NULL, xlab=NULL, ylab=NULL, 
        graphs.per.page=5, mar=par()$mar, reset.screen=TRUE)
    }
\arguments{
    \item{x}{object to be plotted.}
    \item{...}{additional objects to be plotted.}
    \item{start}{start of plot.}
    \item{end}{end of plot.}
    \item{tf}{an alternate way to specify start and end of plot.}
    \item{select.inputs}{series to be plotted. (passed to selectSeries)}
    \item{select.outputs}{series to be plotted. (passed to selectSeries)}
    \item{Title}{string to use for plot title (passed to plot - see tfplot).}
    \item{xlab}{string to use for x label (passed to plot).}
    \item{ylab}{string to use for y label (passed to plot).}
    \item{graphs.per.page}{integer indicating number of graphs to place on a
       page.}
    \item{mar}{margins passed to plot. See par.)}
    \item{reset.screen}{logical indicating if the plot window should be cleared
       before starting. If this is not TRUE then mar values will have no effect.}
}

\seealso{ \code{\link[tfplot]{tfplot}} }

\concept{DSE}
\keyword{ts}

