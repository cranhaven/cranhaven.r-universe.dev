\name{tfplot.TSdata.ee}
\alias{tfplot.TSmodelEstEval}
\alias{tfplot.TSestModelEstEval}
\alias{tfplot.EstEval}

\title{Specific Methods for tfplot}
\description{See the generic function description.}
\usage{
    \method{tfplot}{TSmodelEstEval}(x, graph.args=NULL,
                       criterion ="coef", criterion.args=NULL, ...)
    \method{tfplot}{TSestModelEstEval}(x, graph.args=NULL,
                       criterion ="coef", criterion.args=NULL, ...)
    \method{tfplot}{EstEval}(x, tf=NULL, start=tfstart(tf), end=tfend(tf),
        truth= if(is.TSdata(x$truth)) outputData(x$truth) else x$truth,
        series = seq(length=nseries(truth)),
	Title="Estimated (and true) results",
        ylab = seriesNames(truth), remove.mean = FALSE,
	graphs.per.page=5, mar=par()$mar, reset.screen=TRUE, ...)
}
\arguments{
    \item{x}{an object for which a tfplot is to be produced.}
    \item{tf}{see \code{tfplot}.}
    \item{start}{see \code{tfplot}.}
    \item{end}{see \code{tfplot}.}
    \item{truth}{true value which will be plotted along with estimates.}
    \item{Title}{string of characters to use for title.}
    \item{remove.mean}{logical indicating if means should be removed
          before plotting results.}
    \item{ylab}{vector of strings for y axis labelling.}
    \item{graphs.per.page}{integer indicating number of graphs to place on a
       page.}
    \item{reset.screen}{logical indicating if the plot window should be cleared
       before starting.}
    \item{series}{integer or string indicating
      the series which should be plotted.}
    \item{mar}{plot margins. See \code{par}.}
    \item{graph.args}{list of graphics arguments eventually passed 
       to \code{plot}. See \code{par}.}
    \item{criterion}{criterion which should be used to extract something from
         the object which will then be plotted. See \code{EstEval}.}
    \item{criterion.args}{arguments to be passed to \code{criterion}.}
    \item{...}{arguments passed to other methods.}
}

\seealso{
    \code{\link[tfplot]{tfplot}}
    \code{\link{EstEval}} 
    }

\concept{DSE}
\keyword{ts}

