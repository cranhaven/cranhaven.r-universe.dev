\name{tfVisPlot}
\alias{tfVisPlot}

\title{Plot Tframed Objects using googleVis}
\description{
    Plot tframe or tframed objects using googleVis, which allows pointing to
    lines on the plot in a browser to display extra information. 
}
\usage{
    tfVisPlot(x, tf = tframe(x), start = tfstart(tf), end = tfend(tf), 
                  options=list(title=NULL), ...)  
}
\arguments{
    \item{x}{a tframe or tframed object to plot.}
    \item{...}{any additional tframed objects for the same plot.}
    \item{start}{start of plot. (passed to tfwindow)}
    \item{end}{end of plot. (passed to tfwindow)}
    \item{tf}{a tframe or tframed object which can be used to specify start and
    end.}
    \item{options}{passed to googleVis, including title.}
}
\value{None.}
\details{
This function produces a line plot of time series \code{x} in a web browser
using \code{gvisLineChart} from package \pkg{googleVis}. The advantage of 
this relative to \code{tfplot} and \code{tfOnePlot} is that additional 
information about the series or points are displayed when the mouse pointer 
is close to a point. This can be useful, for example, to distinguish a 
particular vintage among several vintages in a graph. 
See package \pkg{googleVis} for more details.
}
\section{Side Effects}{An object is plotted in a browser.}
\seealso{
    \code{\link{tfplot}},
    \code{\link{tfOnePlot}},
    \code{\link[googleVis]{gvisLineChart}}
}
\examples{
 \dontrun{
  z <- ts(matrix(rnorm(1000),100,10), start=c(1982,1), frequency=12)
  seriesNames(z) <- paste("Series", 1:10) 
  if (requireNamespace("googleVis")) 
     tfVisPlot(z, options=list(title="Random Number Series"))
  }
}
\keyword{ts}
\keyword{plot}


