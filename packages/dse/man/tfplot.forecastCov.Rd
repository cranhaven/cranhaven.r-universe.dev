\name{tfplot.forecastCov}
\alias{tfplot.forecastCov}
\alias{tfplot.forecastCovEstimatorsWRTdata}

\title{Plots of Forecast Variance}
\description{Generate plots of forecast variance calculated by forecastCov.}
\usage{
    \method{tfplot}{forecastCov}(x, ...,
       series = 1:dim(x$forecastCov[[1]])[2], 
       select.cov = 1:length(x$forecastCov), select.true =TRUE, 
       select.zero =TRUE, select.trend =TRUE, y.limit = NULL, line.labels =FALSE, 
       lty = NULL, Legend = NULL, Title = NULL,
       graphs.per.page = 5, mar=par()$mar, reset.screen=TRUE)
    \method{tfplot}{forecastCovEstimatorsWRTdata}(x, 
        series=1:dim(x$forecastCov[[1]])[2], 
        select.cov=1:length(x$forecastCov),
        select.zero=TRUE, select.trend=TRUE,
        graphs.per.page = 5, mar=par()$mar, reset.screen=TRUE, lty=NULL, ...)
}
\arguments{
    \item{x}{The result of forecastCov.}
    \item{series}{integer or string indicating
      the series which should be plotted.}
    \item{select.cov}{logical indicating that
      for the case of multiple models select the covariance to be plotted.}
    \item{select.true}{logical indicating that results from the forecast of the 
      true model (if available) should be plotted.}
    \item{select.zero}{logical indicating that results from a forecast of 
      zero should be plotted.}
    \item{select.trend}{logical indicating that results from a forecast of 
      trend should be plotted.}
    \item{graphs.per.page}{The number of graphs to put on a page.}
    \item{mar}{plot margins (see \code{par}).}
    \item{reset.screen}{logical indicating if the plot window should be cleared
       before starting.}
    \item{lty}{see details.}
    \item{Legend}{optional legend passed to \code{legend}.}
    \item{Title}{optional legend passed to \code{title} (but see details).}
    \item{y.limit}{optional limit on the y scale. Covariance values larger
       than y.limit will not be shown.}
    \item{line.labels}{logical indicating line labels should be printed.}
    \item{...}{For forecastCov objects this allows additional objects to be 
       plotted. For forecastCovEstimatorsWRTdata ... are passed to other 
       methods.}
}
\value{None}
\details{
    This function produces plots of the variance at different horizons.
Output graphics can be paused between pages by setting par(ask=TRUE).
If lty is NULL (default) it is set to
     seq(length(select.cov) +select.true+select.zero+select.trend),
 and corrected if these are TRUE but not in the object.

    The \code{Title} is not put on the plot if the global option
    PlotTitles is FALSE. This can be set with \code{options(PlotTitles=FALSE)}. 
    This provides a convenient  mechanism to omit all titles when the title
    may be added separately (e.g. in Latex).
}
\seealso{
    \code{\link{plot}}
}
\examples{
    data("eg1.DSE.data.diff", package="dse")
    model <- estVARXls(eg1.DSE.data.diff)
    z <- forecastCov(model, data=eg1.DSE.data.diff)
    tfplot(z)
}
\concept{DSE}
\keyword{ts}

