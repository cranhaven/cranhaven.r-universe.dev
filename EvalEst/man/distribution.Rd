\name{distribution}
\alias{distribution}
\alias{distribution.default}
\alias{distribution.TSdata}

\alias{distribution.MonteCarloSimulations}
\title{Generate distribution plots of Monte Carlo simulations}
\description{Generate distribution plots of Monte Carlo simulations.}
\usage{
    distribution(obj, ...)
     \method{distribution}{TSdata}(obj, ..., bandwidth=0.2, 
             select.inputs = seq(length= nseriesInput(obj)),
             select.outputs= seq(length=nseriesOutput(obj)))
     \method{distribution}{default}(obj, ..., bandwidth=0.2, series=NULL)

    \method{distribution}{MonteCarloSimulations}(obj,
        series=seq(dim(obj$simulations)[2]),
        x.sections=TRUE, periods=1:3, graphs.per.page=5, ...)
}
\arguments{
    \item{obj}{The result of MonteCarloSimulations.}
    \item{bandwidth}{passed to \code{density} or \code{ksmooth}.}
    \item{series}{The series which should be plotted. The default gives all series.}
    \item{select.inputs}{series to be plotted. (passed to selectSeries)}
    \item{select.outputs}{series to be plotted. (passed to selectSeries)}
    \item{x.sections}{If TRUE then kernel density estimates are plotted 
       for periods indicated by periods. If FALSE then a time series plots 
       of the mean and estimates 1  and 2 standard deviations from the 
       mean. Periods is ignored if x.sections is FALSE.}
    \item{periods}{
       The periods at which the distribution should be calculated and plotted.
       The default gives the first three.}
    \item{graphs.per.page}{integer indicating number of graphs to place on a
       page.}
    \item{...}{(further arguments, currently disregarded).}
    \item{select}{integer vector indicating roots to be plotted. If select 
        is not NULL then roots are sorted by magnitude and only the 
	indicated roots are plotted. For example, select=c(1,2)  will plot only 
	the two largest roots.}
}
\value{None}
\details{
Kernel estimates of the densities (series by series, not joint densities) are estimated using ksmooth (if available) or density (if available) to produces density plots.
Output graphics can be paused between pages by setting par(ask=TRUE).
}
\seealso{
\code{\link{tfplot.MonteCarloSimulations}}
}
\examples{
data("eg1.DSE.data.diff", package="dse")
model <- estVARXls(eg1.DSE.data.diff)
z <-  MonteCarloSimulations(model)
distribution(z)
}
\concept{DSE}
\keyword{ts}

