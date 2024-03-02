\name{tfplot.MonteCarloSimulations}
\alias{tfplot.MonteCarloSimulations}
\title{Generate plots of Monte Carlo simulations}
\description{Generate plots of Monte Carlo simulations.}
\usage{
    \method{tfplot}{MonteCarloSimulations}(x, 
    tf=tframe(x$simulations), start=tfstart(tf), end=tfend(tf),
    series=seq((dim(x$simulations)[2])), 
    select.simulations=seq(dim(x$simulations)[3]),
    graphs.per.page=5, mar=par()$mar, ...)
}
\arguments{
    \item{x}{The result of MonteCarloSimulations.}
    \item{tf}{The time frame for plots. see \code{tfplot}.}
    \item{start}{The starting period for plots, taken from tf by default.}
    \item{end}{The ending period for plots, taken from tf by default.}
    \item{series}{
       The series which should be plotted. The default NULL gives all series.}
    \item{select.simulations}{
    Vector of integers indicating the simulations which should be plotted. 
    The default plots all simulations.}
    \item{graphs.per.page}{The number of graphs to put on a page.}
    \item{mar}{Plot margins (see \code{par}).}
    \item{...}{arguments passed to other methods.}
}
\value{None}
\details{
This function produces plots of the simulated series.
Output graphics can be paused between pages by setting par(ask=TRUE).
}
\seealso{
\code{\link{distribution.MonteCarloSimulations}}
}
\examples{
data("eg1.DSE.data.diff", package="dse")
model <- estVARXls(eg1.DSE.data.diff)
z <-  MonteCarloSimulations(model)
tfplot(z)
}
\concept{DSE}
\keyword{ts}

