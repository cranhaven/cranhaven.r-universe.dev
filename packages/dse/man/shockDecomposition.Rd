\name{shockDecomposition}
\alias{shockDecomposition}
\title{Shock Decomposition}
\description{Graphs of the effect of shocks are plotted.}
\usage{
    shockDecomposition(model, horizon=30, shock=rep(1,horizon))
}
\arguments{
    \item{model}{An object of class TSmodel or TSestModel.}
    \item{horizon}{The number of periods for which to calculate the effect 
        of shocks.}
    \item{shock}{data to be used model output. See details.}
}
\value{None}
\section{Side Effects}{
Graphs of the effect of shocks are plotted. 
}
\details{All output data is set to zero and then each output in turn is switched to 
a value of shock (default 1.0) for all periods. 
}
\examples{
data("eg1.DSE.data.diff", package="dse")
model <- estVARXls(eg1.DSE.data.diff)
shockDecomposition(model)
}
\concept{DSE}
\keyword{ts}


%%\name{plot.mineStepwise}
%%\alias{plot.mineStepwise}
%%\title{Plot Mine Stepwise Object}
%%\description{plot.mineStepwise}
%%\usage{
%%    \method{plot}{mineStepwise}(x, ...)
%%}
%%\arguments{
%%    \item{x}{Object returned by mineStepwise.}
%%    \item{...}{(further arguments, currently disregarded).}
%%}
%%\value{None}
%%\section{Side Effects}{A plot}
%%\seealso{
%%    \code{\link{mineStepwise}}
%%}
%%\concept{DSE}
%%\keyword{ts}
%%\name{mineStepwise}
%%\alias{mineStepwise}
%%\title{Mine Stepwise}
%%\description{mineStepwise}
%%\usage{
%%    mineStepwise(data, essential.data=1,
%%	method="efroymson", f.crit=2, intercept=TRUE,
%%	subtract.means=FALSE,  standardize=FALSE, 
%%	lags.in=6, lags.out=6, trend=FALSE, plot.=TRUE)
%%}
%%\arguments{
%%    \item{data}{TSdata}
%%    \item{essential.data}{An integer vector indication important data.}
%%    \item{method}{method to pass to stepwise.}
%%    \item{f.crit}{See details.}
%%    \item{intercept}{See details.}
%%    \item{subtract.means}{See details.}
%%    \item{standardize}{See details.}
%%    \item{lags.in}{See details.}
%%    \item{lags.out}{See details.}
%%    \item{trend}{See details.}
%%    \item{plot.}{See details.}
%%}
%%\value{x}
%%\details{This documentation is out of date.
%%    Data should be of class TSdata. essential.data must have length 1.
%%    standardize and subtract means ...
%%    The result is a list with the results of stepwise,..., and
%%    several vectors indicating information about the columns of the matrix
%%    passed to stepwise: 
%%    io.indicator indicating an input (FALSE) or output (TRUE) variable
%%    v.indicator  indicating which series
%%    lag.indicator indicating the lag
%%    s.input.indicator and s.output.indicator are logic matrices
%%    \code{length(stepwise$rss)} by m and p respectively indicating if
%%    a series is included for each element of rss.
%%}
%%
%%\concept{DSE}
%%\keyword{ts}

