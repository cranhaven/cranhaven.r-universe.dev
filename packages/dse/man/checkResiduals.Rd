\name{checkResiduals}
\alias{checkResiduals}
\alias{checkResiduals.default}
\alias{checkResiduals.TSdata}
\alias{checkResiduals.TSestModel}

\title{Autocorrelations Diagnostics}
\description{Calculate autocorrelation diagnostics of a time series 
matrix or TSdata or residuals of a TSestModel}
\usage{
    checkResiduals(obj, ...)
    \method{checkResiduals}{default}(obj, ac=TRUE, pac=TRUE, select=seq(nseries(obj)), 
                   drop=NULL, plot.=TRUE, graphs.per.page=5, verbose=FALSE, ...)
    \method{checkResiduals}{TSdata}(obj, ...)
    \method{checkResiduals}{TSestModel}(obj, ...)
}
\arguments{
    \item{obj}{An TSestModel or TSdata object.}
    \item{ac}{If TRUE the auto-correlation function is plotted.}
    \item{pac}{If TRUE the partial auto-correlation function is plotted.}
    \item{select}{
      Is used to indicate a subset of the residual series. By default all
      residuals are used.}
    \item{drop}{
      Is used to indicate a subset of the residual time periods to drop. 
      All residuals are used with the default (NULL).Typically this can be used 
      to get rid of bad initial conditions (eg. drop=seq(10) ) or outliers.}
    \item{plot.}{If FALSE then plots are not produced.}
    \item{graphs.per.page}{Integer indicating number of graphs to place on a
    page.}
    \item{verbose}{
        If TRUE then the auto-correlations and partial auto-correlations 
        are printed if they are calculated.}
    \item{...}{arguments passed to other methods.}
}
\value{
    A list with residual diagnostic information: residuals, mean, cov, 
    acf= autocorrelations, pacf= partial autocorrelations.
}
\details{
This is a generic function. The default method works for a time series
matrix which is treated as if it were a matrix of residuals. However, in a
Box-Jenkins type of analysis the matrix may be data which is being evaluated to
determine a model. The method for a TSestModel evaluates the residuals
calculated by subtracting the output data from the model predictions.}

\section{Side Effects}{
    Diagnostic information is printed and plotted if a device is available.
Output graphics can be paused between pages by setting par(ask=TRUE).
}
\seealso{
\code{\link{informationTests}},
\code{\link{Portmanteau}}
}
\examples{
    data("eg1.DSE.data.diff", package="dse")
    model <- estVARXls(eg1.DSE.data.diff)
    checkResiduals(model)
}
\concept{DSE}
\keyword{ts}

