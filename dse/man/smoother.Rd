\name{smoother}
\alias{smoother}
\alias{smoother.TSestModel}
\alias{smoother.TSmodel}
\alias{smoother.nonInnov}

\title{Evaluate a smoother with a TSmodel}
\description{Evaluate a state space model.}
\usage{
    smoother(model, data, compiled=.DSEflags()$COMPILED)
    \method{smoother}{nonInnov}(model, data, compiled=.DSEflags()$COMPILED)
    \method{smoother}{TSmodel}(model, data, compiled=.DSEflags()$COMPILED)
    \method{smoother}{TSestModel}(model, data=TSdata(model),
      compiled=.DSEflags()$COMPILED)
}
\arguments{
    \item{model}{
    An object of class `TSestModel' or 'TSmodel' with a model of 
class `nonInnov' 'SS' 'TSmodel'. If filter informatin is not provided 
(i.e. in a TSestModel)
then smoother runs the Kalman filter (l.SS) first.}
    \item{data}{A TSdata object.}.
    \item{compiled}{
    If TRUE the compiled version of the code is used. Otherwise the S version is used.}
}
\value{
An object of class TSestModel with an additional element \code{smooth}. 
\code{smooth} is a list of \code{state}, the smoothed state, and 
\code{track}, the smoothed tracking 
error. The result will also contain the element \code{filter} with \code{state}
and \code{track} (which may or may not have been in the original arguement). 
}
\details{
Calculate fixed interval smoother state values for a model.
Smoother first runs the filter and uses the filtered state to calculate 
a smoothed  estimate of the state (sometimes called a two sided filter).
The smoother requires an non-innovations form model. The method for a TSmodel
gives an error message if the model does not inherit from class
\code{nonInnov}.

Note: this does not allow the same option as \code{l.SS} for calculating over a
   sub-sample. Smoothing is done over the length of the available filter
   data (which will be calculated to the length of the data if not
   supplied). For models with an input, smoothing will only be done over the
   input data period if that is shorter than the filter data. 

See \code{\link{SS}} for details of the model:

       z(t) = Fz(t-1) + Gu(t) + Qe(t)
       y(t) = Hz(t)  + Rw(t)

}
\references{
   Anderson, B. D. O. and Moore, J. B. (1979) \emph{Optimal Filtering}. 
   Prentice-Hall.
   
   Shumway R. H. and Stoffer D.S. (1982) An Approach to Time Series Smoothing
   and Forecasting Using the EM Algorithm. 
   \emph{J. of Time Series Analysis}, \bold{3}, 253--264 (note
   appendix).
   
   Jazwinski, A. H. (1970) \emph{Stochastic Processes and Filtering Theory}. 
   Academic Press.
}

\seealso{
\code{\link{state}},
\code{\link{l}},
\code{\link{SS}}
\code{\link{l.SS}}
\code{\link{TSmodel}}
\code{\link{TSestModel.object}}
}
\examples{
data("eg1.DSE.data.diff", package="dse")
#smoother requires an non-innovations form model
model <- TSmodel(toSSChol(estVARXls(eg1.DSE.data.diff))) 
smoothed.model <- smoother(model, eg1.DSE.data.diff, compiled=FALSE)
tfplot(state(smoothed.model))
tfplot(state(smoothed.model, filter=TRUE))
#compare
tfplot(state(smoothed.model, smoother=TRUE), state(smoothed.model, filter=TRUE))
}
\concept{DSE}
\keyword{ts}

