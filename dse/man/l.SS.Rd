\name{l.SS}
\alias{l.SS}
\title{Evaluate a state space TSmodel}
\description{Evaluate a state space TSmodel.
}
\usage{
    \method{l}{SS}(obj1, obj2, sampleT=NULL, predictT=NULL, error.weights=0,
         return.state=FALSE, return.track=FALSE, result=NULL, 
	 compiled=.DSEflags()$COMPILED,
         warn=TRUE, return.debug.info=FALSE, ...)
}
\arguments{
    \item{obj1}{An 'SS' 'TSmodel' object.}
    \item{obj2}{A TSdata object.}
    \item{sampleT}{an integer indicating the last data point to use 
       for one step ahead filter estimation. If NULL all available data is used. }
    \item{predictT}{an integer indicating how far past the end of the 
       sample predictions should be made. For models with an input, 
       input data must be provided up to predictT. Output data is 
       necessary only to sampleT. If NULL predictT is set to sampleT.}
    \item{error.weights}{a vector of weights to be applied to the 
       squared prediction errors.}
    \item{return.state}{if TRUE the element \code{filter$state} 
       containing E[z(t)|y(t-1), u(t)] is returned as part of the 
       result. This can be a fairly large matrix.}
    \item{return.track}{if TRUE the element \code{filter$track} containing
       the expectation of the tracking error given y(t-1) and u(t) is 
       returned as part of the result. This can be an very large array. }
    \item{result}{if result is not specified an object of class 
       TSestModel is returned. Otherwise, the specified element 
       of \code{TSestModel$estimates} is returned.}
    \item{compiled}{if TRUE the compiled version of the code is used. 
       Otherwise the S/R version is used.}
    \item{warn}{if FALSE then certain warning messages are turned off.}
    \item{return.debug.info}{logical indicating if additional debugging
       information should be returned.}
    \item{...}{(further arguments, currently disregarded).}
}
\value{
Usually an object of class TSestModel (see TSestModel), but see result above.}

\details{
This function is called by the function l() when the argument to l is a state
space model. Using l() is usually preferable to calling l.SS directly.
l.SS calls a compiled program unless compiled=FALSE. The compiled version is much faster
than the S version.

Output data must be at least as long as sampleT. If sampleT is not supplied it
is taken to be Tobs(data).

Input data must be at least as long as predictT. predictT must be at least as
large as sampleT. If predictT is not supplied it
is taken to be sampleT.

If \code{error.weights} is greater than zero then weighted prediction 
errors are calculated up to the horizon indicated
by the length of error.weights. The weights are applied to the squared
error at each period ahead.


\code{sampleT} is the length of data which should be used for calculating
one step ahead predictions. \code{y} must be at least as
long as \code{sampleT}. If \code{predictT} is large than \code{sampleT} 
then the model is simulated to 
\code{predictT}. \code{y} is used if it is long enough. \code{u} must 
be at least as long as \code{predictT}.
The default \code{result=0} returns a list of all the results. Otherwise 
only the indicated list element is return (eg. \code{result=1} return the 
likelihood and \code{result=3} returns the one step ahead predictions.

If \code{z0} is supplied in the model object it is used as the estimate 
of the state at time 0. If not supplied it is set to zero.

If \code{rootP0} is supplied  in the model object then t(rootP0) \%*\% rootP0 is
used as P0. 
If \code{P0} is supplied or calculated from rootP0 in the model object, it is 
used as the initial 
tracking error P(t=1|t=0). If not supplied it is set to the identity matrix.

Additional objects in the result are
 \code{Om} is the estimated output cov matrix.
 \code{pred} is the time series of the one-step ahead predictions, E[y(t)|y(t-1),u(t)].
 The series of prediction error is given by \code{y - pred}
     If error.weights is greater than zero then weighted prediction 
     errors are calculated up to the horizon indicated
     by the length of error.weights. The weights are applied to the squared
     error at each period ahead.
 \code{trackError} is the time series of P, the one step ahead estimate 
       of the state tracking error matrix at each 
       period, Cov(z(t)-E[z(t)|t-1])
       The tracking error can only be calculated if Q and R are provided 
       (i.e. non innovations form models).
       Using the Kalman Innov K directly these are not necessary 
       for the likelihood calculation,
       but the tracking error cannot be calculated.
}

\references{
   Anderson, B. D. O. and Moore, J. B. (1979) \emph{Optimal Filtering}. 
   Prentice-Hall. (note p.39,44.)
}

\seealso{
\code{\link{SS}}
\code{\link{l}}
\code{\link{l.ARMA}}
\code{\link{TSmodel}}
\code{\link{TSestModel}}
\code{\link{TSestModel.object}}
\code{\link{state}}
\code{\link{smoother}}
}
\examples{
data("eg1.DSE.data.diff", package="dse")
model <- toSS(TSmodel(estVARXls(eg1.DSE.data.diff)))
lmodel <- l(model,eg1.DSE.data.diff)
summary(lmodel)
tfplot(lmodel)
lmodel <- l(model,eg1.DSE.data.diff, return.state=TRUE)
tfplot(state(lmodel, filter=TRUE))
}
\concept{DSE}
\keyword{ts}

