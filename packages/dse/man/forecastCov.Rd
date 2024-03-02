\name{forecastCov}
\alias{forecastCov}
\alias{forecastCov.TSdata}
\alias{forecastCov.TSmodel}
\alias{forecastCov.TSestModel}
\alias{is.forecastCov}

\title{Forecast covariance for different models}
\description{Calculate the forecast covariance for different models.}
\usage{
    is.forecastCov(obj)
    forecastCov(obj, ..., data=NULL, horizons=1:12, discard.before=NULL,
       zero=FALSE, trend=FALSE, estimation.sample= NULL, 
       compiled=.DSEflags()$COMPILED)
    \method{forecastCov}{TSmodel}(obj, ..., data=NULL,
       horizons=1:12, discard.before=NULL, 
       zero=FALSE, trend=FALSE, estimation.sample= Tobs(data), 
       compiled=.DSEflags()$COMPILED)
    \method{forecastCov}{TSestModel}(obj, ..., data=TSdata(obj), 
       horizons=1:12, discard.before=NULL, zero=FALSE, trend=FALSE, 
       estimation.sample= Tobs(TSdata(obj)), compiled=.DSEflags()$COMPILED)
    \method{forecastCov}{TSdata}(obj, ..., data=NULL,
       horizons=1:12, discard.before=1,
       zero=FALSE, trend=FALSE, estimation.sample= NULL,
        compiled=.DSEflags()$COMPILED)
}
\arguments{
    \item{obj}{TSdata or one or more TSmodels or TSestModels}
    \item{data}{an object of class TSdata.}
    \item{discard.before}{
       period before which forecasts should be discarded when calculating covariance.}
    \item{horizons}{horizons for which forecast covariance should be calculated.}
    \item{zero}{if TRUE the covariance is calculated for a forecast of zero.}
    \item{trend}{if TRUE the covariance is calculated for a forecast of trend.}
    \item{estimation.sample}{portion of the sample to use for calculating the trend.}
    \item{compiled}{a logical indicating if compiled code should be
      used. (Usually true except for debugging.)}
    \item{...}{arguments passed to other methods.}
}
\value{
    A list with the forecast covariance for supplied models on the given
    sample. This is in the element \code{forecastCov} of the result. Other
    elements contain information in the arguments.
}
\details{
Calculate the forecast cov of obj relative to data. If obj is TSdata
then the output data is used as the forecast. For other classes of obj
TSmodel(obj) is used with data to produce a forecast. TSmodel() is also
applied to each element of ... to extract a model. All models should
work with data. If obj is a TSestModel and data is NULL then TSdata(obj)
is used as the data.
This is multiple applications of forecastCovSingleModel
discard.before is an integer indicating the number of points in the
beginning of forecasts to discard before calculating covariances.
If it is the default, NULL, then the default (minimumStartupLag) will
be used for each model and the default (1) will be used for trend and zero.
If zero  is TRUE then forecastCov is also calculated for a forecast of zero.
If trend is TRUE then forecastCov is also calculated for a forecast of a linear 
trend using data to estimation.sample.
}
\examples{
    data("eg1.DSE.data.diff", package="dse")
    model1 <- estVARXar(eg1.DSE.data.diff)
    model2 <- estVARXls(eg1.DSE.data.diff)
    z <-  forecastCov(model1, model2, data=trimNA(eg1.DSE.data.diff))
    is.forecastCov(z)
}
\concept{DSE}
\keyword{ts}

