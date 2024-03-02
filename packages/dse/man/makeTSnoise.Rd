\name{makeTSnoise}
\alias{makeTSnoise}
\title{Generate a random time series}
\description{
    Generate a random time series (matrix). This is a utility typically used in a time
    series model simulate method and not called directly by the user.}
\usage{
    makeTSnoise(sampleT,p,lags,noise=NULL, rng=NULL, Cov=NULL, sd=1, 
	     noise.model=NULL, noise.baseline=0,
             tf=NULL, start=NULL,frequency=NULL)
}
\arguments{
    \item{sampleT}{an integer indicating the number of periods.}
    \item{p}{an integer indicating the number of series.}
    \item{lags}{an integer indicating the number of periods prior to the sample
       (initial data w0) for which random numbers should be generated. This is
       useful in ARMA models.}
    \item{noise}{Noise can be supplied. Otherwise it will be generated.
        If supplied it should be a list as described below under returned value.}
    \item{Cov}{The covariance of the noise process. If this is specified then sd
       is ignored. A vector or scalar is treated as a diagonal matrix. For an object of class
       TSestModel, if neither Cov nor sd are specified, then Cov is set to
       the estimated covariance (\code{model$estimates$cov}).}
    \item{sd}{The standard deviation of the noise. This can be a vector.}
    \item{noise.model}{A TSmodel to be used for generating noise (not yet
       supported by SS methods).}
    \item{noise.baseline}{a constant or matrix to be added to noise. Alternately this
       can be a vector of length p, each value of which is treated as a constant to
       add to the coresponding noise series.}
    \item{rng}{The random number generator information needed to
       regenerate a simulation.}
    \item{tf}{a time frame to use for the generated matrix. (alternately
       use start and frequency)}
    \item{start}{a time start date to use for the generated matrix.}
    \item{frequency}{a time frequency to use for the generated matrix.}
}
\value{A time series matrix.}
\keyword{internal}
