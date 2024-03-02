\name{scale.TSdata}
\alias{scale.TSdata}
\alias{scale.TSestModel}
\alias{scale.ARMA}
\alias{scale.innov}
\alias{scale.nonInnov}
\alias{checkScale}
\alias{checkScale.TSestModel}
\alias{checkScale.TSmodel}

\title{Scale Methods for TS objects}
\description{Scale data or a model by a given factor.}
\usage{
    \method{scale}{TSdata}(x, center=FALSE, scale=NULL)
    \method{scale}{TSestModel}(x, center=FALSE, scale=NULL)
    \method{scale}{ARMA}(x, center=FALSE, scale=NULL)
    \method{scale}{innov}(x, center=FALSE, scale=NULL)
    \method{scale}{nonInnov}(x, center=FALSE, scale=NULL)
    
    checkScale(x, scale)
    \method{checkScale}{TSestModel}(x, scale)
    \method{checkScale}{TSmodel}(x, scale)
    }
\arguments{
    \item{x}{TSdata, TSmodel or an object containing these.}
    \item{center}{to match generic arguments, not currently used.}
    \item{scale}{
    A list with two matrices or vectors, named input and output,
    giving the multiplication factor for inputs and outputs.
    Vectors are treated as diagonal matrices.
    \code{scale$input} can be NULL if no transformation is to be applied (or 
    the data or model has no input.)}
}

\value{
    The resulting data or model is different from
    the original in proportion to scale.  ie. if S and T are output and 
    input scaling matrices then 
         y'(t) = S y(t) where y' is the new output
         u'(t) = S u(t) where u' is the new input

    For models the result has inputs and outputs (and innovations) 
    which are scaled as if data scaling had been applied to them as above. 
    Thus if the input and output scales are diagonal matrices or 
    scalars the plot of the predictions and residuals for
    l(scale(model,scale=somescale), scale(data, scale=somescale))
    while have the same appearance as l(model, data) but will be 
    scaled differently.
}
\seealso{
    \code{\link[base]{scale}}
}
\examples{
    data("eg1.DSE.data.diff", package="dse")
    # This is a simple example. Usually scale would have something 
    # to do with the magnitude of the data.
    z <- scale(eg1.DSE.data.diff, 
        scale=list(input=rep(2, nseriesInput(eg1.DSE.data.diff)), 
                  output=rep(2,nseriesOutput(eg1.DSE.data.diff))))
    model <- estVARXls(eg1.DSE.data.diff)
    model <- scale(model, 
        scale=list(input=rep(2, nseriesInput(eg1.DSE.data.diff)), 
                  output=rep(2,nseriesOutput(eg1.DSE.data.diff))))
}
\concept{DSE}
\keyword{ts}

