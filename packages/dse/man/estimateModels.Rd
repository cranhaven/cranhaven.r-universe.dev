\name{estimateModels}
\alias{estimateModels}
\alias{is.estimatedModels}

\title{Estimate Models}
\description{Estimate models using given estimation method}
\usage{
    estimateModels(data, estimation.sample = NULL, trend =FALSE, quiet =FALSE,
                       estimation.methods = NULL)
    is.estimatedModels(obj)
}
\arguments{
    \item{data}{An object of class TSdata.}
    \item{estimation.methods}{
       A named list with the names indicating the estimation method and the
       value associated with the name is a list of arguments for each the
       method indicated. Its value should be NULL if no args are needed.}
    \item{estimation.sample}{
       An integer indicating the number of points in the sample to use for
       estimation. If it is NULL the whole sample is used.}
    \item{trend}{
       If trend is TRUE then a linear trend is calculated and returned as
       the element \code{trend.coef}.}
    \item{quiet}{
       If quiet is TRUE then most printing and some warning messages are
       suppressed.}
    \item{obj}{An object.}
}
\value{
Element \code{multi.model} in the result is a list of the same length as 
estimation.methods with resulting models as elements.
}
\details{
    Estimate models from data with estimation methods indicated by 
    estimation.methods. This is primarily a utility for other functions.
}
\seealso{

\code{\link[EvalEst]{EstEval}},
\code{\link{outOfSample.forecastCovEstimatorsWRTdata}}
}
\examples{
data("eg1.DSE.data.diff", package="dse")
z <-  estimateModels(eg1.DSE.data.diff, estimation.methods= list(
        bft=list(verbose=FALSE), 
        estVARXar=list(max.lag=3)))
}
\concept{DSE}
\keyword{ts}

