\name{TSdata}
\alias{TSdata}
\alias{TSdata.default}
\alias{TSdata.TSdata}
\alias{TSdata.TSestModel}
\alias{is.TSdata}
\alias{as.TSdata}

\title{Construct TSdata time series object}
\description{
    Constructor for constructing or extracting a \code{TSdata} object 
    (use by \code{TSmodels}).
}
\usage{
    TSdata(data=NULL, ...)
    \method{TSdata}{default}(data=NULL, input=NULL, output=NULL, ...)
    \method{TSdata}{TSdata}(data, ...)
    \method{TSdata}{TSestModel}(data, ...)
    is.TSdata(obj)
    as.TSdata(d)
}
\arguments{
    \item{data}{object of class TSdata, TSestModel, matrix, list with input
       and output matrices, or another object for which a constructor or 
       TSdata extraction method has been defined.}
    \item{input}{a matrix of time series data.}
    \item{output}{a matrix of time series data.}
    \item{...}{arguments to be passed to other methods.}
    \item{obj}{an object.}
    \item{d}{an object from which a TSdata object can be extracted. See below.}
}
\details{
    Generic method to construct or extract a \code{TSdata} object. The default 
    method constructs a \code{TSdata} object. 
    Specific methods extract the \code{TSdata}
    from other objects (which must contain \code{TSdata}).
    The function \code{is.TSdata(data)} returns \code{TRUE} if data inherits 
    from \code{TSdata} and \code{FALSE} otherwise.

    The function \code{as.TSdata} uses the elements input and output 
    directly and
    strips away other class information and parts of the object (and does 
    not make use of inputData(data) or outputData(data) which may 
    do something special for certain classes.
}
\examples{
   rain  <- matrix(rnorm(86*17), 86,17)
   radar <- matrix(rnorm(86*5),  86,5)
   mydata <- TSdata(input=radar, output=rain)
}
\seealso{
\code{\link{TSdata.object}},
\code{\link{TSmodel}},
\code{\link{TSestModel.object}}
}
\concept{DSE}
\keyword{ts}


