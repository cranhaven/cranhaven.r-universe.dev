\name{acf}
\alias{acf}
%%\alias{acf.default}
\alias{acf.TSdata}
\alias{acf.TSestModel}

\title{Calculate the acf for an object}
\usage{
%%    acf(x, ...)
%%    \method{acf}{default}(x, ...)
    \method{acf}{TSdata}(x, ...)
    \method{acf}{TSestModel}(x, ...)
}
\arguments{
    \item{x}{an object.}
    \item{...}{additional arguments passed to stats::acf.}
}
\description{
   Calculate the acf for an object.
}

\value{see stats::acf.}
\details{
    The default method uses \code{stats::acf}. The method 
    for TSdata extracts the output data and then uses acf.
    The method for TSestModel calculates the residuals 
    (prediction minus data) and then uses acf.}

\seealso{
   \code{stats::\link[stats]{acf}},
}
\author{Paul Gilbert}
\keyword{internal}


