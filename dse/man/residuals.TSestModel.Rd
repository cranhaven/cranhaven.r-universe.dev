\name{residuals.TSestModel}
\alias{residuals.TSestModel}

\title{Calculate the residuals for an object}
\usage{
    \method{residuals}{TSestModel}(object, ...)
}
\arguments{
    \item{object}{an object.}
    \item{...}{additional arguments passed to stats::residuals.}
}
\description{
   Calculate the residuals for an object.
}

\value{A time series matrix.}
\details{
    Calculates the residuals (prediction minus data).}
\author{Paul Gilbert}
\keyword{internal}

