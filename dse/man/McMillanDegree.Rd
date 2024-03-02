\name{McMillanDegree}
\alias{McMillanDegree}
\alias{McMillanDegree.ARMA}
\alias{McMillanDegree.SS}
\alias{McMillanDegree.TSestModel}

\title{Calculate McMillan Degree}
\description{

Calculate the McMillan degree of an ARMA TSmodel.
}
\usage{
    McMillanDegree(model,  ...)
    \method{McMillanDegree}{ARMA}(model, fuzz=1e-4, verbose=TRUE, warn=TRUE, ...)
    \method{McMillanDegree}{SS}(model, fuzz=1e-4, ...)
    \method{McMillanDegree}{TSestModel}(model,  ...)
}
\arguments{
    \item{model}{An object of class TSmodel.}
    \item{fuzz}{Roots within fuzz distance are counted as equivalent.}
    \item{verbose}{If TRUE roots are printed.}
    \item{warn}{If FALSE then warnings about unit roots added for TREND are not printed.}
    \item{...}{arguments to be passed to other methods.}
}
\value{
A list with elements gross and distinct containing all roots 
and distinct roots.
}
\section{Side Effects}{
The number of roots and distinct roots is printed if verbose is TRUE.
}
\seealso{

\code{\link{stability}}
}
\examples{
data("eg1.DSE.data.diff", package="dse")
model <- estVARXls(eg1.DSE.data.diff)
McMillanDegree(model)
}
\concept{DSE}
\keyword{ts}

