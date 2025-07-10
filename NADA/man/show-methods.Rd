\name{show-methods}
\docType{methods}
\alias{show-methods}
%\alias{show,ANY,ANY-method}
\alias{show,ros-method}
\alias{show,cenmle-method}
%\alias{show,cenmle-lognormal,ANY-method}
%\alias{show,cenmle-gaussian,ANY-method}
\alias{show,summary.cenreg-method}
\alias{show,cenfit-method}
\alias{show,cenreg-method}
\alias{show,censummary-method}
\alias{show,NADAList-method}
\alias{show,cenken-method}
\title{Methods for function show in Package NADA}
\description{
  Methods for showting objects in package \pkg{NADA} 
}
\usage{
\S4method{show}{ros}(object)

\S4method{show}{cenfit}(object)

\S4method{show}{cenmle}(object)

\S4method{show}{cenreg}(object)

\S4method{show}{summary.cenreg}(object)

\S4method{show}{cenken}(object)

\S4method{show}{censummary}(object)

\S4method{show}{NADAList}(object)

} % End usage

\arguments{

\item{object}{
    An output object from a NADA function such as \code{\link{cenfit}}.
}
} % End arguments

\seealso{
    \code{\link{show}}
}
\keyword{methods}
