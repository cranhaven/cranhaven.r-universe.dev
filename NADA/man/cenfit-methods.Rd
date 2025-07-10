\name{cenfit-methods}
\docType{methods}
\alias{cenfit-methods}
\alias{cenfit,formula,missing,missing-method}
\alias{cenfit,Cen,missing,missing-method}
\alias{cenfit,numeric,logical,missing-method}
\alias{cenfit,numeric,logical,factor-method}

%\alias{cenfit,numeric,logical}
%\alias{cenfit,numeric,logical,factor}
%\alias{cenfit,Cen}
%\alias{cenfit,formula}

\title{Methods for function cenfit in Package NADA}
\description{
%    Methods for function \code{cenfit} in package \pkg{NADA}.
    See \code{\link{cenfit}} for all the details.
}
%\usage{
%
%\S4method{cenfit}{numeric,logical}(obs, censored, ...)
%
%\S4method{cenfit}{numeric,logical,factor}(obs, censored, groups, ...)
%
%\S4method{cenfit}{Cen}(obs, ...)
%
%\S4method{cenfit}{formula}(obs, ...)
%
%} % End usage
%
%\arguments{
%
%\item{obs}{
%    Either a numeric vector of observations or a formula.
%    See examples in \code{\link{cenfit}}.
%} 
%\item{censored}{
%    A logical vector indicating TRUE where an observation in `obs' is
%    censored (a less-than value) and FALSE otherwise.
%}
%\item{groups}{
%    A factor vector used for grouping `obs' into subsets.
%}
%\item{...}{
%    Addtional items that are common to this function and the \code{survfit}
%    function from the `survival' package.  See Details.
%}
%} % End arguments
%
%\seealso{
%    \code{\link{cenfit}}, \code{\link{Cen}}, \code{\link{survfit}}
%}
\examples{
    data(Atrazine)

    cenfit(Atrazine$Atra, Atrazine$AtraCen)
    cenfit(Atrazine$Atra, Atrazine$AtraCen, Atrazine$Month)

    cenfit(Cen(Atrazine$Atra, Atrazine$AtraCen))
    cenfit(Cen(Atrazine$Atra, Atrazine$AtraCen)~Atrazine$Month)
}
\keyword{methods}
