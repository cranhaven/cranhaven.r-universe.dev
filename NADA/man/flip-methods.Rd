\name{flip-methods}
\docType{methods}
\alias{flip}
\alias{flip-methods}
\alias{flip,Cen-method}
\alias{flip,formula-method}
\title{Methods for function flip in Package NADA}
\description{
    Methods for function \code{flip} in package \pkg{NADA}.

    When used in concert with \code{Cen}, \code{flip} 
    rescales left-censored data into right-censored data
    for use in the survival package routines (which can only handle
    right-censored data sets).

}
\usage{
\S4method{flip}{Cen}(x)

\S4method{flip}{formula}(x)
} % End usage

\arguments{
\item{x}{A \code{Cen} or \code{formula} object.}
} % End arguments

\section{Notes}{
    Flips, or rescales a \code{Cen} object or a formula object.

    By default, \code{flip} rescales the input data by subtracting a
    large constant that is larger than maximum input value from all
    observations.  It then marks the data as right censored so that
    routines from the \code{survival} package can be used.

    IMPORTANT: All NADA routines transparently handle flipping and
    re-transforming data.  Thus, \code{flip} should almost never be used,
    except perhaps in the development of an extension function.

    Also, flipping a Cen object results in a Surv object -- 
    which presently cannot be flipped back to a Cen object!

    Flipping a formula just symbolically updates the 
    response (which should be a Cen object). 
    Result is like: flip(Cen(obs, cen))~groups

} % End Notes

\keyword{methods}

