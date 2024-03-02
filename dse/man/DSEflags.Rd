\name{DSEflags}
\alias{.DSEflags}
\title{Flags to Indicate Use of Compiled Code}
\description{
    Determines if compiled code should be used or not.
    }
\usage{
    .DSEflags(new)
    }
\arguments{
    \item{new}{A list which must have elements COMPILED and DUP.}
     }
\details{
    Setting flags with this function is primarily for debugging. It should not
    normally be needed by users.  If called with no arguments, 
    \code{.DSEflags()} returns the current setting.
    Several \pkg{dse} functions which call compiled fortran or C code will use
    the equivalent S/R version if the \code{.DSEflags()$COMPILED} returns 
    \code{FALSE}. 
    }
\section{Side Effects}{
    The flag setting affects whether compiled fortran or C code is
    called. 
    }
\examples{
    .DSEflags(list(COMPILED=TRUE))
    .DSEflags()$COMPILED
}
\concept{DSE}
\keyword{programming}


