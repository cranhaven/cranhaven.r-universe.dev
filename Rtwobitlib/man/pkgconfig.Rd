\name{pkgconfig}

\alias{pkgconfig}

\title{Compiler configuration arguments for use of Rtwobitlib}

\description{
  The \code{pkgconfig} function prints values for \code{PKG_LIBS}
  and \code{PKG_CPPFLAGS} variables for use in \code{Makevars} files.
  It is not meant for the end user.
  See \code{vignette("Rtwobitlib")} for more information.
}

\usage{
pkgconfig(opt=c("PKG_LIBS", "PKG_CPPFLAGS"))
}

\arguments{
  \item{opt}{
    Either \code{"PKG_LIBS"} or \code{"PKG_CPPFLAGS"}
  }
}

\value{
  The function prints the \code{PKG_LIBS} or \code{PKG_CPPFLAGS} value
  and returns an invisible \code{NULL}.
}

\examples{
pkgconfig("PKG_LIBS")

pkgconfig("PKG_CPPFLAGS")
}

\keyword{manip}
