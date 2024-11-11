\name{findDrawables}
\alias{findDrawables}
\title{
  Find drawable objects in memory
}
\description{
  This function searches an environment for \code{\link{drawable-class}} inheriting objects.
}

\usage{
  findDrawables(varNames = NA, envir = globalenv())
}

\arguments{
  \item{varNames}{
    Character vector, the R expression(s) of potential \code{\link{drawable-class}} inheriting objects (or lists of such objects) to check. If \code{NA}, an object name list will be generated from \code{envir}. Non-\code{NA} values are mainly used for internal recursive calls on lists, users should not have to change it.
}
  \item{envir}{
    The \code{environment} to look into for \code{\link{drawable-class}} inheriting objects.
}
}

\details{
  Objects are currently found if defined as individual variables, as parts of \code{\linkS4class{drawable.list}} objects or into standard R \code{lists}. \code{lists} are explored recursively, so \code{lists} embedded into other \code{lists} are explored too, whatever their depths.
}

\value{
  Returns a character vector containing the R expression(s) to be evaluated in \code{envir} to get the \code{\link{drawable-class}} inheriting objects.
  
  This vector carries an "envir" attribute containing the value passed to this function via the \code{envir} argument.
}

\author{
  Sylvain Mareschal
}

\seealso{
  \code{\link{tk.tracks}}
}

