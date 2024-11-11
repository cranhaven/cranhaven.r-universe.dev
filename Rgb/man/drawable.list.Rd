\name{drawable.list}
\alias{drawable.list}
\title{
  Drawable list constructor
}

\description{
  Produces a \code{\linkS4class{drawable.list}} object, for \code{\link{tk.browse}} and \code{\link{browsePlot}} input.
}

\usage{
  drawable.list(files = character(0), objects = NULL, hidden = FALSE, warn = TRUE)
}

\arguments{
  \item{files}{
    Character vector, path and names of the files holding the drawables object (with .rdt or .rds extensions, see \code{\link{saveRDT}} and \code{\link{saveRDS}} for further details). Can contain \code{NA} values, but providing a path allows updates to be saved.
}
  \item{objects}{
    List of \code{\linkS4class{drawable}}-inheriting objects to include in the list. If \code{NULL}, objects will be extracted from \code{files}, else a list with as many elements as \code{files} is required.
}
  \item{hidden}{
    Logical vector, whether to show tracks in \code{\link{tk.browse}} and \code{\link{browsePlot}} or not (this choice is not definitive). Recycled if necessary.
}
  \item{warn}{
    Single logical value, to be passed to the appropriate \code{check} method.
}
}

\value{
  A \code{\linkS4class{drawable.list}} object.
}

\author{
  Sylvain Mareschal
}

\seealso{
  \code{\link{drawable.list-class}}, \code{\link{drawable-class}}, \code{\link{tk.browse}}, \code{\link{browsePlot}}
}

