\name{refTable-constructor}
\alias{refTable}
\title{
  refTable class constructor
}

\description{
  This function returns a new \code{\linkS4class{refTable}} object from various arguments.
  
  Notice the \code{new()} alternative can be used to produce an empty object, setting only the fields not the content.
}

\usage{
  refTable(..., row.names, warn = TRUE)
}

\arguments{
  \item{\dots}{
    A \code{data.frame}, a \code{list} or a set of named vectors to use as columns. For list and vectors, \code{row.names} will be used as row names if provided.
}
  \item{row.names}{
    Character vector, the names of the rows for list or vector input.
}
  \item{warn}{
    Single logical value, to be passed to the \code{\linkS4class{refTable}} \code{check} method.
}
}

\value{
  An object of class \code{\linkS4class{refTable}}.
}

\author{
  Sylvain Mareschal
}

\seealso{
  \code{\link{refTable-class}}
}

\examples{
  # From vectors
  tab <- refTable(colA=1:5, colB=letters[1:5])
  print(tab$extract(3,))
  
  # From list (recycling)
  columns <- list(number=1, letters=LETTERS)
  tab <- refTable(columns)
  print(tab$extract())
  
  # data.frame conversion
  dataFrame <- data.frame(colA=1:5, colB=letters[1:5])
  tab <- refTable(dataFrame)
  print(tab$extract())
}
