\name{isClosed}
\alias{isClosed}
\title{
  Checks if a DBI connection is closed
}

\description{
  Checks if a connection inheriting from the \code{DBIConnection} class (including 'ODB' connections) is closed.
}

\usage{
  isClosed(connection)
}

\arguments{
  \item{connection}{
    An object inheriting from \code{DBIConnection} class.
}
}

\value{
  Returns a single logical value, whether SQL queries can be sent to the connection or not.
}

\author{
  Sylvain Mareschal
}

\seealso{
  \code{\link{odb.open}}, \code{\link{odb.close}}
  
  \code{\link{dbConnect}}, \code{\link{dbDisconnect}}
}

\examples{
  # New empty .odb file
  odbFile <- tempfile(fileext=".odb")
  odb.create(odbFile, overwrite="do")
  ODB <- odb.open(odbFile)
  
  # Is the connection closed ?
  print(isClosed(ODB))
  
  # Save changes in the .odb file
  odb.close(ODB, write=TRUE)
  
  # And now ?
  print(isClosed(ODB))
}
