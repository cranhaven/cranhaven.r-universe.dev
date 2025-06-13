\name{odb.close}
\alias{odb.close}
\title{
  Closes an "ODB" connection and updates the .odb file
}

\description{
  Closes the connection to the embedded HSQLDB, removing temporary files and updating the .odb file if asked to do so.
}

\usage{
  odb.close(odb, write = TRUE)
}

\arguments{
  \item{odb}{
    An \code{\linkS4class{ODB}} object, as produced by \code{\link{odb.open}}.
}
  \item{write}{
    Single logical value, defining if the modifications has to be saved in the original .odb file or not. Notice they will be definitively lost if closing with write=FALSE.
}
}

\value{
  Invisibly returns TRUE if succeeds, raises an error if not.
}

\author{
  Sylvain Mareschal
}

\note{
  To take data in a first file and save it in an other, copy the file before using \code{\link{odb.open}}.
}

\seealso{
  \code{\link{odb.open}}
}

\examples{
  # New empty .odb file
  odbFile <- tempfile(fileext=".odb")
  odb.create(odbFile, overwrite="do")
  odb <- odb.open(odbFile)
  
  # New table
  odb.write(odb, "CREATE TABLE fruits (name VARCHAR(6) PRIMARY KEY)")
  odb.insert(odb, "fruits", c("banana", "pear", "peach"))
  
  # Writes to the file and closes the connection
  odb.close(odb, write=TRUE)
}
