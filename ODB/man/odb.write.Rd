\name{odb.write}
\alias{odb.write}
\title{
  Executes writing SQL queries in an ODB database (INSERT ...)
}

\description{
  Executes an SQL query expecting no output through an "odb" connection.
}

\usage{
  odb.write(odb, sqlQueries, onError = c("warning", "stop"),
    progress = c("console", "file", "none"))
}

\arguments{
  \item{odb}{
    An \code{\linkS4class{ODB}} object, as produced by \code{\link{odb.open}}.
}
  \item{sqlQueries}{
    Single or multiple character vector, with queries to be executed ordered in distinct elements. ";" query separation should not be used.
}
  \item{onError}{
    Single character vector, defining how to handle SQL errors (using \code{\link{warning}} or \code{\link{stop}}). If "sqlQueries" contains more than one query, "stop" will prevent remaining queries to be executed, while "warning" won't.
}
  \item{progress}{
    Single character vector, the type of progression to print when multiple queries are to be executed (See \code{\link{progress-class}} for further details).
}
}

\author{
  Sylvain Mareschal
}

\note{
  To query databases built with OpenOffice or LibreOffice, it may be necessary to quote table and/or column names in \code{sqlQueries}, as the default behavior of the HSQL engine is to convert unquoted table and column names to uppercases.
}

\seealso{
  \code{\link{odb.read}}
}

\examples{
  # New empty .odb file
  odbFile <- tempfile(fileext=".odb")
  odb.create(odbFile, overwrite="do")
  odb <- odb.open(odbFile)
  
  # New table
  odb.write(odb, "CREATE TABLE fruits (name VARCHAR(6) PRIMARY KEY)")
  odb.insert(odb, "fruits", c("banana", "pear", "peach"))
  print(odb.tables(odb))
  
  # Manual insert
  odb.write(odb, "INSERT INTO fruits VALUES('apple')")
  print(odb.read(odb, "SELECT * FROM fruits"))
  
  # Remove table
  odb.write(odb, "DROP TABLE fruits")
  print(odb.tables(odb))
  
  # Writes to the file and closes the connection
  odb.close(odb, write=TRUE)
}

