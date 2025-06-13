\name{odb.export}
\alias{odb.export}
\title{
  Exports an ODB database to a SQL file.
}

\description{
  Produces SQL queries describing the structure of the database and its content, and saves it to a file.
}

\usage{
  odb.export(odb, file)
}

\arguments{
  \item{odb}{
    An \code{\linkS4class{ODB}} object, as produced by \code{\link{odb.open}}.
}
  \item{file}{
    Path to the file in which export the SQL queries.
}
}

\value{
  Invisibly returns TRUE if succeeds, raises an error if not.
}

\author{
  Sylvain Mareschal
}

\note{
  It is just a wrapper for the "SCRIPT '[file]'" SQL query, as implemented in HSQLDB.
  
  Column comments and queries won't be exported, as they are not stored in the HSQL database.
}

\examples{
  # New empty .odb file
  odbFile <- tempfile(fileext=".odb")
  odb.create(odbFile, overwrite="do")
  odb <- odb.open(odbFile)
  
  # New table
  odb.write(odb, "CREATE TABLE fruits (name VARCHAR(6) PRIMARY KEY)")
  odb.insert(odb, "fruits", c("banana", "pear", "peach"))
  
  # Export to a file
  sqlFile <- tempfile(fileext=".odb")
  odb.export(odb, sqlFile)
  
  # Writes to the file and closes the connection
  odb.close(odb, write=TRUE)
}
