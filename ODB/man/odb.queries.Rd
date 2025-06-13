\name{odb.queries}
\alias{odb.queries}
\alias{odb.queries<-}
\title{
  Gets or sets stored queries in an ODB database
}

\description{
  This function allows stored SQL queries manipulation into OpenOffice Base databases, through an \code{odb} connection.
}

\usage{
  odb.queries(odb, queryNames = NULL)
  odb.queries(odb, queryNames) <- value
}

\arguments{
  \item{odb}{
    An \code{\linkS4class{ODB}} object, as produced by \code{\link{odb.open}}.
}
  \item{queryNames}{
    Character vector naming queries to get or set.
}
  \item{value}{
    Character vector, containing the SQL queries to store.
}
}

\value{
  Returns a named character vector of SQL queries.
}

\author{
  Sylvain Mareschal
}

\note{
  The user is required to check and keep its queries up-to-date himself.
  
  Queries can also be manipulated manually from OpenOffice.
}

\section{Queries and Views}{
  These functions manipulate OpenOffice \bold{queries}, which are stored in the .odb file and not the database itself. They should not be confused with \bold{views}, which are SQL features handled by the database engine.
  
  Views are more portable (as they are stored in the database), and can be accessed as virtual tables in SQL queries. To manage them, user is required to use the SQL queries "CREATE VIEW" and "DROP VIEW" (with the \code{odb.write} in this package context). More informations on these SQL queries can be found in the HSQL documentation.
}

\examples{
  # New empty .odb file
  odbFile <- tempfile(fileext=".odb")
  odb.create(odbFile, overwrite="do")
  odb <- odb.open(odbFile)
  
  # New table
  odb.write(odb, "CREATE TABLE fruits (name VARCHAR(6) PRIMARY KEY)")
  odb.insert(odb, "fruits", c("banana", "pear", "peach"))
  
  # Single query
  odb.queries(odb, "banana") = "SELECT * FROM fruits WHERE name='banana'"
  print(odb.read(odb, odb.queries(odb, "banana")))
  
  # Multiple queries
  odb.queries(odb, c("banana","pear")) <- c(
    "SELECT * FROM fruits WHERE name='banana'",
    "SELECT * FROM fruits WHERE name='pear'"
  )
  
  # All queries
  print(odb.queries(odb))
  
  # Writes to the file and closes the connection
  odb.close(odb, write=TRUE)
}

