\name{odb.read}
\alias{odb.read}
\title{
  Executes a reading SQL query in an ODB database (SELECT ...)
}

\description{
  Executes an SQL query expecting an output through an "odb" connection.
}

\usage{
  odb.read(odb, sqlQuery, stringsAsFactors = FALSE, check.names = FALSE,
    encode = TRUE, autoLogical = TRUE)
}

\arguments{
  \item{odb}{
    An \code{\linkS4class{ODB}} object, as produced by \code{\link{odb.open}}.
}
  \item{sqlQuery}{
    A single character value, containing an unique SQL query to be executed.
}
  \item{stringsAsFactors}{
    Single logical value : should character columns in the output be turned to factor columns or not.
}
  \item{check.names}{
    Single logical value : should column names in the output be made syntactically valid by \code{\link{make.names}} or not.
}
  \item{encode}{
    Single logical value : should character values be translated from UTF-8 charset (native charset for ODB files) to the locale one or not.
}
  \item{autoLogical}{
    Single logical value : should logical-like columns be converted to logical or not. Notice this is a workaround, the conversion is not properly done by JDBC and the logical column type is guessed from the values.
}
}

\value{
  Returns a data.frame, whose content depends on the SQL query executed.
}

\author{
  Sylvain Mareschal
}

\note{
  To query databases built with OpenOffice or LibreOffice, it may be necessary to quote table and/or column names in \code{sqlQuery}, as the default behavior of the HSQL engine is to convert unquoted table and column names to uppercases.
}

\seealso{
  \code{\link{odb.write}}
}

\examples{
  # New empty .odb file
  odbFile <- tempfile(fileext=".odb")
  odb.create(odbFile, overwrite="do")
  odb <- odb.open(odbFile)
  
  # New table
  SQL <- "CREATE TABLE fruits (
    name VARCHAR(6) PRIMARY KEY,
    color VARCHAR(32)
  )"
  odb.write(odb, SQL)
  
  # Data insertion
  dat <- data.frame(
    c("banana", "pear", "peach"),
    c("yellow", "yellow", "purple")
  )
  odb.insert(odb, "fruits", dat)
  
  # Read content
  print(odb.read(odb, "SELECT * FROM fruits"))
  
  # Writes to the file and closes the connection
  odb.close(odb, write=TRUE)
}

