\name{ODB-package}
\alias{ODB-package}
\alias{ODB}
\docType{package}
\title{
  \packageTitle{ODB}
}

\description{
  \packageDescription{ODB}
}

\details{
  The DESCRIPTION file:
  \packageDESCRIPTION{ODB}
  \packageIndices{ODB}
  
  First notice this package is not intented to manage all .odb variations. Currently two distinct types of .odb files can be distinguished : files embedding an HSQL database, and files connecting to a remote database engine such as MySQL. This package is made for the formers, as remote databases can be queried directly using the DBI package. Functions involving the XML content of the .odb file (\code{\link{odb.comments}} and \code{\link{odb.queries}}) may be operative on such files, but there is no garantee they would be.
  
  You should not consider this package without minimal knowledge of the SQL language, as only a few basic operations have functions not relying on the user to build a valid SQL query (\code{\link{odb.insert}}, \code{\link{odb.tables}}, \code{\link{odb.comments}}, \code{\link{odb.queries}}).
  
  This package is built around the \code{odb} class, which extends \code{DBIConnection}. A set of functions is provided to execute reading (SELECT ...) or writing (INSERT, CREATE, DROP ...) SQL queries through the HSQLDB engine, and a set of convenient tools is also provided for the most common tasks (\code{\link{odb.read}}, \code{\link{odb.write}}, \code{\link{odb.insert}}). A few Open Document specific functions are also provided, allowing modifications on features provided by the .odb file format and not the database itself (\code{\link{odb.comments}} and \code{\link{odb.queries}}).
  
  Effects of all these functions can be achieved manually via the OpenOffice or LibreOffice Base software, obviously this package offers a way of automating it on a bigger scale. Both manual and automatic modifications can be done on the same .odb file, but users are strongly recommended to not use the two methods simultaneously. This R package works on a copy of the file made at the \code{\link{odb.open}} call, and this copy will replace the original file at the \code{\link{odb.close}} call, regardless of what happened between the two calls.
  
  Additional tools are also provided, notably \code{\link{odb.export}} and \code{\link{isClosed}}.
  
  See the 'examples' section for more details on the package behavior.
}

\author{
  \packageAuthor{ODB}

  Maintainer: \packageMaintainer{ODB}
}

\keyword{ package }

\examples{

  ## CONNECTION ##
  
  # Creation of a new .odb database
  odbFile <- tempfile(fileext=".odb")
  odb.create(odbFile, overwrite="do")

  # Connection to the file
  odb <- odb.open(odbFile)
  
  
  ## STRUCTURE ##
  
  # Creation of a new table
  SQL <- "CREATE TABLE fruits (id INTEGER PRIMARY KEY, value VARCHAR(8))"
  odb.write(odb, SQL)
  
  # Addition of comments on columns
  odb.comments(odb, "fruits", c("id", "value")) <- c("Fruit rank", "Fruit name")
  
  # Printing of the database structure
  print(odb.tables(odb))
  
  # Printing of a specific comment
  print(odb.comments(odb, "fruits", "value"))
  
  
  ## DATA ##
  
  # Insertion of data
  data <- data.frame(id=1:3, value=c("apple", "orange", "pear"))
  odb.insert(odb, tableName="fruits", data=data, execute=TRUE)
  
  # Reading
  print(odb.read(odb, "SELECT * FROM fruits WHERE id=2"))
  
  # Saved query
  odb.queries(odb, "2nd record") <- "SELECT * FROM fruits WHERE id=2"
  print(odb.read(odb, odb.queries(odb, "2nd record")))
  
  # SQL export
  sqlFile <- tempfile(fileext=".sql")
  odb.export(odb, sqlFile)
  
  
  ## DISCONNECTION ##
  
  # Is the connection closed ?
  print(isClosed(odb))
  
  # Save changes in the .odb file
  odb.close(odb, write=TRUE)
  
  # And now ?
  print(isClosed(odb))
}

