\name{odb.comments}
\alias{odb.comments}
\alias{odb.comments<-}
\title{
  Gets or sets column comments in an ODB database
}

\description{
  This function allows comment manipulation into OpenOffice Base databases, through an \code{odb} connection.
}

\usage{
  odb.comments(odb, tableNames = NULL, columnNames = NULL, simplify = TRUE)
  odb.comments(odb, tableNames, columnNames) <- value
}

\arguments{
  \item{odb}{
    An \code{\linkS4class{ODB}} object, as produced by \code{\link{odb.open}}.
}
  \item{tableNames}{
    Character vector naming tables to search comments in. See the 'Details' section.
}
  \item{columnNames}{
    Character vector naming columns to search comments in. See the 'Details' section.
}
  \item{simplify}{
    Single logical value, whether to simplify returns. See the 'Value' section
}
  \item{value}{
    Character vector, the comments to store.
}
}

\details{
  \code{tableNames}, \code{columnNames} and \code{value} can be considered as columns from a same data.frame: the first comment considered will be the comment on the first \code{columnNames} for the first \code{tableNames}, and so on. Vectors are repeated to achieve same lengths if necessary.

  The \code{NULL} value can be used for \code{tableNames} and \code{columnNames} in the first syntax, with the meaning of "all possible values". See the 'Examples' section.
}

\value{
  The first syntax returns a list of character vectors. Names in the list are table names, names in vectors are column names. Only tables and columns with comments are present in the results.
  
  With \code{simplify} set to TRUE, a character vector is directly returned (without embedding list) when a single \code{tableNames} is interrogated, and column names are skipped if a single \code{tableNames} / \code{columnNames} couple is provided.
}

\author{
  Sylvain Mareschal
}

\note{
  Comments on non-existing tables or columns can be manipulated as well, as there is no link between the comment storage engine and the database itself. Keep in mind these comments are stored in the .odb file, not the database itself.
  
  Comments can also be added manually from OpenOffice while creating a table.
}

\examples{
  # New empty .odb file
  odbFile <- tempfile(fileext=".odb")
  odb.create(odbFile, overwrite="do")
  ODB <- odb.open(odbFile)
  
  # New tables
  SQL <- c(
    "CREATE TABLE fruits (
      name VARCHAR(6) PRIMARY KEY,
      color VARCHAR(32)
      )",
    "CREATE TABLE vegetables (
      name VARCHAR(6) PRIMARY KEY,
      color VARCHAR(32)
      )"
    )
  odb.write(ODB, SQL)
  
  # Setting a single comment
  odb.comments(ODB,
    tableNames = "fruits",
    columnNames = "name"
  ) <- "Fruit names"
  print(odb.comments(ODB))
  
  # Setting two comments in the same table
  odb.comments(ODB,
    tableNames = "fruits",
    columnNames = c("name", "color")
  ) <- "Fruit columns"
  print(odb.comments(ODB))
  
  # Setting two distinct comments
  odb.comments(ODB,
    tableNames = c("fruits", "vegetables"),
    columnNames = c("name", "color")
  ) <- c("Fruit names", "Vegetable colors")
  print(odb.comments(ODB))
  
  # Writes to the file and closes the connection
  odb.close(ODB, write=TRUE)
}
