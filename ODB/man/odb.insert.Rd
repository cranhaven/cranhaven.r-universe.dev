\name{odb.insert}
\alias{odb.insert}
\title{
  Wrapper for inserting data in an ODB table.
}

\description{
  Translates a data.frame into "INSERT INTO" SQL queries adaptated to a specific table from the database. Can execute the queries directly if asked to.
}

\usage{
  odb.insert(odb, tableName, data, execute = TRUE,
    dateFormat = "\%Y-\%m-\%d", ...)
}

\arguments{
  \item{odb}{
    An \code{\linkS4class{ODB}} object, as produced by \code{\link{odb.open}}.
}
  \item{tableName}{
    Single character value, naming the table from the database in which insert the data. No quoting is added so the HSQL engine will convert it to upper case, to refer to a case-specific table name you are required to double-quote this value.
}
  \item{data}{
    An object coercible to data.frame, containing the data to insert into the database. Column count and order must match those of the database table, R \code{NA} values will be considered as SQL \code{NULL} values.
}
  \item{execute}{
    Single logical value. If TRUE, the data will be inserted in the database, if FALSE the queries will be returned but not executed.
}
  \item{dateFormat}{
    Single character value, defining how dates in "data" are formated. See the "format" argument from \code{\link{strptime}} for more details.
}
  \item{\dots}{
    Further arguments for \code{\link{odb.write}}, if "execute" is set to TRUE.
}
}

\value{
  Returns a multiple character vector, with a distinct SQL query for each row of "data". If "execute" is set to TRUE, the return is invisible.
}

\author{
  Sylvain Mareschal
}

\seealso{
  \code{\link{odb.write}}
}

\examples{
  # New empty .odb file
  odbFile <- tempfile(fileext=".odb")
  odb.create(odbFile, overwrite="do")
  odb <- odb.open(odbFile)
  
  
  ## CASE-INSENSITIVE
  
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
  
  # Check content
  print(odb.read(odb, "SELECT * FROM fruits"))
  
  
  ## CASE-SENSITIVE
  
  # New table
  SQL <- "CREATE TABLE \"Fruits\" (
    name VARCHAR(6) PRIMARY KEY,
    color VARCHAR(32)
  )"
  odb.write(odb, SQL)
  
  # Data insertion
  dat <- data.frame(
    c("banana", "pear", "peach"),
    c("yellow", "yellow", "purple")
  )
  odb.insert(odb, "\"Fruits\"", dat)
  
  # Check content
  print(odb.read(odb, "SELECT * FROM \"Fruits\""))
  
  
  # Notice they are distinct tables
  print(odb.tables(odb))
  
  # Writes to the file and closes the connection
  odb.close(odb, write=TRUE)
}
