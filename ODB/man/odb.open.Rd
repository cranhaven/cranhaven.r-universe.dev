\name{odb.open}
\alias{odb.open}
\title{
Creates a connection to a .odb file
}

\description{
  Extracts embedded HSQLDB files from an .odb archive an sets a DBI connection to it, in order to read or write data from the database.
}

\usage{
  odb.open(odbFile, jarFile = NULL)
}

\arguments{
  \item{odbFile}{
    Path to the .odb file to connect to.
}
  \item{jarFile}{
    Path to the .jar library of HSQLDB to use in JDBC. You should not have to care about it, as the current version is included in the package (used if NULL).
}
}

\value{
  An object of class "ODB", which will be used by every other functions of the package.
}

\author{
  Sylvain Mareschal
}

\note{
  The default hsql.jar library version is 1.8.0.10.
  
  Alternate versions may be required when using .odb files not produced by \code{\link{odb.create}}, as they can embed various versions of HSQLDB. Attempting to open an .odb file with the wrong hsql.jar library version will raise an error, it is then up to the user to download the correct hsql.jar file and provide it to \code{odb.open} via the \code{jarFile} argument.
  
  HyperSQL .jar libraries can be downloaded freely from \url{http://www.hsqldb.org}.
  
  The default "hsql.jar" file can be replaced in the "tools" directory of the package if a different version is frequently required.
}

\seealso{
  \code{\link{odb.close}}, \code{\link{odb.create}}
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
