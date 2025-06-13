\name{odb.create}
\alias{odb.create}
\title{
  Creates a .odb empty file.
}

\description{
  Creates an empty HSQL database embedded in a .odb file, copying a template.
}

\usage{
  odb.create(odbFile, template = NULL,
    overwrite = c("warning", "do", "skip", "stop"))
}

\arguments{
  \item{odbFile}{
    Path to the new file.
}
  \item{template}{
    Template to copy. You should not have to care about it, as the current version is included in the package (used if NULL).
}
  \item{overwrite}{
    Single character value, defining how to handle overwriting : 'warning' will overwrite and raise a warning if a file is replaced, 'stop' will raise an error before copying anything, 'do' will overwrite silently, 'skip' won't silently.
}
}

\value{
  Invisibly returns the single logical value returned by \code{\link{file.copy}}, if no critical error occurs before it is called.
}

\author{
  Sylvain Mareschal
}

\note{
  The default template was produced with LibreOffice Base 3.5.5.
  
  To use a different template once, just use the \code{template} argument of this function. Notice a non empty database can be used as template, it will be copied and only the copy will be updated.
  
  To update the default template, replace the "template.odb" file in the "tools" directory of the package with a new one. Usual templates are empty HSQL databases created with OpenOffice or LibreOffice, and saved as .odb files.
  
  Notice that newer templates may require a different version of the hsql.jar library in the \code{\link{odb.open}} call (an error will be raised if necessary). See \code{\link{odb.open}} for further details on it.
}

\examples{
  # New empty .odb file
  odbFile <- tempfile(fileext=".odb")
  odb.create(odbFile, overwrite="do")
  odb <- odb.open(odbFile)
  
  # Empty template
  print(odb.tables(odb))
  
  # New table
  odb.write(odb, "CREATE TABLE fruits (name VARCHAR(6) PRIMARY KEY)")
  odb.insert(odb, "fruits", c("banana", "pear", "peach"))
  
  # Writes to the file and closes the connection
  odb.close(odb, write=TRUE)
  
  
  # Use as template
  odbFile2 <- tempfile(fileext=".odb")
  odb.create(odbFile2, template=odbFile, overwrite="do")
  odb <- odb.open(odbFile2)
  print(odb.tables(odb))
  odb.close(odb, write=TRUE)
}
