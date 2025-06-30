\name{TSwriteXLS}
\alias{TSwriteXLS}
\alias{TSwriteCSV}

\title{Write Time Series to an .xls Spreadsheet File}
\description{Write one or more time series to a .xls spreadsheet file}

\usage{
    TSwriteXLS(x, ..., FileName="R.xls", SheetNames=NULL,
               dateHeader="date", verbose = FALSE)
    TSwriteCSV(x, FileName="R.csv",  dateFormat=1, dateHeader="date")
}

\arguments{
    \item{x}{a time series or matrix of time series.)}
    \item{...}{additional time series objects (like x).}
    \item{FileName}{a string to use for the file name.}
    \item{SheetNames}{Logical indicating if NA should be removed from the beginning
    and end of a series.}
    \item{dateFormat}{an integer indicating the format to use for dates. (See details.)}
    \item{dateHeader}{string or vector of strings to use as header on date column.}
    \item{verbose}{logical passed to WriteXLS.}
}
\value{logical}
\details{
  This functions uses \code{WriteXLS} to write time series data to an .xls spreadsheet
  file. If the object \code{x} has mulitple series then they will be put on
  the same sheet. Additional objects in \dots will be put on additional pages.

  The first line of each sheet will have column headers using \code{seriesNames}
  from the series in the column. The first column will have dates and the 
  heading  for that column will be determined by \code{dateHeader}. This can be a 
  vector of length equal the number of sheets, of a single character string, in 
  which case it is replicated for each sheet. The \code{dateHeader} is a 
  might be useful for indication information like the date of the last 
  observation in a known place on the sheet (A1) for indicating on a graph.
  For convenience, the year, period, and a character 
  representation of the period are indicated in columns 2 to 4. This is followed 
  by columns of the data.
  
  If \code{SheetNames} is \code{NULL} (the default) then the sheet names will 
  be generated from the first series name in each object. If \code{SheetNames}
  is supplied then it should have an element for \code{x} and for each object 
  in \dots.
  
  If \code{WriteXLS} does not find a version of perl with appropriate modules
  then a work around is attempted using \code{save}.
  
  \code{WriteCSV} handles only a single object \code{x} which can have mulitple 
  series. These are wtten as columns in the comma separated value file. 
  \code{dateFormat} can be  0 indicating no date, 1 indicating a format like
  "Jan 1969",   2 indicating a format like  1969,1, or   3 indicating a 
  format like 1969,"Jan",. Only 0 or 1 are supported for data other than monthly 
  and quarterly frequencies.
  }
\seealso{
    \code{\link[WriteXLS]{WriteXLS}}
}
\examples{
 tofile <- tempfile()
 
 z <- ts(1:10, start=c(1999,2), freq=12)
 seriesNames(z) <- "ser 1"
 TSwriteXLS(z, FileName=tofile) 

 zz <- tbind(z, diff(z))
 seriesNames(zz) <- c("ser 1", "diff")
 TSwriteXLS(zz, FileName=tofile,  SheetNames="2 series")

 zz <- ts(1:10, start=c(1999,1), freq=1)
 seriesNames(zz) <- "annual ser"
 TSwriteXLS(z, zz, FileName=tofile,  SheetNames=c("monthly", "annual"))

 unlink(tofile) 
}
\keyword{utilities}
\keyword{ts}
