\name{tsScan}
\alias{tsScan}
\alias{tsWrite}
\title{Read and Write Time Series to Files}
\description{Read and write time series to files.}
\usage{
   tsScan(file="", skip=1, nseries=1, sep=",", 
       na.strings=c("NA", "NC", "ND"), ...)

   tsWrite(x, file="data", header=TRUE, sep=",", digits=16)

}
\arguments{
    \item{file}{name of file to read or write.}
    \item{x}{A time series or time series matrix.}
    \item{skip}{number of lines to skip at start of file before reading data.}
    \item{nseries}{number of columns of series to expect.}
    \item{sep}{field separator.}
    \item{na.strings}{charaters that should be treated as NA.}
    \item{header}{a logical indicating is a header line should be written.}
    \item{digits}{number of significant digits to print.}
    \item{...}{additional arguments passed to scan.}
}
\value{A time series vector or matrix.}
\details{
  Read and write a file with time series data. By default the  file is comma 
  separated values (csv) with one header line (the series names on write, 
  ignored on read). The year and period are the first two columns, with series
  in following columns. 
  These are wrappers for \code{scan} and \code{write}.
  
  Beware that short digits settings will result in truncated data.
}
\seealso{
    \code{\link{scan}, \link{write}}
}
\examples{
tmpfile <- tempfile()
on.exit(unlink(tmpfile) )
z <- ts(matrix(100 + rnorm(200),100,2), start=c(1991,1), frequency=4)
tsWrite(z, file=tmpfile)
zz <- tsScan(tmpfile, nseries=2)

max(abs(z - zz)) 
}
\keyword{ts}
