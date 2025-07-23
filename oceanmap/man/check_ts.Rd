\name{check_ts}
\alias{check_ts}
\title{
checks if daily \code{'.gz'}-file time series is complete}
\description{
checks if daily \code{'.gz'}-file time series in the present working directory is complete.}
\usage{
check_ts(sstring="*.gz",folder,output=F)
}
\arguments{
  \item{sstring}{
Character string indicating search criteria for gz-files (default is \code{'*.gz'}). 
}
  \item{folder}{
Character string indicating the folder in which searched files are located (default is current working directory)
}
  \item{output}{
  weather the missing dates should be returned as vector (default is \code{F}).
}
}
\value{
optional vector of missing dates (see \code{output} argument).
}
\seealso{\link{name_split}, \link{check_gzfiles}} 


\author{
Robert K. Bauer
}

\examples{
## Example 1: plot '.gz'-files, following default plot-procedure
path <- system.file("test_files", package="oceanmap")
check_gzfiles(folder=path) # return file summary-table per filetype

## check for missing dates
check_ts('medw4*',folder=path)
check_ts('medw4*',folder=path,output=TRUE)
}
