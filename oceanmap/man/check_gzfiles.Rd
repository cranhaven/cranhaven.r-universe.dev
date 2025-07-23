\name{check_gzfiles}
\alias{check_gzfiles}
\title{
Returns summary on \code{'.gz'}-file types}
\description{
Returns summary table on \code{'.gz'}-file types available in a specified folder. Provided information include \code{region} (region covered, as described by the \link{region_definitions}), \code{sat} (satellite source), \code{param} (parameter), \code{res} (spatial resolution), \code{ts} (temporal resolution), \code{filetype} (file filetype)}
\usage{
check_gzfiles(sstring="*",folder,filetype=".gz")
}
\arguments{
  \item{sstring}{
Character string indicating the search criteria for sat files (default is \code{*}, including all \code{'.gz'}-files). 
}
  \item{folder}{
Character string indicating the folder in which searched files are located (default is current working directory)
}
  \item{filetype}{
Character string indicating thefile type of sat files  (default is \code{.gz})
}
}
\value{
An aggregated data frame, returning \code{'.gz'}-file type-information (see description) on available files in a specified folder.
}
\seealso{\link{name_split}, \link{check_ts}} 


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
