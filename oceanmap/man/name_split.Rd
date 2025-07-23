\name{name_split}
\alias{name_split}
\alias{get.gz.info}

\title{
Returns a summary data frame of \code{'.gz'} encoded oceanography files by splitting their name
}

\description{
Returns a summary \link{data.frame} of \code{'.gz'} encoded oceanography files by splitting their name
}
\usage{
name_split(gz.files)
get.gz.info(gz.files)
}

\value{
Returns a summary \link{data.frame} of \code{'.gz'} encoded oceanography files by splitting their name

\code{area source parameter resolution timestep date1 date2 option}

\item{\code{area}}{
region keyword
}

\item{\code{source}}{
data source
}

\item{\code{param}}{
the parameter saved in the \code{'.gz'}-file. Can only be one value!
}

\item{\code{resolution}}{
the spatial resolution
}

\item{\code{timestep}}{
the temporal resolution
}

\item{\code{date1} & \code{date2}}{
the time interval covered in date format
}


\item{\code{option}}{
a character string holding supplmentary information of \code{'.gz'}-file treatment
}
}


\arguments{
  \item{gz.files}{
Optional character vector or search criteria for .gz-encoded oceanography files. 
}
}
\seealso{See \link{check_gzfiles} to return summary of available \code{'.gz'}-files and \link{name_join} to create \code{'.gz'}-filenames from splitted names (\link{name_split})-calls} 


\author{
Robert K. Bauer
}

\examples{
## Example: read and plot '.gz'-file
path <- system.file("test_files", package="oceanmap")
gz.files <- Sys.glob(paste0(path,'/*.gz')) # load sample-'.gz'-files
check_gzfiles(folder=path) # return file summary-table

# return summary of availble '.gz'-files
# suffix-column corresponds to option column of the name_split-call
# addition n-column returns the number of available files per filetype
check_gzfiles(gz.files)

## Example: split and rejoin '.gz'-filenames
gz.files
name_split(gz.files) # return summary-table per file
name_split() # return summary-table of all gz-file in current folder
name_join(name_split(gz.files))
}
