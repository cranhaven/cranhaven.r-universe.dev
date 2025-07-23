\name{name_join}
\alias{name_join}
\title{
create '\code{'.gz'}-filenames from a list or dataframe
}


\description{
creates filenames based on a list or dataframe with the (header)-names:

\code{area source parameter resolution timestep date1 date2 option}

by aligning the defined filetype: 

e.g. \code{area_source_parameter_resolution_timestep_date1_date2.option.filetype}
}


\usage{
name_join(parts,filetype='gz')
}

\arguments{

\item{parts}{
a list or dataframe with the parts:
}

\describe{
\item{\code{area}}{
, the region keyword
}

\item{\code{source}}{
, the data source
}

\item{\code{param}}{
, the parameter saved in the \code{'.gz'}-file. Can only be one value!
}

\item{\code{resolution}}{
, the  spatial resolution
}

\item{\code{timestep}}{
, the temporal resolution
}

\item{\code{date1} & \code{date2}}{
, the temporal resolution (the time interval covered).
}


\item{\code{option}}{
a character string holding supplmentary information of \code{'.gz'}-file treatment
}
}

\item{filetype}{
character string inidicating the filtype to be checked. ('.gz' by default)}

}

\seealso{See \link{check_gzfiles} to return summary of available \code{'.gz'}-files and \link{name_split} to split \code{'.gz'}-filenames
}


\author{
Herve Demarq, translated from IDL by Robert K. Bauer
}

\examples{
## Example: read and plot '.gz'-file
path <- system.file("test_files", package="oceanmap")
gz.files <- Sys.glob(paste0(path,'/*.gz')) # load sample-'.gz'-files
check_gzfiles(folder=path) # return file summary-table

# return summary of availble '.gz'-files
# suffix-column corresponds to option column of the name_join-call
# addition n-column returns the number of available files per filetype
check_gzfiles(gz.files)

## Example: split and rejoin '.gz'-filenames
name_split(gz.files) # return summary-table per file
name_join(name_split(gz.files))
}
