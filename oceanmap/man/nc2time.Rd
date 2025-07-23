\name{nc2time}
\alias{nc2time}
\title{
reads and converts the time variable of a netcdf-file ('.nc'-file) or \code{ncdf4}-object as \code{as.Date}-object
}
\description{
reads and converts the time variable of a netcdf-file ('.nc'-file) or \code{ncdf4}-object as \code{as.Date}-object.
}
\usage{
nc2time(nc,varname)
}
\arguments{
\item{nc}{character string indicating the filepath to a netcdf-file ('.nc'-file), or a \code{ncdf4}-object.
}
\item{varname}{character string indicating the name of the time vaiable of the netcdf-file.
}
}

\author{
Robert K. Bauer
}


\examples{
path <- system.file("test_files", package="oceanmap")
nfile <- Sys.glob(paste0(path,'/herring*.nc')) # load sample-'.nc'-files
head(nc2time(nfile))

library('ncdf4')
nc <- nc_open(nfile)
head(nc2time(nc))

}