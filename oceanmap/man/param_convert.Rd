\name{param_convert}
\alias{param_convert}
\alias{param_unconvert}
\title{
converts byte data to absolte values or vise versa (\code{param_unconvert})
}
\description{
converts byte data as stored in \code{'.gz'}-files to absolte values (\code{param_convert}) or vise versa (\code{param_unconvert}) using the parameter_definitions-dataset. \code{param_convert} is used by \link{readbin}, \code{param_unconvert} is used by \link{writebin}.
}
\usage{
param_convert(x,param)

param_unconvert(x,param)
}
\arguments{
  \item{x}{
vector, matrix or raster-object holding byte-data that that should be converted to absolute values (\code{param_convert}) or vise versa (\code{param_unconvert}). 
}
  \item{param}{
Character string indicating parameter of the dataset to be treated. See \code{parameter_definitions} for available parameters.
}
}
\seealso{ \link{param_unconvert}, \link{readbin}}


\author{
Robert K. Bauer
}

\keyword{parameter_definitions}

\examples{
library('fields')
path <- system.file("test_files", package="oceanmap")
gz.file <- Sys.glob(paste0(path,'/*.gz'))[1] # load sample-'.gz'-files
param <- name_split(gz.file)$parameter
print(param)

## converted data, according to param information
m <- readbin(gz.file, Raster=FALSE) 
image.plot(m)

## byte data ("unconverted") according to param information, as stored in ".gz"files
bin <- param_unconvert(m,param) 
image.plot(bin)

## reconverting byte data, according to param information
conv <- param_convert(bin,param) 
image.plot(conv)

}