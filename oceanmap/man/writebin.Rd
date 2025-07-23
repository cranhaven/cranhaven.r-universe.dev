\name{writebin}
\alias{writebin}
\title{
Saves geographic data as byte file (\code{'.gz'})}
\description{
Saves geographic data as byte file, in gzip compressed format (\code{'.gz'}). \bold{ATTENTION!}! Only 2D (one layer) can be stored!
}
\usage{
writebin(satdata,filename,folder,param)
}
\arguments{
\item{satdata}{
  one layer-\link[raster]{raster}-object or \link{matrix} holding spatial data.
}
  \item{param}{
character string indicating the parameter name for the dataset treatment. See \code{parameter_definitions} for available parameters.
}
  \item{filename}{
character string naming the \code{'.gz'}-file to be created. 
}
  \item{folder}{
character string indicating the target directory. 
}
}
\seealso{\link{readbin}, \link{regions}, \link[raster]{crop}, \link{raster2matrix}, \link{param_unconvert}} 

\author{
Robert K. Bauer
}

\keyword{parameter_definitions}

\examples{
## Example for selecting wrong area definition when saving files
path <- system.file("test_files", package="oceanmap")
gz.files <- Sys.glob(paste0(path,'/*.gz')) # load sample-'.gz'-files
v(gz.files[1])

fname <- name_split(gz.files[1])
param <- fname$parameter
gz <- readbin(gz.files[1])
dim(gz)
v(gz.files[1])

### reset region name
fname$area <- 'med9'
fname <- name_join(fname)
# writebin(gz,fname,folder=path,param=param)
# v(fname)
# system(paste('rm', fname))

### multi layer raster file
gz2 <- stack(gz,gz)
# writebin(gz2,rep(gz.files[1],2),folder=path,param) # error message since multi layer
# writebin(gz,gz.files[1],folder=path,param) # single layer raster file
# v(gz.files[1])
}