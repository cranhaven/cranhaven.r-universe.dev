\name{raster2matrix}
\alias{raster2matrix}
\alias{raster2array}
\title{
Convert Raster layer to a matrix or array}
\description{
\code{raster2matrix} converts a raster layer to a matrix or array. Used by \link{readbin} and \link{writebin}.
}
\usage{
raster2matrix(RasterLayer)

raster2array(RasterLayer)
}
\arguments{
  \item{RasterLayer}{
raster layer to be converted.
}
}

\author{
Robert K. Bauer
}


\examples{
library('raster')
path <- system.file("test_files", package="oceanmap")
gz.files <- Sys.glob(paste0(path,'/*.gz')) # load sample-'.gz'-files
check_gzfiles(folder=path) # return file summary-table

raster.file <- readbin(gz.files[1]) # loading gz-file as raster-layer
image(raster.file)

## Example 1: converting single raster layer to matrix
image(as.matrix(raster.file)) # unflipped conversion
m <- raster2matrix(raster.file) # converting raster-layer to matrix
image(m)

## Example 2: converting double raster layer to an array
stack.file <- stack(raster.file,raster.file)
image(as.array(stack.file)[,,1]) # unflipped conversion
a <- raster2array(stack.file) # converting raster-layer to array (works also with raster2matrix)
image(a[,,1])
}