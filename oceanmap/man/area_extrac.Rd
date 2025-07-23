\name{area_extrac}
\alias{area_extrac}
\title{
Extracts a pre-defined region from \code{'.gz'}-file and saves subset as a new \code{'.gz'}-file}
\description{
Extracts a pre-defined region from \code{'.gz'}-file and saves subset as a new \code{'.gz'}-file (gzip compressed format). Basically it represents a combined call of \link{regions}, \link[raster]{crop}, \link{raster2matrix}  and \link{writebin}.
}

\usage{
area_extrac(obj,area)
}
\arguments{
 \item{obj}{
Character string indicating search criteria for \code{'.gz'}-files. 
}
  \item{area}{
Character string identifying the region that should be extracted. \code{area} must be a subregion of the original region defined by the \code{'.gz'}-file.
See \link{region_definitions} for area definitions and use \link{add.region} to add new regions.
}

}
\seealso{\link{readbin}, \link{writebin}, \link[raster]{crop}, \link{raster2matrix}, \link{param_unconvert}} 

\examples{
## Example 1: extract, write '.gz'-files, following default plot-procedure
library(raster)

# load sample-'.gz'-file
path <- system.file("test_files", package="oceanmap")
gz.files <- Sys.glob(paste0(path,'/*.gz'))[1] # load sample-'.gz'-files
print(gz.files)
# area_extrac(gz.files[1],area='lion')

# gz <- Sys.glob(path,'/medw4*.gz') # load new-'.gz'-file
# v(gz) # visualize new-'.gz'-file
# system(paste('rm', gz))       
# v(gz.files[1],v_area='lion')
}

\author{
Robert K. Bauer
}

\keyword{region_definitions}

