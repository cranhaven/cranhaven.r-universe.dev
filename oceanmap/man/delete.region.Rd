\name{delete.region}
\alias{delete.region}
\title{
deletes a region from the \link{region_definitions}-definition file}
\description{
deletes a specified region from the \link{region_definitions}-definition file}
\usage{
delete.region(region,lib.folder,restore=F)
}
\arguments{
  \item{region}{
Character string identifying the region that should be deleted.
See \link{region_definitions} for area definitions and use \link{add.region} to add new regions.
}
  \item{lib.folder}{
Character string indicating R-library path in which the oceanmap-package is installed.
}
  \item{restore}{
whether the original \link{region_definitions}-file should be restored.
}

}
\seealso{\link{add.region}, \link{region_definitions}, \link{regions}, \link{writebin}} 

\author{
Robert K. Bauer
}

\keyword{region_definitions}

\examples{
## Example 1: Add region by supplying a one-row data.frame 
##            that holds the entire required information
data(region_definitions)
lion <- region_definitions[region_definitions$label == 'lion',] # selecting Gulf of Lions region
lion
junk <- lion 
junk$label <- 'junk' # rename region label
#add.region(junk) # add junk region
data(region_definitions) # reload region_definitions
region_definitions[,1:9]

## Example 2: Delete region
#delete.region("junk") # delete junk region
data(region_definitions) # reload region_definitions
region_definitions[,1:9]
}
