\name{get.cb.pos}
\alias{get.cb.pos}
\title{
Returns additional colorbar placement information}
\description{
Returns colorbar ticks length, label-, cb.title- and cb.xlab-position. ticks.lab, cb.title and cb.lab are horizontally centered, so only the y-value is returned.
}
\usage{
get.cb.pos(cbx,cby,oticks)
}
\arguments{
  \item{cbx}{
 the horizontal limits (x1, x2) of the colorbar.
}
  \item{cby}{
 the vertical limits (y1, y2) of the colorbar.
}
 \item{oticks}{
  letter defining alignment of colorbar ticks and labels. Allowed values are: \code{'b'} for bottom, \code{'l'} for left and \code{'r'} for right alignment. 
}
}              

\seealso{ \code{ \link{set.colorbar}, \link{cust.colorbar}, \link{region_definitions}}} 


\author{
Robert K. Bauer
}

\keyword{internal}

\examples{
path <- system.file("test_files", package="oceanmap")
gz.files <- Sys.glob(paste0(path,'/*.gz')) # load sample-'.gz'-files

obj <- readbin(gz.files[2],area='lion')
ncorse <- crop(obj,extent(6,9,40,42))
cbx <-c(8.3,8.9)
cby <- c(40.7,40.8)
v(ncorse,zlim=c(20,30),cbx=cbx,cby=cby) # skipping colorbar widget

get.cb.pos(cbx=cbx,cby=cby,oticks="b") 

#dev.new()
#image(obj)
#align <- set.colorbar()
#get.cb.pos(cbx=align$cbx,cby=align$cby,oticks=align$oticks)
}