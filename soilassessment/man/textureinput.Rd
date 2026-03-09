\name{textureinput}
\alias{textureinput}
\docType{data}
\title{
Sample texture dataset for mapping soil texture
}
\description{
Sample dataset for assessing soil texture
}
\usage{data("textureinput")}
\format{
  The format is:
Formal class 'SpatialGridDataFrame' [package "sp"] with 4 slots
  ..@ data       :'data.frame':	16900 obs. of  3 variables:
  .. ..$ sand: num [1:16900] 61.5 59.8 60.6 58.2 59.1 ...
  .. ..$ clay: num [1:16900] 12.6 13.9 14.1 13.8 13.8 ...
  .. ..$ silt: num [1:16900] 25 26.9 25.3 28 26.9 ...
  ..@ grid       :Formal class 'GridTopology' [package "sp"] with 3 slots
  .. .. ..@ cellcentre.offset: Named num [1:2] 383216 3341506
  .. .. .. ..- attr(*, "names")= chr [1:2] "x" "y"
  .. .. ..@ cellsize         : num [1:2] 357 357
  .. .. ..@ cells.dim        : int [1:2] 130 130
  ..@ bbox       : num [1:2, 1:2] 383038 3341327 429478 3387767
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : chr [1:2] "x" "y"
  .. .. ..$ : chr [1:2] "min" "max"
  ..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slot
  .. .. ..@ projargs: chr "+proj=utm +zone=41 +datum=WGS84 +units=m +no_defs"
}
\examples{
data(textureinput)
summary(textureinput)
}
\keyword{datasets}
