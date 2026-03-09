\name{suitabinput}
\alias{suitabinput}
\docType{data}
\title{
Sample grid stack map of nutrient indicators for crop fertility requirements
}
\description{
A grid stack map of nine variables for assessing crop suitabilities
}
\usage{data("suitabinput")}
\format{
  The format is:
Formal class 'SpatialGridDataFrame' [package "sp"] with 4 slots
  ..@ data       :'data.frame':	16900 obs. of  12 variables:
  .. ..$ cac03     : num [1:16900] 21.8 20.6 21.2 22 22.3 ...
  .. ..$ ec        : num [1:16900] 2.5 2.38 2.15 2.36 2.24 ...
  .. ..$ depthcodes: num [1:16900] 3 1 3 3 3 3 3 3 1 1 ...
  .. ..$ rain      : num [1:16900] 282 279 260 279 276 ...
  .. ..$ texture   : int [1:16900] 5 5 5 5 5 5 5 5 11 11 ...
  .. ..$ dem       : num [1:16900] 489 489 489 485 487 ...
  .. ..$ drainage  : int [1:16900] 2 5 2 2 2 5 7 5 5 5 ...
  .. ..$ stones    : num [1:16900] 6 9 6 6 6 6 6 9 9 9 ...
  .. ..$ structure : int [1:16900] 3 8 7 5 5 5 7 5 9 9 ...
  .. ..$ ph        : num [1:16900] 8.76 8.83 8.73 8.71 8.69 ...
  .. ..$ slope     : num [1:16900] 0.969 0.969 0.969 0.969 0.969 ...
  .. ..$ texture1  : Factor w/ 3 levels "Lo","SaLo","SiLo": 2 2  ...
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
data(suitabinput)
summary(suitabinput$depthcodes)
hist(suitabinput$dem)
}
\keyword{datasets}
