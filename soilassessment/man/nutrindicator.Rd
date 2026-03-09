\name{nutrindicator}
\alias{nutrindicator}
\docType{data}
\title{
A grid stack map of indicators for crop fertility requirements
}
\description{
A grid stack map of eleven variables for assessing soil fertility
}
\usage{data("nutrindicator")}
\format{
Formal class 'SpatialGridDataFrame' [package "sp"] with 4 slots
  ..@ data       :'data.frame':	16900 obs. of  11 variables:
  .. ..$ soc       : num [1:16900] 0.163 0.242 0.233 0.218 0.179 ...
  .. ..$ nitrogen  : num [1:16900] 0.0272 0.0242 0.0266 0.0275 0.0256 ...
  .. ..$ phosphorus: num [1:16900] 9.4 8.22 8.92 7.45 8.3 ...
  .. ..$ manganese : num [1:16900] 2.84 2.7 2.95 2.88 3.19 ...
  .. ..$ potassium : num [1:16900] 93.2 102.3 93.5 96.5 87.8 ...
  .. ..$ cec       : num [1:16900] 10.9 10.7 10 10.1 10.2 ...
  .. ..$ boron     : num [1:16900] 0.172 0.16 0.171 0.172 0.174 ...
  .. ..$ copper    : num [1:16900] 0.368 0.421 0.37 0.369 0.412 ...
  .. ..$ iron      : num [1:16900] 0.238 0.231 0.241 0.239 0.242 ...
  .. ..$ zinc      : num [1:16900] 0.816 0.652 0.816 0.818 0.814 ...
  .. ..$ sulfur    : num [1:16900] 153 131 119 135 163 ...
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
data(nutrindicator)
str(nutrindicator)
#spplot(nutrindicator["nitrogen"])
}
\keyword{datasets}
