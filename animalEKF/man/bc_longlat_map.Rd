\name{bc_longlat_map}
\alias{bc_longlat_map}
\docType{data}
\title{
Image of Bolsa Chica for use with \code{\link{shark_vis_longlat}}
}
\description{
Image of Bolsa Chica for use with \code{\link{shark_vis_longlat}}
}
\usage{data(bc_longlat_map)}
\format{
  The format is:
List of 8
 $ lat.center: Named num 33.7
  ..- attr(*, "names")= chr "lat"
 $ lon.center: Named num -118
  ..- attr(*, "names")= chr "lon"
 $ zoom      : num 15
 $ myTile    : num [1:640, 1:640, 1:4] 0.639 0.639 0.639 0.639 0.639 ...
 $ BBOX      :List of 2
  ..$ ll: num [1, 1:2] 33.7 -118.1
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : NULL
  .. .. ..$ : chr [1:2] "lat" "lon"
  ..$ ur: num [1, 1:2] 33.7 -118
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : NULL
  .. .. ..$ : chr [1:2] "lat" "lon"
 $ url       : chr "google"
 $ size      : num [1:2] 640 640
 $ SCALE     : num 1
 - attr(*, "class")= chr "staticMap"
}
\source{
Google Maps.
}
\keyword{datasets}
