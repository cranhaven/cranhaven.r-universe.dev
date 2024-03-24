\name{matrix}
\alias{matrix}
\docType{data}
\title{Matrix of data
%%   ~~ data name/kind ... ~~
}
\description{Matrix of size 200x200 containing Gaussian data satisfying model D with change-points 40,80,120,160.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(matrix)}
\format{
  The format is:
 num [1:200, 1:200] 6.77 7.69 8.44 8.95 6.81 ...
 - attr(*, "dimnames")=List of 2
  ..$ : NULL
  ..$ : chr [1:200] "V1" "V2" "V3" "V4" ...
}
\examples{
library(HiCseg)
data(matrix)
}
\keyword{datasets}
