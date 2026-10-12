\name{binned_zmat}
\alias{binned_zmat}
\alias{bz}
\alias{pbz}
\docType{data}
\title{
Three GWAS studies - input objects to main function 
}
\description{
This data was created from the \code{zmat} matrix (see \code{\link{SNPlocations}})  using \code{\link{ztobins}} function. It contain two objects to be input to the main function \code{repfdr}.
}
\format{
The file includes two objects - a matrix and 3d array:

\code{bz} is a matrix of binned 249024 z-scores (in rows) in each of the 3 studies (columns).

\code{pbz} is a 3-dimensional array which contains for each study (first dimension), the probabilities of a z-score to fall in the bin (second dimension), under each hypothesis status (third dimension).
}
\examples{
\dontrun{
download.file('http://www.math.tau.ac.il/~ruheller/repfdr_RData/binned_zmat.RData',
  destfile = "binned_zmat.RData")
load(file = "binned_zmat.RData")

bz[1:5,]
pbz[,1:5,]
}
}
\keyword{datasets}
\keyword{Replicability}
\keyword{GWAS}
