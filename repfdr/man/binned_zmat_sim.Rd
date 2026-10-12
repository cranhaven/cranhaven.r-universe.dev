\name{binned_zmat_sim}
\alias{binned_zmat_sim}
\alias{bz_sim}
\alias{pbz_sim}
\docType{data}
\title{
Simulated data set - input objects to main function
}
\description{
This data was created from the \code{\link{zmat_sim}} matrix  using \code{\link{ztobins}} function. It contain two objects to be input to the main function \code{repfdr}.
}
\usage{data(binned_zmat_sim)}
\format{
The file includes two objects - a matrix and 3d array:

\code{bz_sim} is a matrix of binned 10000 z-scores (in rows) in each of the 3 studies (columns).

\code{pbz_sim} is a 3-dimensional array which contains for each study (first dimension), the probabilities of a z-score to fall in the bin (second dimension), under each hypothesis status (third dimension).
}
\examples{
data(binned_zmat_sim)
bz_sim[1:5,]
pbz_sim[,1:5,]
}
\keyword{datasets}
\keyword{Replicability}
\keyword{GWAS}
