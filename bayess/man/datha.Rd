\name{datha}
\alias{datha}
\docType{data}
\title{
Non-standardised Licence dataset%%   ~~ data name/kind ... ~~
}
\description{
The dataset used in Chapter 6 is derived from an image of a license plate, called \code{license}
and not provided in the package.  The actual histogram of the grey levels is
concentrated on 256 values because of the poor resolution of the image, but
we transformed the original data as \code{datha.txt}.
}
\usage{data(datha)}
\format{
  A data frame with 2625 observations on the following variable.
  \describe{
    \item{\code{x}}{Grey levels}
  }
}
\details{
\code{datha.txt} was produced by the following \R code:
\preformatted{
> license=jitter(license,10)
> datha=log((license-min(license)+.01)/
+ (max(license)+.01-license))
> write.table(datha,"datha.txt",row.names=FALSE,col.names=FALSE)
}
where \code{jitter} is used to randomize the dataset and avoid repetitions
}
\examples{
data(datha)
datha=as.matrix(datha)
range(datha)
}
\keyword{datasets}
\keyword{grey levels}
\keyword{license plate}
