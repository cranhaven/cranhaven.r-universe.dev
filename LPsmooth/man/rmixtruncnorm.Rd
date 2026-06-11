\name{rmixtruncnorm}
\alias{rmixtruncnorm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Random numbers generator for truncated normal mixtures
}
\description{
Generates random samples from a mixture of truncated normals.

}
\usage{
rmixtruncnorm(n, pis, means, sds, range)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
  Size of the random sample.}

  \item{pis}{
  A vector collecting the mixture weights. See details.}
  \item{means}{
  A vector collecting the means of the mixture components.
}
  \item{sds}{
  A vector collecting the standard deviations of the mixture components.
}
  \item{range}{
  Interval corresponding to the support of each of  truncated normal contributing to the mixture. See details.}
}
\details{
The argument \code{pis} is a vector with its length equal the number of components in the mixture. The vector \code{pis} must sum up to one, e.g. \code{c(0.7, 0.2, 0.1)}.
The argument \code{range} is an interval corresponding to the support of each  truncated normal contributing to the mixture.
}
\value{
A vector collecting the random sample of size \code{n} from the mixture of truncated specified.
}
\author{
Sara Algeri and Xiangyu Zhang
}

\seealso{
\code{\link{dmixtruncnorm}},\code{\link{dmixnegbinom}}, \code{\link{rmixnegbinom}}, \code{\link{find_h_cont}}.
}
\examples{
rmixtruncnorm(n=10,pis=c(0.5,0.5),means=c(3,6),sds=c(3,4),range=c(0,30))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Mixture of truncated normals sampler}% use one of  RShowDoc("KEYWORDS")
