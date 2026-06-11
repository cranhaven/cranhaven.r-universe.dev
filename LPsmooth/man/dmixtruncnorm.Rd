\name{dmixtruncnorm}
\alias{dmixtruncnorm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Probability density function of a mixture of truncated normals}
\description{
Computes the probability density function of a mixture of truncated normals.
}
\usage{
dmixtruncnorm(x, pis, means, sds, range)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  A scalar or vector of real values.
}
  \item{pis}{
  A vector collecting the mixture weights. See details.
}
  \item{means}{
  A vector collecting the means of the mixture components.
}
  \item{sds}{
  A vector collecting the standard deviations of the mixture components.
}
  \item{range}{
  Interval corresponding to the support of each of truncated normal contributing to the mixture. See details.}
}

\details{
The argument \code{pis} is a vector with its length equal the number of components in the mixture. The vector \code{pis} must sum up to one, e.g. \code{c(0.7, 0.2, 0.1)}.
The argument \code{range} is an interval corresponding to the support of each truncated normal contributing to the mixture. In other words, all the truncated normals contributing to the mixture are assumed to have the same range.
}
\value{
Value of the probability density function of the mixture of normals evaluated at \code{x}.
}

\author{
Sara Algeri and Xiangyu Zhang
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{rmixtruncnorm}}, \code{\link{dmixnegbinom}}, \code{\link{rmixnegbinom}}, \code{\link{find_h_cont}}}

\examples{
xx<-seq(0,30,length=10)
dmixtruncnorm(xx,pis=c(0.3,0.6,0.1),means=c(3,6,25),sds=c(3,4,10),range=c(0,30))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Mixture of truncated normals}% use one of  RShowDoc("KEYWORDS")
