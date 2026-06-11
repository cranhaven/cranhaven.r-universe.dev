\name{dmixnegbinom}
\alias{dmixnegbinom}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Probability mass function of a mixture of negative binomials

}
\description{
Computes the probability mass function of a mixture of the negative binomials.
}
\usage{
dmixnegbinom(x, pis, size, probs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  A scalar or vector of non-negative integer values.
}
  \item{pis}{
  A vector collecting the mixture weights. See details.
}
  \item{size}{
  A postive value corresponding to the target for number of successful trials.
}
  \item{probs}{
  A vector collecting the probabilities of success for each mixture component.
}

}
\details{
The argument \code{pis} is a vector with length equal the number of components in the mixture. The vector \code{pis} must sum up to one, e.g. \code{c(0.7, 0.2, 0.1)}.
All the negative binomials contributing to the mixture are assumed to have the same \code{size}.
}
\value{
Value of the probability mass function of the mixture of negative binomials evaluated at \code{x}.
}

\author{
Xiangyu Zhang and Sara Algeri
}

\seealso{
\code{\link{rmixtruncnorm}}, \code{\link{dmixtruncnorm}}, \code{\link{rmixnegbinom}}, \code{\link{find_h_disc}}.}

\examples{

xx<-seq(0,30,length=31)
dmixnegbinom(xx,pis=c(0.4,0.6),size=25,probs=c(0.6,0.7))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the R documentation directory.
\keyword{Mixture of negative binomials}% use one of RShowDoc("KEYWORDS")
