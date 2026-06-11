\name{rmixnegbinom}
\alias{rmixnegbinom}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Random numbers generator for negative binomial mixtures

}
\description{
Generates random samples from a mixture of negative binomials.
}
\usage{
rmixnegbinom(n, pis, size, probs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
  Size of the random sample.
}
  \item{pis}{
  A vector collecting the mixture weights. See details.
}
  \item{size}{
 A postive value corresponding to the target for number of successful trials. See details.
}
  \item{probs}{
  A vector collecting the probabilities of success for every mixture component.
}

}
\details{
The argument \code{pis} is a vector with  length equal the number of components in the mixture. The vector \code{pis} must sum up to one, e.g. \code{c(0.7, 0.2, 0.1)}.
All the negative binomials contributing to the mixture are assumed to have the same \code{size}.
}
\value{
A vector collecting the random sample of size \code{n} from the mixture of negative binomials specified.
}

\author{
Xiangyu Zhang and Sara Algeri
}

\seealso{
\code{\link{rmixtruncnorm}}, \code{\link{dmixtruncnorm}}, \code{\link{dmixnegbinom}}, \code{\link{find_h_disc}}.}

\examples{
rmixnegbinom(n=100,pis=c(0.3,0.6,0.1),size=2,probs=c(0.3,0.4,0.2))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Mixture of negative binomials sampler}% use one of  RShowDoc("KEYWORDS")
