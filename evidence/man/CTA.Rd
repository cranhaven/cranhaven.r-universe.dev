\name{CTA}
\alias{CTA}
\title{
Contingency Table Analysis in different ways
}
\description{
An n x n contingency table is analyzed in frequentist,
information-theoretical, likelihood, and Bayesian ways.
Note that for the Bayesian analysis package LearnBayes
needs to be installed.
}
\usage{
CTA(X, extBayes = FALSE)
}
\arguments{
  \item{X}{
    a matrix with non-negative integers representing the counts for
    the row-column levels
  }
  \item{extBayes}{
    should a Bayesian analysis with a near-independence prior (instead
    of only an independence prior) be done as well? Defaults to FALSE.
    }
}
\value{
none provided: the function is run for its graphical and numerical output
}
\references{
van Hulst, R. 2018. Evaluating Scientific Evidence. ms.
}
\author{
Robert van Hulst
}
\seealso{
\code{\link{Bft2x2}}
}
\examples{
data(Smoking)
CTA(Smoking)
}

\keyword{ htest }
\keyword{ models }
