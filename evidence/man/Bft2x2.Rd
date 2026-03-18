\name{Bft2x2}
\alias{Bft2x2}
\title{
Bayesian analysis of a 2 x 2 contingency table.
}
\description{
A 2 x 2 contingency table (in matrix form) is analyzed in a Bayesian
way using uniform priors. The posterior probabilities of each of the the
two outcomes given the other factor levels  are calculated.
See MacKay(2003, p. 460).
}
\usage{
Bft2x2(X, div = 100, plotit = TRUE)
}
\arguments{
  \item{X}{
a contingency table in the form of a 2 x 2 matrix with row and column names
}
  \item{div}{
optional: the number of divisions for the row and column variables for
use in calculations (can be left at 100)
}
  \item{plotit}{
should plots be produced? (defaults to TRUE)
}
}
\details{
  Note that the rows of the 2 x 2 matrix are assumed to represent the
  "outcomes" and the columns the "treatments"---where these expressions
  are applicable. Note also that to obtain properly labeled plots the
  matrix has to be supplied with dimnames.
}
\value{
the matrix of \code{div} x \code{div} posterior probabilities that was plotted
}
\references{
  MacKay, D.J.C. 2003. Information Theory, Inference, and Learning
  Algorithms. Cambridge University Press, Cambridge.
  
  van Hulst, R. 2018. Evaluating Scientific Evidence. ms.
}
\author{
Robert van Hulst
}

\seealso{
\code{\link{fisher.test}}
}
\examples{
data(Glasses)
Bft2x2(Glasses)
}

\keyword{ models }

\keyword{ htest }
