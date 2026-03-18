\name{sn2ft2x2}
\alias{sn2ft2x2}
\title{
Conversion of 2 props input to 2x2 contingency table
}
\description{
  This function converts the successes and totals vectors required as
  input for function B2props to a 2x2 contingency table for input to
  CTA or Bft2x2.
}
\usage{
sn2ft2x2(s, n)
}
\arguments{
  \item{s}{
a vector of length 2 of successes
}
  \item{n}{
a vector of length 2 of numbers of trials
}
}
\value{
a 2 x 2 contingency table equivalent to the two arguments
}
\references{
van Hulst, R. 2018. Evaluating Scientific Evidence. ms.
}
\author{
Robert van Hulst
}
\examples{
sn2ft2x2(c(47, 59), c(120, 125))
}


