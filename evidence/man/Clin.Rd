\name{Clin}
\alias{Clin}
\docType{data}
\title{ Made-up data to illustrate Simpson's paradox. }
\description{
  These made-up data illustrate the discrete form (contingency table
  form) of Simpson's paradox.
}
\usage{data(Clin)}
\format{
  A three-dimensional array of frequencies with:
  
  rows indicating "outcome" (either "death" or "cured"),
  
  columns indicating "male" (either "Yes" or "No"), and
  
  layers indicating "clinic" (either "A" or "B").
  }
\source{ van Hulst, R. 2018. Evaluating Scientific Evidence. ms.
}
\examples{
data(Clin)
Clin[1,,]
prop.table(Clin[1,,], 2)
}
\keyword{datasets}
