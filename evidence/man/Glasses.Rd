\name{Glasses}
\alias{Glasses}
\docType{data}
\title{ A contingency table of 16 British youths categorized as juvenile
delinquents or not, and as wearing glasses or not. }
\description{
  Data from Heidelberger and Holland(2004) categorizing a random sample of
  16 British juveniles on the basis of whether they were juvenile
  delinquents or not, and whether wore glasses or not.
}
\usage{data(Glasses)}
\format{
  A matrix with 16 counts cross-classified on Juvenile delinquency (rows)
  and the wearing of glasses (columns).
  }
\source{
  Heiberger, R.M. and Holland, B.(2004) Statistical Analysis and Data
  Display: An Intermediate Course with Examples in S-PLUS, R, and
  SAS. Springer, New York.
}
\references{
  van Hulst, R. 2018. Evaluating Scientific Evidence. ms.
}
\examples{
data(Glasses)
Bft2x2(Glasses)
}
\keyword{datasets}
