\name{find.match}
\alias{find.match}
\title{ Internal function: finding the best match between a set of detected features and a set of known features.  }
\description{ Given a small matrix of distances, find the best column-row pairing that minimize the sum of distances of the matched pairs. }
\usage{
find.match(a, unacceptable = 4)
}
\arguments{
  \item{a}{ A matrix of distances. }
  \item{unacceptable}{ A distance larger than which cannot be accepted as pairs. }
}
\value{ A matrix the same dimension as the input matrix, with matched position taking value 1, and all other positions taking value 0. }
\author{ Tianwei Yu <tyu8@emory.edu> }
\keyword{ models }