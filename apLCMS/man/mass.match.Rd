\name{mass.match}
\alias{mass.match}
\title{
An internal function: finding matches between two vectors of m/z values.
}
\description{
Given two vectors of m/z values and the tolerance ppm level, find the potential matches between the two vectors. 
}
\usage{
mass.match(x, known.mz, match.tol.ppm = 5)
}
\arguments{
  \item{x}{
m/z values from the data.
}
  \item{known.mz}{
m/z values from the known feature table. 
}
  \item{match.tol.ppm}{
tolerance level in ppm.
}
}
\value{
A vector the same length as x. 1 indicates matched, and 0 indicates unmatched. 
}
\author{ Tianwei Yu <tyu8@emory.edu> }
\keyword{ models }