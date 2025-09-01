\name{find.turn.point}
\alias{find.turn.point}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Find peaks and valleys of a curve. }
\description{
  This is an internal function which is not supposed to be directly accessed by the user. Finds the peaks and valleys of a smooth curve. 
}
\usage{
find.turn.point(y)
}
\arguments{
  \item{y}{ The y values of a curve in x-y plane. }
}
\value{
A list object:
  \item{pks}{The peak positions.}
  \item{vlys}{The valley positions}
}
\references{
Bioinformatics. 25(15):1930-36. 
BMC Bioinformatics. 11:559.
}
\author{ Tianwei Yu <tyu8@emory.edu> }
\keyword{ models }