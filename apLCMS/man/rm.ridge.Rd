\name{rm.ridge}
\alias{rm.ridge}
\title{Removing long ridges at the same m/z.}
\description{ This is an internal function. It substracts a background estimated through kernel smoothing when an EIC continuously span more than half the retention time range. }
\usage{
rm.ridge(x, y2, bw)
}
\arguments{
  \item{x}{ Retetion time vector. }
  \item{y2}{ Intensity vector. }
  \item{bw}{ Bandwidth for the kernel smoother. A very wide one is used here. }
}
\value{ A vector of intensity value is returned. }
\references{
Bioinformatics. 25(15):1930-36. 
BMC Bioinformatics. 11:559.
}
\author{ Tianwei Yu <tyu8@emory.edu> }
\keyword{ models }