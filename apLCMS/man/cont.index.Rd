\name{cont.index}
\alias{cont.index}
\title{ Continuity index }
\description{ This is an internal function. It uses continuity index (or "run filter") to select putative peaks from EIC. }
\usage{
cont.index(newprof, min.pres = 0.6, min.run = 5)
}
\arguments{
  \item{newprof}{ The matrix containing m/z, retention time, intensity, and EIC label as columns. }
  \item{min.pres}{ Run filter parameter. The minimum proportion of presence in the time period for a series of signals grouped by m/z to be considered a peak.}
  \item{min.run}{ Run filter parameter. The minimum length of elution time for a series of signals grouped by m/z to be considered a peak. }
}
\details{ This is the run filter described in Yu et al Bioinformatics 2009. }
\value{
A list is returned.
  \item{new.rec}{ The matrix containing m/z, retention time, intensity, and EIC label as columns after applying the run filter. }
  \item{height.rec}{ The vector of peak heights. }
  \item{time.range.rec}{ The vector of peak retention time span. }
  \item{mz/pres.rec}{ The vector of proportion of non-missing m/z. }
}
\references{
Bioinformatics. 25(15):1930-36. 
BMC Bioinformatics. 11:559.
}
\author{ Tianwei Yu <tyu8@emory.edu> }
\keyword{ models }