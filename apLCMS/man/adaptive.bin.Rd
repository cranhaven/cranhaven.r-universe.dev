\name{adaptive.bin}
\alias{adaptive.bin}
\title{Adaptive binning}
\description{This is an internal function. It creates EICs using adaptive binning procedure}
\usage{
adaptive.bin(x, min.run, min.pres, tol, baseline.correct, weighted=FALSE)
}
\arguments{
  \item{x}{ A matrix with columns of m/z, retention time, intensity. }
  \item{min.pres}{ Run filter parameter. The minimum proportion of presence in the time period for a series of signals grouped by m/z to be considered a peak.}
  \item{min.run}{ Run filter parameter. The minimum length of elution time for a series of signals grouped by m/z to be considered a peak. }
  \item{tol}{ m/z tolerance level for the grouping of data points. This value is expressed as the fraction of the m/z value. This value, multiplied by the m/z value, becomes the cutoff level. The recommended value is the machine's nominal accuracy level. Divide the ppm value by 1e6. For FTMS, 1e-5 is recommended. }
  \item{baseline.correct}{After grouping the observations, the highest intensity in each group is found. If the highest is lower than this value, the entire group will be deleted. The default value is NA, in which case the program uses the 75th percentile of the height of the noise groups. }
  \item{weighted}{Whether to weight the local density by signal intensities.}
}
\details{It uses repeated smoothing and splitting to separate EICs. The details are described in the reference and flowchart.}
\value{
A list is returned.
  \item{height.rec}{The records of the height of each EIC.}
  \item{masses}{ The vector of m/z values after binning. }
  \item{labels}{ The vector of retention time after binning. }
  \item{intensi}{ The vector of intensity values after binning. }
  \item{grps}{ The EIC labels, i.e. which EIC each observed data point belongs to. }
  \item{times}{ All the unique retention time values, ordered. }
  \item{tol}{ The m/z tolerance level. }
  \item{min.count.run}{ The minimum number of elution time points for a series of signals grouped by m/z to be considered a peak.}
  \item{weighted}{Whether to weight the local density by signal intensities.}
}
\references{
Bioinformatics. 25(15):1930-36. 
BMC Bioinformatics. 11:559.
}
\author{ Tianwei Yu <tyu8@emory.edu> }
\keyword{ models }