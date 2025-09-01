\name{adaptive.bin.2}
\alias{adaptive.bin.2}
\title{Adaptive binning specifically for the machine learning approach.}
\description{This is an internal function. It creates EICs using adaptive binning procedure}
\usage{
adaptive.bin.2(x, tol, ridge.smoother.window=50, baseline.correct)
}
\arguments{
  \item{x}{ A matrix with columns of m/z, retention time, intensity. }
  \item{tol}{ m/z tolerance level for the grouping of data points. This value is expressed as the fraction of the m/z value. This value, multiplied by the m/z value, becomes the cutoff level. The recommended value is the machine's nominal accuracy level. Divide the ppm value by 1e6. For FTMS, 1e-5 is recommended. }
  \item{ridge.smoother.window}{The size of the smoother window used by the kernel smoother to remove long ridge noise from the EIC. }
  \item{baseline.correct}{After grouping the observations, the highest intensity in each group is found. If the highest is lower than this value, the entire group will be deleted. The default value is NA, in which case the program uses the 75th percentile of the height of the noise groups. }
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
}
\references{
Bioinformatics. 30(20):  2941-2948.
Bioinformatics. 25(15):1930-36. 
BMC Bioinformatics. 11:559.
}
\author{ Tianwei Yu <tyu8@emory.edu> }
\keyword{ models }