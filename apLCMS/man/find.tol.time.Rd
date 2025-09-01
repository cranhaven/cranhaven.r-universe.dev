\encoding{latin1}
\name{find.tol.time}
\alias{find.tol.time}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ An internal function that is not supposed to be directly accessed by the user. Find elution time tolerance level.  }
\description{
  This function finds the time tolerance level. Also, it returns the grouping information given the time tolerance.
}
\usage{
find.tol.time(mz, chr, lab, num.exp, mz.tol = 2e-05, chr.tol = NA,
                 aver.bin.size = 200, min.bins = 50, max.bins = 100,
                 max.mz.diff = 0.01, max.num.segments = 10000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mz}{ mz value of all peaks in all profiles in the study. }
  \item{chr}{ retention time of all peaks in all profiles in the study.}
  \item{lab}{ label of all peaks in all profiles in the study.}
  \item{num.exp}{ The number of spectra in this analysis. }
  \item{mz.tol}{ m/z tolerance level for the grouping of signals into peaks. This value is expressed as the percentage of the m/z value. This value, multiplied by the m/z value, becomes the cutoff level. }
  \item{chr.tol}{ the elution time tolerance. If NA, the function finds the tolerance level first. If a numerical value is given, the function directly goes to the second step - grouping peaks based on the tolerance. }
  \item{aver.bin.size}{The average bin size to determine the number of equally spaced points in the kernel density estimation.}
  \item{min.bins}{the minimum number of bins to use in the kernel density estimation. It overrides aver.bin.size when too few observations are present.}
  \item{max.mz.diff}{As the m/z tolerance in alignment is expressed in relative terms (ppm), it may not be suitable when the m/z range is wide. This parameter limits the tolerance in absolute terms. It mostly influences feature matching in higher m/z range.}
  \item{max.bins}{the maximum number of bins to use in the kernel density estimation. It overrides aver.bin.size when too many observations are present.}
  \item{max.num.segments}{ the maximum number of segments. }
}
\details{
  The peaks are first ordered by m/z, and split into groups by the m/z tolerance. Then within every peak group, the pairwise elution time difference is calculated. All the pairwise elution time differences within groups are merged into a single vector. A mixture model (unknown distribution for distance between peaks from the same feature, and a triangle-shaped distribution for distance between peaks from different features) is fit to find the elution time tolerance level. The elution times within each peak group are then ordered. If a gap between consecutive retention times is larger than the elution time tolerance level, the group is further split at the gap. Grouping information is returned, as well as the elution time tolerance level. 
}
\value{
  A list object is returned:
  \item{chr.tol}{ The elution time tolerance level.}
  \item{comp2 }{A matrix with six columns. Every row corrsponds to a peak in one of the spectrum. The columns are: m/z, elution time, spread, signal strength, spectrum label, and peak group label. The rows are ordered by the median m/z of each peak group, and with each peak group the rows are ordered by the elution time.}
}
\author{Tianwei Yu <tyu8@emory.edu>}
\keyword{ models }
