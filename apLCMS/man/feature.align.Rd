\name{feature.align}
\alias{feature.align}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Align peaks from spectra into a feature table. }
\description{
  Identifies which of the peaks from the profiles correspond to the same feature.
}
\usage{
feature.align(features, min.exp = 2, mz.tol = NA, chr.tol = NA,find.tol.max.d=1e-4,
max.align.mz.diff=0.01)
}
\arguments{
  \item{features}{ A list object. Each component is a matrix which is the output from proc.to.feature(). }
  \item{min.exp}{ A feature has to show up in at least this number of profiles to be included in the final result. }
  \item{mz.tol}{ The m/z tolerance level for peak alignment. The default is NA, which allows the program to search for the tolerance level based on the data. This value is expressed as the percentage of the m/z value. This value, multiplied by the m/z value, becomes the cutoff level.}
  \item{chr.tol}{ The retention time tolerance level for peak alignment. The default is NA, which allows the program to search for the tolerance level based on the data. }
  \item{find.tol.max.d}{Argument passed to find.tol(). Consider only m/z diffs smaller than this value.This is only used when the mz.tol is NA. }
  \item{max.align.mz.diff}{As the m/z tolerance is expressed in relative terms (ppm), it may not be suitable when the m/z range is wide. This parameter limits the tolerance in absolute terms. It mostly influences feature matching in higher m/z range.}
}
\details{
  The function first searches for the m/z tolerance level using a mixture model. After the mz.tol is obtained, the peaks are grouped based on it. Consecutive peaks with m/z value difference smaller than the tolerance level are considered to belong to the same peak group. Non-parametric density estimation within each peak group is used to further split peak groups.
  The function then searches for the retention time tolerance level. Because the peaks are grouped using m/z, only metabolites that share m/z require this parameter. A rather lenient retention time tolerance level is found using a mixture model. After splitting the peak groups by this value, non-parametric density estimation is used to further split peak groups. Peaks belonging to one group are considered to correspond to the same feature.
}
\value{
  Returns a list object with the following objects in it:
  \item{aligned.ftrs}{A matrix, with columns of m/z values, elution times, signal strengths in each spectrum.}
  \item{pk.times}{A matrix, with columns of m/z, median elution time, and elution times in each spectrum.}
  \item{mz.tol}{The m/z tolerance used in the alignment.}
  \item{chr.tol}{The elution time tolerance in the alignment.}
}
\author{ Tianwei Yu <tyu8@emory.edu> }
\seealso{ proc.to.feature }
\examples{
\donttest{
data(features)
features.2<-adjust.time(features)
this.aligned<-feature.align(features,min.exp=2)
summary(this.aligned)
this.aligned$aligned.ftrs[1:5,]
this.aligned$pk.times[1:5,]
}
}
\keyword{ models }