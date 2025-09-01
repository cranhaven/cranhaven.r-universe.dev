\encoding{latin1}
\name{adjust.time}
\alias{adjust.time}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Adjust retention time across spectra. }
\description{
  This function adjusts the retention time in each LC/MS profile to achieve better between-profile agreement. 
}
\usage{
adjust.time(features, mz.tol = NA, chr.tol = NA, colors=NA, find.tol.max.d=1e-4,
max.align.mz.diff=0.01, transform.mz=FALSE, transform.mz.const=0.1)
}
\arguments{
  \item{features}{ A list object. Each component is a matrix which is the output from proc.to.feature(). }
  \item{mz.tol}{ The m/z tolerance level for peak alignment. The default is NA, which allows the program to search for the tolerance level based on the data. This value is expressed as the percentage of the m/z value. This value, multiplied by the m/z value, becomes the cutoff level.}
  \item{chr.tol}{ The retention time tolerance level for peak alignment. The default is NA, which allows the program to search for the tolerance level based on the data. }
  \item{colors}{ The vector of colors to be used for the line plots of time adjustments. The default is NA, in which case the program uses a set of default color set. }
  \item{find.tol.max.d}{Argument passed to find.tol(). Consider only m/z diffs smaller than this value. This is only used when the mz.tol is NA. }
  \item{max.align.mz.diff}{As the m/z tolerance is expressed in relative terms (ppm), it may not be suitable when the m/z range is wide. This parameter limits the tolerance in absolute terms. It mostly influences feature matching in higher m/z range.}
  \item{transform.mz}{Whether to apply a nonlinear transformation to m/z values before alignment.}
  \item{transform.mz.const}{A constant used in the m/z transformation function}
}
\details{
  The function first searches for the m/z tolerance level using a mixture model. After the mz.tol is obtained, the peaks are grouped based on it. The function then searches for the retention time tolerance level. Because the peaks are grouped using m/z, only metabolites that share m/z require this parameter. A rather lenient retention time tolerance level is found using a mixture model. 
  
  The profile with the highest number of peaks is selected as the template and every other spetrum is adjusted to it one at a time. At every m/z value, if each of the two spetra has just one peak, and the peaks are within the retention time tolerance range, the pair of retention time values are used in the curve fitting. A kernel smoother is fitted using the difference in retention time against the retention time in the profile to be adjusted. 
}
\value{
A list object with the exact same structure as the input object features, i.e. one matrix per profile being processed. The only difference this output object has with the input object is that the retention time column in each of the matrices is changed to new adjusted values.
}
\author{ Tianwei Yu <tyu8@emory.edu> }
\seealso{ feature.align }
\examples{
data(features)
adjusted<-adjust.time(features, colors=c("red","blue","green","cyan"))
}
\keyword{ models }
