\name{binarize}
\alias{binarize}
\alias{binarizeByPercentage}
\alias{densityOnes}


\title{Binarize}
\description{ Methods to convert a real matrix to a binary matrix.}
\usage{
binarize(x, threshold=NA)
binarizeByPercentage(x,percentage, error=0.2, gap=0.1)
densityOnes(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The data matrix to be binarized.}
  \item{threshold}{Threshold used to binarize. Values over threshold will be set to 1,
    the rest to 0. If threshold is NA, median is used as threshold. Default NA.}
  \item{percentage}{Percentage of ones against zeros desired in the binary matrix.}
  \item{error}{Percentage of ones against zeros in the final matrix will be in [percentage-error, percentage+error]. Default 0.2}
  \item{gap}{Value used for incremental search of threshold. Default 0.1}
  }
\details{
The \code{binarize} function returns a matrix binarized by input threshold, or by the median if no threshold is given.

The \code{binarizeByPercentage} function returns a matrix binarize by input percentage, given as desired density of ones against zeros.

The \code{densityOnes} function returns the percentage of ones against zeros in a logical matrix
}
%\value{}
%\references{}

\author{
  Rodrigo Santamaria
  \email{rodri@usal.es}
  }

%\seealso{  }
\examples{
  data(BicatYeast)
  m1=binarize(BicatYeast)
  m2=binarize(BicatYeast, 0.2)
  m3=binarizeByPercentage(BicatYeast, 5)
  densityOnes(m3)
  densityOnes(m2)
  densityOnes(m1)
  drawHeatmap(BicatYeast)
  drawHeatmap(m1)
  drawHeatmap(m2)
  drawHeatmap(m3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
\keyword{array}
