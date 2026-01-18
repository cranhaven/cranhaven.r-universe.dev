\name{utcomp}
\alias{utcomp}
\title{ p-value for the Utsu's test }
\description{
	This function calculates the \emph{p}-value for the Utsu's test from 2 values of b and 2 sample sizes.
}
\usage{
utcomp(b1, n1, b2, n2)
}
\arguments{
  \item{b1}{ \emph{b}-value from the first sample }
  \item{n1}{ size of the first sample (number of data above or equal to the threshold magnitude in the first data set) }
  \item{b2}{ \emph{b}-value from the second sample }
  \item{n2}{ size of the second sample (number of data above or equal to the threshold magnitude in the second data set) }
}
\details{
The formula is given in Utsu's (1992, 1999).
}
\value{
\code{utcomp} returns the \emph{p}-value of the Utsu's test for the comparison of 2 \emph{b}-values. 
}
\references{
Utsu, T. (1992) Introduction to seismicity, \emph{Surijishingaku (Mathematical Seismology)}, Inst. Statis. Math, 34, (VII), 139-157, (in Japanese).

Utsu, T. (1999) Representation and analysis of the earthquake size distribution: a historical review and some new approaches, \emph{Pure appl. Geophys.}, 155, 509-535
}
\author{ Daniel Amorese }
\seealso{\code{\link{BBootComp}}} 
\examples{
# Utsu's \emph{p}-value for the comparison of 2 \emph{b}-values in the Santa Paula area.
utcomp(0.97,366,0.77,1161)
}
\keyword{ htest }
