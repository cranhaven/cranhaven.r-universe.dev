\name{sin1reg}
\alias{sin1reg}
\docType{data}
\title{sin1 regression dataset}
\description{
Simple synthetic dataset with 1000 points, where y=0.7*sin(pi*x1/2000)+0.3*sin(pi*x2/2000)
}
\usage{data(sin1reg)}
\format{
  The format is:
 chr "sin1reg"
}
\details{Simple synthetic dataset with 1000 points, where y=0.7*sin(pi*x1/2000)+0.3*sin(pi*x2/2000)}
\source{See references}
\references{
\itemize{
\item To cite the Importance function, sensitivity analysis methods or synthetic datasets, please use:\cr
P. Cortez and M.J. Embrechts.\cr
Using Sensitivity Analysis and Visualization Techniques to Open Black Box Data Mining Models.\cr
In Information Sciences, Elsevier, 225:1-17, March 2013.\cr
\doi{10.1016/j.ins.2012.10.039}\cr
}
}
\examples{
data(sin1reg)
print(summary(sin1reg))
}
\keyword{datasets}
