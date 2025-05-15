\name{servo}
\alias{servo}
\docType{data}
\title{
Servo Data Set
}
\description{
A small regression data set taken from UCI Machine Learning Repository. Response variable is "class".
}
\usage{data("servo")}
\format{
  The format is:
 chr "servo"
}

\source{
Bache, K. & Lichman, M. (2013). UCI Machine Learning Repository [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California, School of Information and Computer Science.
}

\examples{
data(servo)
lm(class~motor+screw+pgain+vgain, servo)
}
\keyword{datasets}
