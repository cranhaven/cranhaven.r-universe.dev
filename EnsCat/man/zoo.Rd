\name{zoo}
\alias{zoo}
\docType{data}
\title{zoo data
%%   ~~ data name/kind ... ~~
}
\description{
Classification data set from the UCI Machine Learning Repository
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("zoo")}
\format{
  The format of the data is a list with components $obs and
 $lab.
 "zoo$obs" includes the observations that are stored as numerical values.
"zoo$lab" contains the labels of the data.
}
\details{
A simple classification data set containing 17 Boolean-valued attributes with 101
observations. Since the true labels are known, this data can be used to evaluate clustering methods.
}
\source{
Lichman, M. (2013). UCI Machine Learning Repository [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California, School of Information and Computer Science.

\url{https://archive.ics.uci.edu/ml/datasets/Zoo}
}

\examples{
data(zoo)
}
\keyword{datasets}
