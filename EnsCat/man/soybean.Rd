\name{soybean}
\alias{soybean}
\docType{data}
\title{Soybean (small) data
%%   ~~ data name/kind ... ~~
}
\description{
Classification data set from the UCI Machine Learning Repository
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("soybean")}
\format{
  The format of the data is a list with components $obs and
 $lab.
 "soybean$obs" includes the observations that are stored as numerical values.
"soybean$lab" contains the labels of the data.
}
\details{
A simple classification data set containing 35 attributes with 47
observations. Since the true labels are known, this data can be used to evaluate clustering methods.
}
\source{
Lichman, M. (2013). UCI Machine Learning Repository [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California, School of Information and Computer Science.

\url{https://archive.ics.uci.edu/ml/datasets/Soybean+(Small)}
}

\examples{
data(soybean)
}
\keyword{datasets}
